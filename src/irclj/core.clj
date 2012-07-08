;; Irclj is an IRC library for Clojure. On one hand, the goal is to make it as flexible
;; as possible and allow for any number of use cases. On the other hand, I want it to
;; be super-easy to write IRC bots with Irclj.
;;
;; Irclj takes the approach of stuffing all the information about an IRC connection
;; in a single ref that the user will hold and pass around.
;;
;; Irclj is also entirely asynchronous. Everything is based on events. You can register
;; callbacks for things that happen on IRC, such as a PRIVMSG or a NOTICE. Things that
;; you might expect to return info doesn't actually return anything. Most of Irclj's
;; functions return immediately, since all they do is sent a message to IRC. The info
;; is collected later when IRC sends the requested info to the client. It might not be
;; entirely clear how to get certain information, but you can almost always collect
;; the info you want just by registering a callback that handles the data.
(ns irclj.core
  "An IRC library for Clojure."
  (:require [clojure.string :as string]
            [irclj.parser :as parser]
            [irclj.process :as process]
            [irclj.events :as events]
            [irclj.connection :as connection]))

;; ## IRC Commands

(def ^{:private true
       :doc "clojure.string/join partially applied to \",\"."}
  comma-join
  (partial string/join ","))

;; The IRC spec requires that servers allow for a join command to
;; join several channels at once. We're doing some fun stuff to make
;; sure that the keyed channels come first. They have to come first
;; because if you were to try something like so:
;; `JOIN keyedchan,nonkeyed,anotherkeyedchan key,key2`
;; then IRC would think that key2 is for nonkeyed and not for
;; anotherkeyedchan.
(defn join
  "Joins channels. A channel is either a string or a vector of string and key.
   Blocks until :ready? is delivered."
  [irc & channels]
  (let [[keyed regular] ((juxt filter remove) vector? channels)
        chans (concat (map first keyed) regular)
        keys (map last keyed)]
    (when @(:ready? @irc)
      (connection/write-irc-line irc "JOIN" (comma-join chans) (comma-join keys)))))

(defn part
  "Part from channels. A channel is either a string or a vector of string and key.
   If a :message key is passed, then that message is used as the parting message.
   If this key is passed, it **must** be the last thing passed to this function."
  [irc & channels-and-opts]
  (let [[channels opts] (split-with (complement keyword?) channels-and-opts)
        opts (apply hash-map opts)]
    (connection/write-irc-line irc "PART"
                               (string/join "," channels)
                               (when-let [message (:message opts)]
                                 (connection/end message)))))

(defn message
  "Sends a PRIVMSG to a user or channel."
  [irc target & s]
  (connection/write-irc-line irc "PRIVMSG" target
                             (connection/end (string/join " " s))))

(defn reply
  "Reply to a PRIVMSG. Determines user or channel based on original message."
  [irc m & s]
  (apply message irc (:target m) s))

(defn identify
  "Identify with NICKSERV. Will block until the connection is registered."
  [irc password]
  (when @(:ready? @irc)
    (message irc "NickServ" "IDENTIFY" password)))

(defn set-nick
  "Change your nickname on IRC."
  [irc nick]
  (connection/write-irc-line irc "NICK" nick))

(defn mode
  "Request or set the mode for a channel."
  [irc channel & [modes]]
  (connection/write-irc-line irc "MODE" channel modes))

(defn kick
  "Kick a user from a channel."
  [irc channel user & [message]]
  (connection/write-irc-line irc "KICK" channel user
                             (when message (connection/end message))))

;; We fire our raw-log callback for the lines we read from IRC as well.
(defn- process
  "Prepare and process a line from IRC."
  [irc line]
  (events/fire irc :raw-log :read line)
  (process/process-line (parser/parse line) irc))

(defn connect
  "Connect to IRC. Connects in another thread and returns a big fat ref of
   data about the connection, you, and IRC in general."
  [host port nick &
   {:keys [timeout real-name mode username callbacks]
    :or {real-name "irclj", mode 0
         callbacks {:raw-log events/stdout-callback}}
    :as all}]
  (let [{:keys [in] :as connection} (connection/create-connection host port)
        irc (ref {:connection connection
                  :shutdown? false
                  :nick nick
                  :real-name real-name
                  :username username
                  :callbacks callbacks
                  :init-mode mode
                  :network host
                  :ready? (promise)})]
    (.start
     (Thread.
      (fn []
        (connection/set-timeout irc timeout)
        (connection/register-connection irc)
        (loop [lines (connection/safe-line-seq in)]
          (if-let [line (first lines)]
            (do (process irc line)
                (recur (rest lines)))
            (events/fire irc :on-shutdown))))))
    irc))

(defn kill
  "Close the socket associated with an IRC connection."
  [irc]
  (.close (get-in @irc [:connection :socket])))