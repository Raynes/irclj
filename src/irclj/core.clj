;; Irclj is an IRC library for Clojure. On one hand, the goal is to make it as flexible
;; as possible and allow for any number of use cases. On the other hand, I want it to
;; be super-easy to write IRC bots with Irclj.
;;
;; Irclj takes the approach of stuffing all the information about an IRC connection
;; in a single ref that the user will hold and pass around. It also has a callback
;; system. You can register callbacks for things that happen on IRC, such as a PRIVMSG
;; or a notice.
(ns irclj.core
  "An IRC library for Clojure."
  (:require [clojure.string :as string]
            [irclj.parser :as parser]
            [irclj.process :as process]
            [irclj.events :as events]
            [irclj.connection :as connection]))

;; ## IRC Commands

;; The IRC spec requires that servers allow for a join command to
;; join several channels at once. We're doing some fun stuff to make
;; sure that the keyed channels come first. They have to come first
;; because if you were to try something like so:
;; `JOIN keyedchan,nonkeyed,anotherkeyedchan key,key2`
;; then IRC would think that key2 is for nonkeyed and not for
;; anotherkeyedchan.
(defn join-channels
  "Joins channels. A channel is either a string or a vector of string and key.
   Blocks until :ready? is delivered."
  [irc & channels]
  (let [[keyed regular] ((juxt filter remove) vector? channels)
        chans (concat (map first keyed) regular)
        keys (map last keyed)]
    (when @(:ready? @irc)
      (connection/write-irc-line irc
                                 "JOIN"
                                 (string/join "," chans)
                                 (string/join "," keys)))))

(defn part-channels
  "Part from channels. A channel is either a string or a vector of string and key.
   If message is nil, no part message is used."
  [irc message & channels]
  (connection/write-irc-line irc "PART"
                             (string/join "," channels)
                             (connection/end message)))

(defn send-message
  "Sends a PRIVMSG to a user or channel."
  [irc target & s]
  (connection/write-irc-line irc "PRIVMSG" target
                             (connection/end (string/join " " s))))

(defn identify
  "Identify with NICKSERV. Will block until the connection is registered."
  [irc password]
  (when @(:ready? @irc)
    (send-message irc "NickServ" "IDENTIFY" password)))

(defn set-nick
  "Change your nickname on IRC."
  [irc nick]
  (connection/write-irc-line irc "NICK" nick))

(defn mode
  "Get or set the mode for a channel."
  [irc channel & [modes]]
  (connection/write-irc-line irc "MODE" channel modes))

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