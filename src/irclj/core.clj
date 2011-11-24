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
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [slingshot.slingshot :as stone]
            [irclj.parser :as parser])
  (:import (java.net Socket SocketTimeoutException)
           java.io.IOException))

;; ## IRC Message Processing

;; We're using an event-based system for communicating with users. This
;; fires event callbacks.
(defn fire
  "Fire a callback of type type if it exists, passing irc and args."
  [irc type & args]
  (when-let [callback (get-in @irc [:callbacks type])]
    (apply callback irc args)))

;; We want to allow users to log raw input and output in whatever way
;; they please. To that, we'll fire the :raw-log callback function when
;; we read and write.
(defn write-irc-line
  "Writes a line to the IRC connection and fires the raw-log callback.
   Can take arbitrary arguments, joining them with spaces (like println)."
  [irc & s]
  (let [s (string/join " " s)]
    (fire irc :raw-log :write s)
    (binding [*out* (get-in @irc [:connection :out])]
      (println s))))

;; You usually want to prefix your last parameter with a : so that IRC knows
;; to allow it to contain spaces and parse it all as one param. This is for
;; that. `write-irc-line` *could* do this itself, but I think this is sufficient.
;; If `write-irc-line` did it internally, it would mean doing some very expensive
;; operations. While in our simple case it would hardly matter, this is really
;; just as easy and more flexible. Tacking `(end ..)` on your last param if
;; necessary isn't much of a chore.
(defn end
  "If param is nil, return nil. Otherwise, return param with : prefixed."
  [param]
  (when (seq param)
    (str \: param)))

;; We're going handle IRC messages polymorphically. Whatever IRC commands we
;; support are implemented as process-line implementations. process-line takes
;; the result of irclj.parser/parse.
(defmulti process-line
  "Process a parsed IRC message."
  (fn [m _] (:command m)))

;; ### Numeric

(defn- parse-prefix
  "Parses the PREFIX section of an ISUPPORT message. Returns a map of symbols
   to their corresponding modes."
  [{:keys [raw]}]
  (when-let [[modes prefixes] (next (re-find #"PREFIX=\((.*?)\)(\S+)" raw))]
    (zipmap prefixes modes)))

;; We want to parse this line to find out which modes a user can have (operator,
;; voice, etc).
(defmethod process-line "005" [m irc]
  (when-let [prefixes (parse-prefix m)]
    (dosync (alter irc assoc :prefixes prefixes))))

;; IRC sends 332 to tell you what the channel topic is (if present).
(defmethod process-line "332" [{:keys [params]} irc]
  (dosync (alter irc assoc-in [:channels (first params) :topic] (last params))))

(defn- nick-parser
  "Returns a function that parses a nick, returning a map where the nick
   is the key and the value is another map containing a :mode key which is
   either the user's mode (determined by the first character of the nick) if
   it is present in prefixes or nil if not."
  [prefixes]
  (fn [nick]
    (let [prefix (-> nick first prefixes)]
      [(if prefix (subs nick 1) nick) {:mode prefix}])))

(def ^{:doc "A map of indicators from 353 to their meanings."}
  indicators
  {"@" :secret
   "*" :private
   "=" :public})

;; 353 gives you the list of users that are in a channel. We want this.
(defmethod process-line "353" [{:keys [params]} irc]
  (let [[_ indicator channel names] params
        names (into {}
                    (map (nick-parser (:prefixes @irc))
                         (string/split names #" ")))]
    
    (dosync
     (alter irc update-in [:channels channel]
            (fn [old]
              (-> old
                  (assoc :indicator (doto (indicators indicator) prn))
                  (update-in [:users] #(into names %))))))))

;; At this point, the IRC server has registered our connection. We can communicate
;; this by delivering our ready? promise.
(defmethod process-line "001" [m irc]
  (deliver (:ready? @irc) true)
  (fire irc :001 m))

;; We can't really recover from a nick-already-in-use error. Just throw an
;; exception.
(defmethod process-line "433" [m irc]
  (fire irc :433 m)
  (stone/throw+ (into @irc {:parsed-message m})
                "Nick is already taken. Can't recover."))

;; ### Wordy Responses

;; PONG!
(defmethod process-line "PING" [m irc]
  (write-irc-line irc (.replace (:raw m) "PING" "PONG")))

;; We don't want to die if something that we don't support happens. We can just
;; ignore it instead.
(defmethod process-line :default [& _] nil)

;; ## IRC Commands

;; The IRC spec requires that servers allow for a join command to
;; join several channels at once. We're doing some fun stuff to make
;; sure that the keyed channels come first. They have to come first
;; because if you were to try something like so:
;; `JOIN keyedchan,nonkeyed,anotherkeyedchan key,key2`
;; then IRC would thing that key2 is for nonkeyed and not for
;; anotherkeyedchan.
(defn join-channels
  "Joins channels. A channel is either a string or a vector of string and key.
   "
  [irc & channels]
  (let [[keyed regular] ((juxt filter remove) vector? channels)
        chans (concat (map first keyed) regular)
        keys (map last keyed)]
    (when @(:ready? @irc)
      (write-irc-line irc "JOIN" (string/join "," chans) (string/join "," keys)))))

(defn part-channels
  "Part from channels. A channel is either a string or a vector of string and key.
   If message is nil, no part message is used."
  [irc message & channels]
  (write-irc-line irc "PART" (string/join "," channels) (end message)))

(defn send-message
  "Sends a PRIVMSG to a user or channel."
  [irc target & s]
  (write-irc-line irc "PRIVMSG" target (end (string/join " " s))))

(defn identify
  "Identify with NICKSERV."
  [irc password]
  (send-message irc "NickServ" "IDENTIFY" password))

(defn set-nick
  "Change your nickname on IRC."
  [irc nick]
  (write-irc-line irc "NICK" nick))

;; ## Connections

(defn create-connection
  "Creates a socket from a host and a port. Returns a map
   of the socket and readers over its input and output."
  [host port]
  (let [socket (Socket. host port)]
    {:socket socket
     :in (io/reader socket)
     :out (io/writer socket)}))

;; IRC requires that you do this little dance to register your connection
;; with the IRC network.
(defn- register-connection
  "writes NICK and USER messages to IRC, registering the connection."
  [irc]
  (let [{:keys [nick username real-name init-mode]} @irc]
    (write-irc-line irc "NICK" nick)
    (write-irc-line irc "USER" (or username nick) init-mode "*" (end real-name))))

;; If something happens with the connection that we don't otherwise notice,
;; we want it to be able to timeout appropriately so that we can move along.
;; This will set a timeout in milliseconds that will throw a SocketTimeoutException
;; if no data is received during that time.
(defn- set-timeout
  "Set a timeout on the socket. timeout is in milliseconds."
  [irc timeout]
  (when timeout
    (.setSoTimeout (:socket (:connection @irc)) timeout)))

;; `BufferedReader`, the reader we use, promises that reading from it if it is empty
;; (if it is dead/closed/etc) will return nil. Unfortunately, the `InputStream` from
;; Socket throws an `IOException` instead. Because of this, we can't use the `line-seq`
;; from core and still handle dead connections gracefully. This is the same as
;; `clojure.core/line-seq` but catches the IOException and returns nil.
(defn- safe-line-seq
  "Get an infinite lazy sequence of lines from a reader."
  [rdr]
  (try
    (cons (.readLine rdr) (lazy-seq (safe-line-seq rdr)))
    (catch IOException _ nil)))

;; We fire our raw-log callback for the lines we read from IRC as well.
(defn- process
  "Prepare and process a line from IRC."
  [irc line]
  (fire irc :raw-log :read line)
  (process-line (parser/parse line) irc))

;; This is the default raw-log callback function. It logs input and output to
;; stdout, which is the most common use case.
(defn stdout-callback
  "A raw-log callback that prints to stdout."
  [_ type s]
  (println
   (case type
     :write (str ">> " s)
     :read s)))

(defn connect
  "Connect to IRC. Connects in another thread and returns a big fat ref of
   data about the connection, you and IRC in general."
  [host port nick &
   {:keys [timeout real-name mode username
           callbacks]
    :or {real-name "irclj", mode 0
         callbacks {:raw-log stdout-callback}}
    :as all}]
  (let [{:keys [in] :as connection} (create-connection host port)
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
        (set-timeout irc timeout)
        (register-connection irc)
        (loop [lines (safe-line-seq in)]
          (if-let [line (first lines)]
            (do (process irc line)
                (recur (rest lines)))
            (fire irc :on-shutdown))))))
    irc))