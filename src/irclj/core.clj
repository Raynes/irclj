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

(defn create-connection
  "Creates a socket from a host and a port. Returns a map
   of the socket and readers over its input and output."
  [host port]
  (let [socket (Socket. host port)]
    {:socket socket
     :in (io/reader socket)
     :out (io/writer socket)}))

(defn fire
  "Fire a callback of type type if it exists, passing irc and args."
  [irc type & args]
  (when-let [callback (get-in @irc [:callbacks type])]
    (apply callback irc args)))

;; We want to allow users to log raw input and output in whatever way
;; they pleae. To that, we'll fire the :raw-log callback function when
;; we read and write.
(defn write-irc-line
  "Writes a line to the IRC connection and fires the raw-log callback.
   Can take arbitrary arguments, joining them with spaces (like println)."
  [irc & s]
  (let [s (string/join " " s)]
    (fire irc :raw-log :write s)
    (binding [*out* (get-in @irc [:connection :out])]
      (println s))))

;; This is the default raw-log callback function. It logs input and output to
;; stdout, which is the most common use case.
(defn stdout-callback
  "A raw-log callback that prints to stdout."
  [_ type s]
  (println
   (case type
     :write (str ">> " s)
     :read s)))

;; We're going handle IRC messages polymorphically. Whatever IRC commands we
;; support are implemented as process-line implementations. process-line takes
;; the result of irclj.parser/parse.
(defmulti process-line
  "Process a parsed IRC message."
  (fn [m _] (:command m)))

;; We can't really recover from a nick-already-in-use error. Just throw an
;; exception.
(defmethod process-line "433" [m irc]
  (stone/throw+ (into @irc {:parsed-message m})
                "Nick is already taken. Can't recover."))

;; PONG!
(defmethod process-line "PING" [m irc]
  (write-irc-line irc (.replace (:raw m) "PING" "PONG")))

;; We don't want to die if something that we don't support happens. We can just
;; ignore it instead.
(defmethod process-line :default [& _] nil)

;; IRC requires that you do this little dance to register your connection
;; with the IRC network.
(defn- register-connection
  "writes NICK and USER messages to IRC, registering the connection."
  [irc]
  (let [{:keys [nick username real-name init-mode]} @irc]
    (write-irc-line irc "NICK" nick)
    (write-irc-line irc "USER" (or username nick) init-mode "*" (str ":" real-name))))

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
                  :init-mode mode})]
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