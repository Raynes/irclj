(ns irclj.connection
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [irclj.events :as events])
  (:import java.net.Socket
           javax.net.ssl.SSLSocketFactory
           java.io.IOException))

;; We want to allow users to log raw input and output in whatever way
;; they please. To that, we'll fire the :raw-log callback function when
;; we read and write.
(defn write-irc-line
  "Writes a line to the IRC connection and fires the raw-log callback.
   Can take arbitrary arguments, joining them with spaces (like println)."
  [irc & s]
  (let [s (string/join " " s)]
    (events/fire irc :raw-log :write s)
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

(defn create-connection
  "Creates a socket from a host and a port. Returns a map
   of the socket and readers over its input and output."
  [host port ssl?]
  (let [socket (if ssl?
                 (.createSocket (SSLSocketFactory/getDefault) host port)
                 (Socket. host port))]
    {:socket socket
     :in (io/reader socket)
     :out (io/writer socket)}))

;; IRC requires that you do this little dance to register your connection
;; with the IRC network.
(defn register-connection
  "writes NICK and USER (and optionally PASS) messages to IRC, registering the connection."
  [irc]
  (let [{:keys [pass nick username real-name init-mode]} @irc]
    (when pass
      (write-irc-line irc "PASS" pass))
    (write-irc-line irc "NICK" nick)
    (write-irc-line irc "USER" (or username nick) init-mode "*" (end real-name))))

;; If something happens with the connection that we don't otherwise notice,
;; we want it to be able to timeout appropriately so that we can move along.
;; This will set a timeout in milliseconds that will throw a SocketTimeoutException
;; if no data is received during that time.
(defn set-timeout
  "Set a timeout on the socket. timeout is in milliseconds."
  [irc timeout]
  (when timeout
    (.setSoTimeout (:socket (:connection @irc)) timeout)))

;; `BufferedReader`, the reader we use, promises that reading from it if it is empty
;; (if it is dead/closed/etc) will return nil. Unfortunately, the `InputStream` from
;; Socket throws an `IOException` instead. Because of this, we can't use the `line-seq`
;; from core and still handle dead connections gracefully. This is the same as
;; `clojure.core/line-seq` but catches the IOException and returns nil.
(defn safe-line-seq
  "Get an infinite lazy sequence of lines from a reader."
  [rdr]
  (try
    (cons (.readLine rdr) (lazy-seq (safe-line-seq rdr)))
    (catch IOException _ nil)))
