(ns irclj.connection
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [irclj.events :as events]
            [gloss.core :as gloss]
            [aleph.tcp :as tcp]
            [lamina.core :as lamina]))

;; We want to allow users to log raw input and output in whatever way
;; they please. To that, we'll fire the :raw-log callback function when
;; we read and write.
(defn write-irc-line
  "Writes a line to the IRC connection and fires the raw-log callback.
   Can take arbitrary arguments, joining them with spaces (like println)."
  [irc & s]
  (lamina/enqueue (:connection @irc)
                  (string/join " " s)))

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
  [host port irc-promise]
  (let [conn @(tcp/tcp-client {:host host, :port port,
                               :frame (gloss/string :utf-8 :delimiters ["\r\n"])})]
    (lamina/splice (doto (lamina/channel)
                     (->> (lamina/join conn)))
                   (doto (->> (lamina/channel)
                              (lamina/map* (fn [x]
                                             (if (coll? x)
                                               (string/join " " x)
                                               x))))
                     (lamina/join conn)
                     (lamina/receive-all (fn [s]
                                           (events/fire @irc-promise :raw-log :write s)))))))

;; IRC requires that you do this little dance to register your connection
;; with the IRC network.
(defn register-connection
  "writes NICK and USER (and optionally PASS) messages to IRC, registering the connection."
  [irc]
  (let [{:keys [pass nick username real-name init-mode connection]} @irc]
    (apply lamina/enqueue connection
           `[~@(when pass [["PASS" pass]])
             ~["NICK" nick]
             ~["USER" (or username nick) init-mode "*" (end real-name)]])))
