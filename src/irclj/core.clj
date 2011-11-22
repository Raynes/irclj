(ns irclj.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import java.net.Socket))

(defn create-connection [host port]
  (let [socket (Socket. host port)]
    {:socket socket
     :in (io/reader socket)
     :out (io/writer socket)}))

(defn read-irc-line [{:keys [in]}]
  (binding [*in* in]
    (read-line)))

(defn write-irc-line [{:keys [out]} s]
  (binding [*out* out]
    (println s)))

(defn connect [host port & {:keys [timeout]}]
  (let [irc (ref {:connection (create-connection host port)
                  :shutdown? false})
        {irc :connection} @irc]
    (.start
     (Thread.
      (fn []
        (when timeout
          (.setSoTimeout (:socket irc) timeout))
        (while true
          (let [line (read-irc-line irc)]
            (cond
             (.startsWith line "PING")
             (write-irc-line irc (.replace line "PING" "PONG"))))))))))