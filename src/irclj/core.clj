(ns irclj.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [slingshot.slingshot :as stone]
            [irclj.parser :as parser])
  (:import (java.net Socket SocketTimeoutException)
           java.io.IOException))

(defn create-connection [host port]
  (let [socket (Socket. host port)]
    {:socket socket
     :in (io/reader socket)
     :out (io/writer socket)}))

(defn fire [irc type & args]
  (when-let [callback (get-in @irc [:callbacks type])]
    (apply callback irc args)))

(defn write-irc-line [irc & s]
  (let [s (string/join " " s)]
    (fire irc :raw-log :write s)
    (binding [*out* (get-in @irc [:connection :out])]
      (println s))))

(defn stdout-callback [_ type s]
  (println
   (case type
     :write (str ">> " s)
     :read s)))

(defmulti process-line
  "Process a parsed IRC message."
  (fn [m _] (:command m)))

(defmethod process-line "433" [m irc]
  (stone/throw+ (into @irc {:parsed-message m})
                "Nick is already taken. Can't recover."))

(defmethod process-line "PING" [m irc]
  (write-irc-line irc (.replace (:raw m) "PING" "PONG")))

(defmethod process-line :default [& _] nil)

(defn- register-connection [irc]
  (let [{:keys [nick username real-name init-mode]} @irc]
    (write-irc-line irc "NICK" nick)
    (write-irc-line irc "USER" (or username nick) init-mode "*" (str ":" real-name))))

(defn- set-timeout [irc timeout]
  (when timeout
    (.setSoTimeout (:socket (:connection @irc)) timeout)))

(defn- safe-line-seq [rdr]
  (try
    (cons (.readLine rdr) (lazy-seq (safe-line-seq rdr)))
    (catch IOException _ nil)))

(defn- process [irc line]
  (fire irc :raw-log :read line)
  (process-line (parser/parse line) irc))

(defn connect [host port nick &
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