(ns   
    #^{:author "Anthony Simpson (Rayne)",
       :doc "A small IRC library to abstract over lower-level IRC connection handling."} 
    irclj.irclj
    (:use clojure.contrib.io)
    (:import [java.io PrintStream PrintWriter BufferedReader InputStreamReader]
	     java.net.Socket))

(defrecord IRC [name password server username port realname fnmap])

(defn create-bot 
  "Function to create an IRC(bot). You need to at most supply a server and fnmap.
  If you don't supply a name, username, realname, or port, they will default to
  irclj, irclj, teh bawt, and 6667 respectively."
  [{:keys [name password server username port realname fnmap]
    :or {name "irclj" username "irclj" realname "teh bawt"
	 port 6667}}]
  (IRC. name password server username port realname fnmap))

(defn send-message 
  "Takes an IRC, a message and a target to send it to, and sends an IRC message to
  target."
  [{{sockout :sockout} :connection} target message]
  (.println sockout (str "PRIVMSG " target " :" message))
  (println (str ">>>PRIVMSG " target " :" message)))

(defn mess-to-map
  "Parses a message into a map."
  [[user doing & more]]
  (let [[nick ident hostmask] (.split user "\\!|\\@")
	message-map {:user user
		     :nick nick
		     :ident ident
		     :doing doing}]
    (merge message-map 
	   (condp = doing
	     "PRIVMSG" {:channel (first more) :message (->> more second rest butlast (apply str))}
	     "QUIT" {:reason (apply str (rest more))}
	     "JOIN" {:channel (apply str (rest more))}
	     "PART" {:channel (first more) :reason (apply str (rest (second more)))}
	     "NOTICE" {:target (first more) :message (->> more second rest butlast (apply str))}
	     "MODE" {:channel (first more) :mode (second more) :user (last more)}
	     {}))))

(defn handle [& more])

(defn close
  "Closes an IRC connection (including the socket)."
  [{{:keys [sock sockout sockin]} :connection}]
  (.println sockout "QUIT")
  (.close sock)
  (.close sockin)
  (.close sockout))

(defn connect
  "Takes an IRC defrecord and optionally, a sequence of channels to join and
  connects to IRC based on the information provided in the IRC and optionally joins
  the channels. The connection itself runs in a separate thread, and the input stream
  and output stream are merged into the IRC and returned."
  [#^IRC {:keys [name password server username port realname fnmap server port] :as botmap}
   & {channels :channels}]
  (let [sock (Socket. server port)
	sockout (PrintWriter. (output-stream sock) true)
	sockin (reader (input-stream sock))]
    (doto sockout
      (.println (str "NICK " name))
      (.println (str "USER " username " na na :" realname)))
    (.start (Thread. (fn []
		       (while (not (.isClosed sock))
			      (let [rline (try (.readLine sockin) 
					       (catch java.net.SocketException _ "Socket Closed."))
				    line (apply str (rest rline))
				    words (.split line " ")]
				(println rline)
				(cond
				 (.startsWith rline "PING") (.println sockout (.replace rline "PING" "PONG"))
				 (= (second words) "001") (doseq [channel channels] 
							    (.println sockout (str "JOIN " channel))))
				:else (handle (mess-to-map words) fnmap))))))
    (assoc botmap :connection {:sock sock :sockin sockin :sockout sockout})))


(def bot (create-bot {:name "ircljbot" :server "irc.freenode.net"}))
(def newbot (connect bot :channels ["#irclj"]))
(send-message newbot "#irclj" (read-line))
(read-line)
(close newbot)