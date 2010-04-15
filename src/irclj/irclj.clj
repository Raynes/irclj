(ns   
    #^{:author "Anthony Simpson (Rayne)",
       :doc "A small IRC library to abstract over lower-level IRC connection handling."} 
    irclj.irclj
  (:import [java.io PrintStream PrintWriter BufferedReader InputStreamReader]
	   java.net.Socket))

(defn mess-to-map
  ""
  [[user doing & more]]
  (let [[nick ident hostmask] (.split user "\\!|\\@")
	message-map {:user user
		     :nick nick
		     :ident ident
		     :doing doing}]
    (merge message-map 
	   (condp = doing
	     "PRIVMSG" {:channel (first more) :message (apply str (butlast (rest (second more))))}
	     "QUIT" {:reason (apply str (rest more))}
	     "JOIN" {:channel (apply str (rest more))}
	     "PART" {:channel (first more) :reason (apply str (rest (second more)))}))))
  
(defn connect
  ""
  [{:keys [name password server channels username port realname fnmap] 
    :or {port 6667 username "irclj" realname "I'm a robot, hear me roar!"
	 channels ["#irclj"]}}]
  (let [sock (Socket. server port)
	sockin (PrintWriter. (.getOutputStream sock) true)
	sockout (BufferedReader. (InputStreamReader. (.getInputStream sock)))]
    (doto sockout
      (.println (str "NICK " name))
      (.println (str "USER " username " na na :" realname)))
    (while (not (.isClosed sock))
	   (let [rline (.readLine sockin)
		 line (apply str (rest rline))
		 words (.split lines " ")]
	     (println line)
	     (cond
	      (.startsWith rline "PING") (.println sockout (.replace rline "PING" "PONG"))
	      (= (nth words 1) "001") (doseq [channel channels] 
					(.println sockout (str "JOIN " channel))))
	     ))))