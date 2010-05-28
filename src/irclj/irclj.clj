(ns   
    #^{:author "Anthony Simpson (Rayne)"
       :doc "A small IRC library to abstract over lower-level IRC connection handling."} 
    irclj.irclj
    (:use [clojure.contrib io [string :only [join]] [def :only [defmacro-]]])
    (:import [java.io PrintStream PrintWriter BufferedReader InputStreamReader]
	     java.net.Socket))

(defrecord IRC [name password server username port realname fnmap])

(defn create-irc 
  "Function to create an IRC(bot). You need to at most supply a server and fnmap.
  If you don't supply a name, username, realname, or port, they will default to
  irclj, irclj, teh bawt, and 6667 respectively."
  [{:keys [name password server username port realname fnmap]
    :or {name "irclj" username "irclj" realname "teh bawt"
	 port 6667}}]
  (IRC. name password server username port realname fnmap))

(defn print-irc-line
  "Prints a line of text to an IRC connection."
  [{{sockout :sockout} :connection} text]
  (println (str ">>>" text))
  (binding [*out* sockout] 
    (println text)
    (str sockout)))

(defn read-irc-line
  "Reads a line from IRC. Returns the string 'Socket Closed.' if the socket provided is closed."
  [{{sockin :sockin} :connection}]
  (try 
   (binding [*in* sockin]
     (let [line (read-line)]
       (println line)
       line))
   (catch java.net.SocketException _ "Socket Closed.")))

(defn- strip-start
  "Strips everything away until the message."
  [s]
  (second (.split s ":")))

(defn send-msg
  "Utility function for sending raw messages. Don't use this function unless you're implementing
  a protocol function. Use send-message instead."
  [irc type message]
  (print-irc-line @irc (str type " " message)))

(defn send-raw-message
  "Takes an IRC, a message and a target to send it to, and sends an IRC message to
  target (user, channel). Be careful using this function, because newlines and carriage
  returns are filter out. You want to use send-message unless you need this behavior."
  [irc target message]
  (send-msg irc "PRIVMSG" (str target " :" message)))

(defn send-message
  "Takes an IRC, a message and a target to send it to, and sends an IRC message to
  target (user, channel). Filters out newlines and carriage returns."
  [irc target message]
  (send-msg irc "PRIVMSG" (str target " :" (.replaceAll message "\n|\r" ""))))

(defn send-notice
  "Takes an IRC, a message, and a target to send to, and sends a NOTICE to target
  (user, channel)."
  [irc target message]
  (send-msg irc "NOTICE" (str target " :" message)))

(defn send-action
  "Sends a CTCP ACTION to a target (user, channel)"
  [irc target message]
  (send-msg irc "PRIVMSG" (str target " :" \ "ACTION " message \)))

(defn set-nick
  "Changes your nick."
  [irc nick]
  (let [res (send-msg irc "NICK" nick)]
    (when (= (second (.split (read-irc-line @irc) " ")) "NICK")
      (dosync (alter irc assoc :name nick)))
    res))

(defn join-chan
  "Joins a channel."
  [irc channel]
  (let [res (send-msg irc "JOIN" (str ":" channel))
	rline (apply str (rest (read-irc-line @irc)))
	words (.split rline " ")]
    (println (str ":" rline))
    (when-not (= (second words) "403")
      (dosync (alter irc assoc :channels (conj (:channels @irc) channel))))
    res))

(defn part-chan
  "Leaves a channel."
  [irc channel & {reason :reason}]
  (let [res (send-msg irc "PART" channel)]
    (dosync (alter irc assoc :channels (remove #(= % channel) (:channels @irc))))
    res))

(defn set-mode
  "Set modes."
  [irc channel mode nick]
  (send-msg irc "MODE" (str channel " " mode " " nick)))

(defn set-topic
  "Sets the topic for a channel."
  [irc channel topic]
  (send-msg irc "TOPIC" (str channel " :" topic)))

(defn kick
  "Kicks a user from a channel."
  [irc channel nick & {reason :reason :or [reason "bai"]}]
  (send-msg irc "KICK" (str channel " " nick " :" reason)))

(defn get-names
  "Gets a list of the users in a channel. Includes modes. Returns nil if the channel
  doesn't exist."
  [irc channel]
  (send-msg irc "NAMES" channel)
  (loop [acc []]
    (let [rline (apply str (rest (read-irc-line @irc)))
	  words (.split rline " ")
	  num (second words)]
      (when-not (= num "403")
	(if (= num "353")
	  (recur (conj acc (strip-start rline))) 
	  (.split (apply str (interpose " " acc)) " "))))))

(defn get-topic
  "Gets the topic of a channel. Returns a map of :topic, :set-by, and :date
  (haven't quite worked date out yet). If the channel doesn't exist, returns nil."
  [irc channel]
  (send-msg irc "TOPIC" channel)
  (let [rline (apply str (rest (read-irc-line @irc)))
	words (.split rline " ")]
    (when (= (second words) "332")
      (let [rline2 (.split (read-irc-line irc) " ")]
	{:topic (apply str (rest (drop-while #(not= % \:) rline)))
	 :set-by (last (butlast rline2))
	 :date (last rline2)}))))

(defn whois
  "Sends a whois request and returns a map with the contents mapped to keys.
  Keys are: :loggedinas, :user, :channels, and :server. Returns nil if user
  doesn't exist."
  [irc nick]
  (send-msg irc "WHOIS" nick)
  (loop [acc []]
    (let [rline (apply str (rest (read-irc-line @irc)))
	  words (.split rline " ")
	  num (second words)]
      (when-not (= num "401")
	(if-not (= num "318")
	  (recur (->> words (drop 4) (interpose " ") (apply str) (conj acc)))
	  (zipmap [:user :channels :server :loggedinas] acc))))))

(defn identify
  "Indentifies with the network."
  [irc]
  (send-message irc "NickServ" (str "IDENTIFY " (:password @irc))))

(defn- extract-message [s]
  (apply str (rest (join " " s))))

(defn- mess-to-map
  "Parses a message into a map."
  [[user doing & [channel & message :as more] :as everything]]
  (let [[nick ident hostmask] (.split user "\\!|\\@")
	message-map {:user user
		     :nick nick
		     :hmask hostmask
		     :ident ident
		     :doing doing}]
    (merge message-map
	   {:raw-message (str ":" (apply str (interpose " " everything)))}
	   (condp = doing
	     "PRIVMSG" {:channel channel :message (extract-message message)}
	     "QUIT" {:reason (extract-message more)}
	     "JOIN" {:channel (apply str (rest channel))}
	     "PART" {:channel channel :reason (extract-message message)}
	     "NOTICE" {:target channel :message (extract-message message)}
	     "MODE" (let [[mode target] message] {:channel channel :mode mode :target target})
	     "TOPIC" {:channel channel :topic (extract-message message)}
	     "KICK" (let [[target & message] message] 
		      {:channel channel :target target :message (extract-message message)})
	     {}))))

(defmacro- when-not-nil 
  "Like when-not, but checks if it's predicate is nil."
  [pred & body]
  `(when-not (nil? ~pred) ~@body))

(defn- handle-ctcp
  "Takes a CTCP message and responds to it."
  [irc nick ctcp-s]
  (let [ctcp (apply str (remove #(= \ %) ctcp-s))]
    (when-not (= (first (.split ctcp " ")) "ACTION")
      (send-notice 
       irc nick (condp = ctcp
		  "VERSION" "irclj version ohai"
		  "TIME"    "Time for you to SHUT THE FUCK UP."
		  "FINGER"  "OMG, DADDY TOUCHED ME IN THE BAD PLACE.!"
		  "PING" "PONG!"
		  "Not supported.")))))

(defn- channel-or-nick [{:keys [channel nick irc] :as info-map}]
  (if (= channel (:name @irc)) (assoc info-map :channel nick) info-map))

(defn- handle 
  "Handles various IRC things. This is important."
  [{:keys [user nick ident doing channel message reason target mode] :as info} irc]
  
  (let [{{:keys [on-any on-action on-message on-quit on-part on-join
		 on-notice on-mode on-topic on-kick]} :fnmap} @irc
		 info-map (assoc info :irc irc)]
    ; This will be executed independent of what type of event comes in. Great for logging.
    (when-not-nil on-any (on-any info-map))
    (condp = doing
	"PRIVMSG" (if (= (first message) \)
		    (handle-ctcp irc nick message)
		    (if (and on-action (.startsWith message "ACTION"))
		      (on-action (channel-or-nick info-map))
		      (when-not-nil on-message (on-message (channel-or-nick info-map)))))
	"QUIT" (when-not-nil on-quit (on-quit info-map))
	"JOIN" (when-not-nil on-join (on-join info-map))
	"PART" (when-not-nil on-part (on-part info-map))
	"NOTICE" (when-not-nil on-notice (on-notice info-map))
	"MODE" (when-not-nil on-mode (on-mode info-map))
	"TOPIC" (when-not-nil on-topic (on-topic info-map))
	"KICK" (when-not-nil on-kick (on-kick info-map))
	nil)))

(defn close
  "Closes an IRC connection (including the socket)."
  [irc]
  (let [{{:keys [sock sockout sockin]} :connection} @irc]
    (print-irc-line @irc "QUIT")
    (.close sock)
    (.close sockin)
    (.close sockout)))

(defn connect
  "Takes an IRC defrecord and optionally, a sequence of channels to join and
  connects to IRC based on the information provided in the IRC and optionally joins
  the channels. The connection itself runs in a separate thread, and the input stream
  and output stream are merged into the IRC and returned as a ref.

  If you wish to identify after connecting, you'll want to supply a password to your IRC
  record, and the optional key :identify-after-secs to connect. :indentify-after-secs takes
  a number of seconds to wait after identifying before joining channels. This is useful for
  people with hostmasks."
  [#^IRC {:keys [name password server username port realname fnmap server port] :as botmap}
   & {:keys [channels identify-after-secs]}]
  (let [sock (Socket. server port)
	sockout (PrintWriter. (output-stream sock) true)
	sockin (reader (input-stream sock))
	irc (ref (assoc botmap :connection {:sock sock :sockin sockin :sockout sockout}))]
    (.start (Thread. (fn []
		       (print-irc-line @irc (str "NICK " name))
		       (print-irc-line @irc (str "USER " username " na na :" realname))
		       (while (not (.isClosed sock))
			      (let [rline (read-irc-line @irc)
				    line (apply str (rest rline))
				    words (.split line " ")]
				(cond
				 (.startsWith rline "PING") ; :>> 
				 (do 
				   (print-irc-line @irc (.replace rline "PING" "PONG")))
				 (= (second words) "001")  ; :>>
				 (do
				   (when (and identify-after-secs password)
				     (identify irc)
				     (Thread/sleep (* 1000 identify-after-secs))
				     (println "Sleeping while identification takes place."))
				   (when channels
				     (doseq [channel channels] 
				       (join-chan irc channel)))))
				:else (handle (mess-to-map words) irc))))))
    irc))