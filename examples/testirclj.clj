(use 'irclj.irclj)

(def fnmap {:on-message (fn [{:keys [nick channel message irc]}] 
			  (let [[cmd & more] (.split message " ")] 
			    (condp = cmd
			      "!whoareyou?" (send-action irc channel (:name @irc))
			      "!setnick" (set-nick irc (first more))
			      "!setmode" (set-mode irc channel (first more) (second more))
			      "!join" (join-chan irc (first more))
			      "!part" (part-chan irc (first more))
			      "!names" (get-names irc (first more))
			      "!channels" (send-message irc channel (apply str (interpose " " (:channels @irc))))
			      "!whois" (println (whois irc (first more)))
			      "!topic?" (doseq [x (vals (get-topic irc (first more)))] (println x))
                              "!printirc" (println @irc)
			      nil)))
	    :on-quit (fn [{:keys [nick reason irc]}] 
		       (send-message irc "#irclj" (str nick " quit. His reason was: " reason)))
	    :on-part (fn [{:keys [nick reason channel irc]}]
		       (send-message irc channel (str nick " parted. Reason: " reason)))
	    :on-action (fn [{:keys [nick message reason channel irc]}]
			 (send-message irc channel (str "you said " message)))})

(def bot (connect (create-irc {:name "ircljbot" :server "irc.freenode.net" :fnmap fnmap}) 
		  :channels ["#()" "#irclj"]))
(read-line)
(close bot)