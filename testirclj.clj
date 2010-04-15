(use 'irclj.irclj)

(def fnmap {:on-message (fn [{:keys [nick channel message irc]}] 
			  (let [[cmd & more] (.split message " ")] 
			    (condp = cmd
			      "$whoareyou?" (send-action irc channel (:name @irc))
			      "$setnick" (set-nick irc (first more))
			      "$setmode" (set-mode irc channel (first more) (second more))
			      "$join" (join-chan irc (first more))
			      "$part" (part-chan irc (first more))
			      nil)))
	    :on-quit (fn [{:keys [nick reason irc]}] 
		       (send-message irc "#irclj" (str nick " quit. His reason was: " reason)))
	    :on-part (fn [{:keys [nick reason channel irc]}]
		       (send-message irc channel (str nick " parted. Reason: " reason)))})

(def bot (create-bot {:name "ircljbot" :server "irc.freenode.net" :fnmap fnmap}))
(def newbot (connect bot :channels ["#()" "#irclj"]))
(read-line)
(close newbot)