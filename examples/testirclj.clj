(use 'irclj.core)

(def fnmap {:on-message (fn [{:keys [nick channel message irc]}] 
			  (let [[cmd & more] (.split message " ")] 
			    (condp = cmd
			      "!whoareyou?" (send-action irc channel (:name @irc))
			      "!setnick" (set-nick irc (first more))
			      "!setmode" (set-mode irc channel (first more) (second more))
			      "!join" (join-chan irc (first more))
			      "!part" (part-chan irc (first more))
                              "!part2" (part-chan irc (first more) :reason "Cause I feel like it. ")
			      "!names" (get-names irc (first more))
			      "!channels" (send-message irc channel (apply str (interpose " " (:channels @irc))))
			      "!whois" (println (whois irc (first more)))
			      "!topic?" (doseq [x (vals (get-topic irc (first more)))] (println x))
                              "!printirc" (println @irc)
                              "!dividebyzero" (/ 1 0)
                              "!ping" (send-msg irc "PING" "blahblahblah")
			      nil)))
	    :on-quit (fn [{:keys [nick reason irc]}] 
		       (send-message irc "#irclj" (str nick " quit. His reason was: " reason)))
	    :on-part (fn [{:keys [nick reason channel irc]}]
		       (send-message irc channel (str nick " parted. Reason: " reason)))
	    on-action (fn [{:keys [nick message reason channel irc]}]
                        (send-message irc channel (str "you said " message)))
            :on-connect (fn [_] (println "\n\nON-CONNECT TRIGGERED.\n\n"))
            :on-error (fn [{:keys [irc channel error doing]}]
                        (send-message irc channel (str "While executing the " doing " handler, an error occurred: " (.getMessage error))))})

(def bot (connect (create-irc {:name "ircljbot" :server "irc.freenode.net" :fnmap fnmap}) 
		  :channels ["#irclj" "#dld" ["#keyed" "secret"]]))
(read-line)
(close bot)