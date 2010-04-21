(ns irclj.irclj-test
  (:use [irclj.irclj] :reload-all)
  (:use clojure.test
	clojure.contrib.io))

(defn make-rdr [s]
  (reader
   (java.io.ByteArrayInputStream. 
    (.getBytes s))))


(defn fake-conn-info [] 
  (ref
   {:connection {:sockin (make-rdr ":rer/f34wr232/ PRIVMSG chan :ohai")
		 :sockout (java.io.StringWriter.)}}))

(deftest print-irc-line-test
  (is (= "lol\n" (print-irc-line (deref (fake-conn-info)) "lol"))))

(deftest read-irc-line-test
  (is (= ":rer/f34wr232/ PRIVMSG chan :ohai" (read-irc-line (deref (fake-conn-info))))))

(deftest send-msg-test
  (is (= (send-msg "PRIVMSG" (fake-conn-info) "#chan" ":sup there friend!")
	 "PRIVMSG #chan :sup there friend!\n")))

(deftest send-message-test
  (is (= (send-message (fake-conn-info) "#omg" "What's up?") "PRIVMSG #omg :What's up?\n")))

(deftest send-notice-test
  (is (= (send-notice (fake-conn-info) "Raynes" "Hai!") "NOTICE Raynes :Hai!\n")))

(deftest send-action-test
  (is (= (send-action (fake-conn-info) "#omg" "like's pancakes.") "PRIVMSG #omg :ACTION like's pancakes.\n")))

(deftest set-nick-test
  (let [con (ref {:connection {:sockin (make-rdr ":wellyeahdood NICK blahblah blah")
			       :sockout (java.io.StringWriter.)}
		  :name "Yamama"})]
    (is (= (set-nick con "Rayne") "NICK Rayne \n"))
    (is (= (:name @con) "Rayne"))))

(deftest join-test
  (let [con (ref {:connection {:sockin (make-rdr ":rtgern JOIN :#chan\n")
			       :sockout (java.io.StringWriter.)}
		  :channels []})]
    (is (= "JOIN  :#chan\n" (join-chan con "#chan")))
    (is (= "#chan" (first (:channels @con))))))

(deftest part-test
  (let [con (ref {:connection {:sockin (make-rdr ":blsnfreiu PART #chan")
			       :sockout (java.io.StringWriter.)}
		  :channels ["#chan"]})]
    (is (= "PART  #chan\n" (part-chan con "#chan")))
    (is (= nil (first (:channels @con))))))