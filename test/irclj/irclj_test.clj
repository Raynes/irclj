(ns irclj.irclj-test
  (:use [irclj.irclj] :reload-all)
  (:use clojure.test
	clojure.contrib.io))

(defn fake-conn-info [] 
  (ref
   {:connection {:sockin (reader 
			  (java.io.ByteArrayInputStream. 
			   (.getBytes ":rer/f34wr232/ PRIVMSG chan :ohai")))
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