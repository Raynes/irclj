(ns irclj.process
  "Processes IRC messages."
  (:require [clojure.string :as string]
            [irclj.connection :as connection]
            [irclj.events :as events]))

;; We're going handle IRC messages polymorphically. Whatever IRC commands we
;; support are implemented as process-line implementations. process-line takes
;; the result of irclj.parser/parse.
(defmulti process-line
  "Process a parsed IRC message."
  (fn [m _] (:command m)))

;; ### Numeric

(defn- parse-prefix
  "Parses the PREFIX section of an ISUPPORT message. Returns a map of symbols
   to their corresponding modes."
  [{:keys [raw]}]
  (when-let [[modes prefixes] (next (re-find #"PREFIX=\((.*?)\)(\S+)" raw))]
    (zipmap prefixes modes)))

;; We want to parse this line to find out which modes a user can have (operator,
;; voice, etc).
(defmethod process-line "005" [m irc]
  (when-let [prefixes (parse-prefix m)]
    (dosync (alter irc assoc :prefixes prefixes))))

;; IRC sends 332 to tell you what the channel topic is (if present).
(defmethod process-line "332" [{:keys [params] :as m} irc]
  (dosync (alter irc assoc-in [:channels (second params) :topic]
                 {:text (last params)}))
  (events/fire irc :332 m))

;; IRC sends 333 to tell you the user who last set the topic and when.
(defmethod process-line "333" [{:keys [params] :as m} irc]
  (let [[_ channel nick timestamp] params]
    (dosync
     (alter irc update-in [:channels channel :topic]
            assoc
            :nick nick
            :timestamp timestamp))))

(defn- nick-parser
  "Returns a function that parses a nick, returning a map where the nick
   is the key and the value is another map containing a :mode key which is
   either the user's mode (determined by the first character of the nick) if
   it is present in prefixes or nil if not."
  [prefixes]
  (fn [nick]
    (let [prefix (-> nick first prefixes)]
      [(if prefix (subs nick 1) nick) {:mode prefix}])))

(def ^{:doc "A map of indicators from 353 to their meanings."}
  indicators
  {"@" :secret
   "*" :private
   "=" :public})

;; 353 gives you the list of users that are in a channel. We want this.
(defmethod process-line "353" [{:keys [params]} irc]
  (let [[_ indicator channel names] params
        names (into {}
                    (map (nick-parser (:prefixes @irc))
                         (string/split names #" ")))]
    
    (dosync
     (alter irc update-in [:channels channel]
            (fn [old]
              (-> old
                  (assoc :indicator (indicators indicator))
                  (update-in [:users] #(into names %))))))))

;; At this point, the IRC server has registered our connection. We can communicate
;; this by delivering our ready? promise.
(defmethod process-line "001" [m irc]
  (deliver (:ready? @irc) true)
  (events/fire irc :001 m))

;; So we can keep mode lists up to date.
(defmethod process-line "324" [{:keys [params] :as m} irc]
  (let [[_ channel & modes] params]
    (dosync
     (alter irc assoc-in [:channels channel :mode] (string/join " " modes)))
    (events/fire irc :324 m)))

;; We can't really recover from a nick-already-in-use error. Just throw an
;; exception.
(defmethod process-line "433" [m irc]
  (events/fire irc :433 m)
  (throw (Exception. "Nick is already taken. Can't recover.")))

;; ### Wordy Responses

;; PONG!
(defmethod process-line "PING" [m irc]
  (connection/write-irc-line irc (.replace (:raw m) "PING" "PONG")))

(defn- update-nicks [users old-nick new-nick]  
  (when users
    (let [old-data (users old-nick)]
      (assoc (dissoc users old-nick) new-nick old-data))))

(defn- update-channels [channels old-nick new-nick]
  (into {}
        (for [[channel data] channels]
          [channel
           (update-in data [:users] update-nicks old-nick new-nick)])))

;; We need to process this so that we can reflect NICK changes. This is
;; a fairly complicated process. NICK messages give you no information
;; at all about what channels the user changing their nick is in. This
;; is understandable, but it means we have to work our asses off a bit.
;; This gnarly code is necessary because we need to update the user list
;; in each channel. Since we don't know *which* channels, we have to look
;; at all of them.
(defmethod process-line "NICK" [{:keys [nick params] :as m} irc]
  (let [new-nick (first params)]
    (dosync
     (alter irc
            (fn [old]
              (let [old (if (= (:nick @irc) nick)
                          (assoc old :nick new-nick)
                          old)]
                (update-in old [:channels] update-channels nick new-nick))))))
  (events/fire irc :nick m))

(defmethod process-line "JOIN" [{:keys [nick params] :as m} irc]
  (dosync
   (alter irc assoc-in [:channels (first params) :users nick] nil))
  (events/fire irc :join m))

(defmethod process-line "PART" [{:keys [nick params] :as m} irc]
  (dosync
   (alter irc update-in [:channels (first params) :users] dissoc nick))
  (events/fire irc :part m))

(defmethod process-line "QUIT" [{:keys [nick params] :as m} irc]
  (dosync
   (alter irc update-in [:channels (first params) :users] dissoc nick))
  (events/fire irc :part m))

;; Modes are complicated. Parsing them and trying to update a bunch of data properly
;; would be error-prone and pointless. Instead, we'll just let clients do that if
;; they really want to. However, we will go ahead and request the MODE from IRC
;; when we see that it has been changed, that way we can maintain the current channel
;; modes.
(defmethod process-line "MODE" [m irc]
  (connection/write-irc-line irc "MODE" (first (:params m)))
  (events/fire irc :mode m))

(defmethod process-line "KICK" [{:keys [params] :as m} irc]
  (dosync
   (alter irc update-in [:channels (first params) :users]
          dissoc (second params)))
  (events/fire irc :kick m))

(defmethod process-line "PRIVMSG" [{:keys [params] :as m} irc]
  (let [[target text] params
        m (assoc m :target target, :text text)]
    (if-let [[_ message] (re-find #"\u0001(.*)\u0001" text)]
      (let [[ctcp remainder] (string/split message #"\s+" 2)]
        (events/fire irc (keyword (str "ctcp-" (.toLowerCase ctcp)))
                     (assoc m
                       :ctcp-text remainder
                       :ctcp-kind ctcp)))
      (events/fire irc :privmsg m))))

;; We obviously don't need a defmethod for every single protocol response,
;; so we'll automagically fire callbacks for any response we don't recognize.
(defmethod process-line :default [m irc]
  (events/fire irc (-> m :command string/lower-case keyword) m))