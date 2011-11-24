(ns irclj.events
  "Default callbacks and event firing.")

;; We're using an event-based system for communicating with users. This
;; fires event callbacks.
(defn fire
  "Fire a callback of type type if it exists, passing irc and args."
  [irc type & args]
  (when-let [callback (get-in @irc [:callbacks type])]
    (apply callback irc args)))

;; This is the default raw-log callback function. It logs input and output to
;; stdout, which is the most common use case.
(defn stdout-callback
  "A raw-log callback that prints to stdout."
  [_ type s]
  (println
   (case type
     :write (str ">> " s)
     :read s)))