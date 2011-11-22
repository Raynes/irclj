(ns irclj.parser
  (:require [clojure.string :as string]))

(defn extract-prefix [line-s]
  (when (= \: (ffirst line-s)) (first line-s)))

(defn parse-prefix [line-s]
  (when-let [prefix (extract-prefix line-s)]
    (zipmap [:nick :user :host] (string/split prefix #"!|@"))))

(defn parse-params [line-s prefix]
  (let [[single multi] (string/split
                        (string/join " "
                                     (if prefix
                                       (drop 2 line-s)
                                       (rest line-s)))
                        #":" 2)]
    (conj (string/split single #" ") multi)))

(defn parse-message [line]
  (let [line-s (string/split line #" ")
        prefix (parse-prefix line-s)]
    (into
     {:command (if prefix (second line-s) (first line-s))
      :params (parse-params line-s prefix)}
     prefix)))