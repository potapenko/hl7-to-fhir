(ns aidbox.hl7.parser
  (:require [clojure.string :as string]))

(defn- parse-segment [s]
  (let [[id & fields] (->> (string/split s #"\|")
                             (map string/trim)
                             (map (fn [x]
                                    (if (re-find #"\^" x)
                                      (string/split x #"\^")
                                      x))))]
   {:id id :fields fields}))

(defn parse [s]
  {:segments
   (->> s
        string/split-lines
        (map string/trim)
        (remove string/blank?)
        (map parse-segment))})
