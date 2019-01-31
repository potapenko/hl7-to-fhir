(ns aidbox.hl7.adt
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def adt-table
  (->> "tables/adt.txt"
       io/resource
       slurp
       string/split-lines
       (map string/trim)
       (remove string/blank?)
       (map #(string/split % #"\s+"))
       (map (fn [[name info]]
              [(keyword name) info]))
       (into {})))

