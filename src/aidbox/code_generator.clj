(ns aidbox.code-generator
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-one-segment [s]
  (let [[header _ & fields] (->> s
                               string/split-lines
                               (map string/trim)
                               (remove string/blank?))
        [_ info id] (re-find #"^(.+?)?\s*\((.+)\).*$" header)]
    {:info info
     :id id
     :fields (->> fields
                  (map #(string/split % #"\s+")))}))

(defn generate-orm []
  (->> (io/resource "tables/segments")
       io/as-file
       file-seq
       (sort-by #(-> % .lastModified))
       (take 1)
       (map slurp)
       (map parse-one-segment)
       doall))

(comment

  (generate-orm)


  )
