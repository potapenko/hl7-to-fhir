(ns aidbox.code-generator
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]))


(defn string->keyword [s]
  (-> s
      string/lower-case
      (string/replace #"[_\s/]+" "-")
      keyword))

(defn parse-one-segment [s]
  (let [[header _ & fields] (->> s
                                 string/split-lines
                                 (map string/trim)
                                 (remove string/blank?))
        [_ info id]         (re-find #"^(.+?)?\s*\((.+)\).*$" header)]
    {:info   info
     :id     id
     :fields (->> fields
                  (map #(string/split % #"\t+"))
                  (map (fn [[sequence	length data-type required repetition name]]
                         (let [id (string->keyword name)]
                           {:id         id
                            :sequence   (read-string sequence)
                            :length     (read-string  length)
                            :data-type  (string->keyword data-type)
                            :required   (= required "REQ")
                            :repetition (string->keyword repetition)
                            :info       name}))))}))

(defn create-method [segment]
  ";; segment")

(defn generate-orm []
  (->> (io/resource "tables/segments")
       io/as-file
       file-seq
       (sort-by #(-> % .lastModified))
       (take 1)
       (map slurp)
       (map parse-one-segment)
       (map create-method)
       (string/join "\n\n")
       (format (slurp (io/resource "templates/orm.txt")))
       (spit (io/file "./src/aidbox/orm.clj"))))

(comment

  (pprint (generate-orm))


  )
