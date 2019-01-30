(ns aidbox.code-generator
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]))


(defn string->keyword [s]
  (or
   (some-> s
           string/lower-case
           (string/replace #"[(-.^].+" "")
           (string/replace #"&.+" "")
           (string/replace #"^\d+" "")
           string/trim
           (string/replace #"[_\s/]+" "-")
           keyword)
   :invalid-string))

(defn parse-one-segment [s]
  (let [[header _ & fields] (->> s
                                 string/split-lines
                                 (map string/trim)
                                 (remove string/blank?))
        [_ info id]         (re-find #"^(.+?)?\s*\((.+)\).*$" header)]
    (when id
     (println ">>" info id)
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
                             :info       name}))))})))

(defn- wrap-vector [s]
  (str "[" s "]"))

(defn create-method [{:keys [info id fields]}]
  (let [vars-str (->> fields
                      (map (fn [x]
                             (-> x :id name wrap-vector)))
                      (string/join "\n         "))
        map-str  ""]
    (format
     (slurp (io/resource "templates/segment-method.txt"))
      info id vars-str id map-str)))

(defn generate-orm []
  (->> (io/resource "tables/segments")
       io/as-file
       file-seq
       (sort-by #(-> % .lastModified))
       (remove #(-> % .isDirectory))
       ;; (take 1)
       (map slurp)
       (map parse-one-segment)
       (remove nil?)
       (map create-method)
       (string/join "\n\n")
       (format (slurp (io/resource "templates/orm.txt")))
       (spit (io/file "./src/aidbox/orm.clj"))))

(comment

  (generate-orm)


  )
