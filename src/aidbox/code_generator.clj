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
           ;; (string/replace #"^when|other|which")
           string/trim
           (string/replace #"[_\s/]+" "-")
           keyword)
   :invalid-string))

(defn parse-one-segment [s]
  (let [[header _ & fields] (->> s
                                 string/split-lines
                                 (map string/trim)
                                 (remove string/blank?))
        [_ info id]         (or
                             (re-find #"^(.+?)?\s*\((.+)\).*$" header)
                             [nil "No info" (string/trim header)])]
    (if-not id
     (println "Parse error:" header)
     (do
       (println "Parse success:" header "->" id)
       {:info   info
        :id     id
        :fields (->> fields
                     (map #(string/split % #"\t+"))
                     (map (fn [[sequence	length data-type required repetition name]]
                            (let [id (string->keyword name)]
                              {:id         id
                               :sequence   (read-string sequence)
                               :length     (read-string  length)
                               :data-type  (-> data-type string->keyword clojure.core/name string/upper-case keyword)
                               :required   (= required "REQ")
                               :repetition (string->keyword repetition)
                               :info       name})))
                     (reduce (fn [res {:keys [id] :as field}]
                               (let [use-before (-> res :ids (get id 0))
                                     unique-id  (if (pos? use-before)
                                                  (keyword (format "%s-%s" (name id) (inc use-before)))
                                                  id)]
                                 (-> res
                                     (update :fields conj (assoc field :id unique-id))
                                     (update :ids assoc id (inc use-before)))))
                             {:ids {} :fields []})
                     :fields)}))))

(defn- wrap-vector [s]
  (str "[" s "]"))

(defn- destruct-field-data [{:keys [id sequence	length data-type required repetition name info]}]
  (str (-> id clojure.core/name wrap-vector)  " ;; " info " (" data-type ")"))

(defn prepare-field-data [{:keys [id sequence	length data-type required repetition name]}]
  (-> id clojure.core/name))

(defn create-method [{:keys [info id fields]}]
  (let [vars-str (->> fields
                      (map destruct-field-data)
                      (string/join "\n         "))
        map-str  (str
                  "{"
                  (->> fields
                       (map (fn [{:keys [id info] :as field}]
                              (str id " " (prepare-field-data field))))
                       distinct
                       (string/join "\n      ")
                       )
                  "}")]
    (format
     (slurp (io/resource "templates/segment-method.txt"))
      info id vars-str id map-str)))

(defn generate-orm []
  (->> (io/resource "tables/segments")
       io/as-file
       file-seq
       (sort-by #(or (-> % .lastModified)))
       (remove #(-> % .isDirectory))
       (sort-by #(if (-> % .getName (= "msh.txt")) -1 1))
       (map slurp)
       (map parse-one-segment)
       (remove nil?)
       (map create-method)
       (string/join "\n")
       (format (slurp (io/resource "templates/orm.txt")))
       (spit (io/file "./src/aidbox/orm.clj"))))

(comment

  (generate-orm)


  )
