(ns aidbox.hl7.orm
  (:require [aidbox.hl7.messages :as messages]
            [aidbox.hl7.util :as util]
            [clojure.java.io :as io]))

(def load-orm
  (memoize
   (fn []
     (-> (io/resource "orm.edn")
         slurp
         read-string))))

(defn parse-segment [{:keys [id] :as segment}]
  (let [id (keyword id)]
   (when-let [orm (id (load-orm))]
     [id
      (into
       {}
       (map (fn [{:keys [id sequence	length data-type required repetition name]} data]
              [id data])
            (:fields orm)
            (:fields segment)))])))

(defn parse-message [message]
  (->> message
       :segments
       (map parse-segment)
       (into {})))

(comment

  (load-orm)


  )
