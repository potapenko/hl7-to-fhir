(ns aidbox.orm
  (:require [aidbox.messages :as messages]
            [clj-time.format :as f]
            [clojure.pprint :refer [pprint]]
            [com.nervestaple.hl7-parser.parser :as hl7]))

(def hl7-time-format (f/formatter "yyyyMMddHHmmSS"))

(defmulti parse-segment :id)

;; hello

(defn parse-message [message]
  (->> message
       :segments
       (map parse-segment)
       (into {})))
