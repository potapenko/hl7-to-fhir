(ns aidbox.hl7.util
  (:require [clj-time.format :as f]
            [clojure.pprint :refer [pprint]]))

(def hl7-time-format (f/formatter "yyyyMMddHHmmSS"))

(defn parse-hl7-date [s]
  (f/parse hl7-time-format s))
