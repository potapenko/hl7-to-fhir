(ns aidbox.util
  (:require [clj-time.format :as f]
            [clojure.pprint :refer [pprint]]
            [com.nervestaple.hl7-parser.parser :as hl7]))

(def hl7-time-format (f/formatter "yyyyMMddHHmmSS"))

(defn parse-hl7-date [s]
  (f/parse hl7-time-format s))
