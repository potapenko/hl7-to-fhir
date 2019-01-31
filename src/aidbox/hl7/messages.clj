(ns aidbox.hl7.messages
  (:require [aidbox.hl7.parser :as parser]
            [clojure.string :as string]
            [clojure.java.io :as io]))

(defn get-stored-message [index]
  (some->>  "hl7"
            io/resource
            io/as-file
            file-seq
            (filter #(-> % .getName (string/starts-with? (str index))))
            first
            slurp))

(defn parse [s]
  (-> s parser/parse))

(comment

  (parser/parse (get-stored-message 4))

  )
