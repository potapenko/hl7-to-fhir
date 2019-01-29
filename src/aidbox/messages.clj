(ns aidbox.messages
  (:require [com.nervestaple.hl7-parser.parser :as hl7]
            [com.nervestaple.hl7-parser.message :as hl7-message]
            [clojure.string :as string]
            [clojure.java.io :as io])
  (:import [java.util Date]))

(defn fix-message-text [s]
  (-> s
   string/trim
   (string/replace #"(\n|\r)+" (str (char hl7/ASCII_CR)))))

(defn get-stored-message [index]
  (some->> (io/file "./resources/")
           file-seq
           (filter #(-> % .getName (string/starts-with? (str index))))
           first
           slurp
           fix-message-text))

(comment

  (hl7/parse (get-stored-message 4))
  (hl7/parse (get-stored-message 1))
  (hl7/parse (test-message))

  (def my-delimiters (-> (test-message) hl7/parse :delimiters))

  (hl7/create-message my-delimiters
                      (hl7/create-segment "MSH"
                                          (hl7/create-field (hl7/pr-delimiters my-delimiters))
                                          (hl7/create-field ["MYAPP"])
                                          (hl7/create-field ["TEST LAB"])
                                          (hl7/create-field ["19202830920"])))

  )
