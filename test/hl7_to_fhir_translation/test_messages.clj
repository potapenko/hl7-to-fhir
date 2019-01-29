(ns hl7-to-fhir-translation.test-messages
  (:use midje.sweet)
  (:require [aidbox.messages :as messages]
            [com.nervestaple.hl7-parser.parser :as hl7]))

(fact "All messages parsed"
      (->> (range 1 16)
           (map messages/get-stored-message)
           (map hl7/parse)
           doall))
