(ns hl7-to-fhir-translation.test-03-orm
  (:use midje.sweet)
  (:require [aidbox.hl7.messages :as messages]
            [clojure.pprint :refer [pprint]]
            [aidbox.hl7.orm :as orm]))

(fact "Parse orm"
      (-> (messages/get-stored-message 4)
          (messages/parse)
          (orm/parse-message))

      )
