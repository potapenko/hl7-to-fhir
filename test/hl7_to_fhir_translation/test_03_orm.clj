(ns hl7-to-fhir-translation.test-03-orm
  (:use midje.sweet)
  (:require [aidbox.messages :as messages]
            [com.nervestaple.hl7-parser.parser :as hl7]
            [clojure.pprint :refer [pprint]]
            [aidbox.orm :as orm]))

(fact "Parse orm"
      (-> (messages/get-stored-message 4)
          (messages/parse)
          (orm/parse-message)
          )


      )
