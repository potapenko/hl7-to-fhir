(ns hl7-to-fhir-translation.test-02-mapping
  (:use midje.sweet)
  (:require [aidbox.messages :as messages]
            [com.nervestaple.hl7-parser.parser :as hl7]
            [clojure.pprint :refer [pprint]]))

(def step-1
  (messages/parse
   "MSH|^~\\&|EPICADT|DH|RADADT|DH|201701010915||ADT^A01|MSG000001|P|2.3
    PID|||MRN12345^5^M11||Doe^Jane||19780101|F||C|||||||||123456789|"))

(fact "Patient admitting"

      (pprint step-1)


      )
