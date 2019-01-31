(ns hl7-to-fhir-translation.test-01-hl7-parsing
  (:use midje.sweet)
  (:require [aidbox.hl7.messages :as messages]
            [aidbox.hl7.parser :as parser]
            [clojure.pprint :refer [pprint]]))

(fact "All messages parsed"
      (->> (range 1 16)
           (map messages/get-stored-message)
           (map parser/parse)
           doall))

(comment "
MSH: the message header (required everywhere)
EVN: indicating the event that happened (in this case the admission of the patient)
PID: the patient identification segment. Note that demographics can be included within the PID segment itself
NK1: next of kin (which is always good to know in a hospital context in case something goes wrong)
PV1: information about the patient “visit” - this is where you would include the physician info and the current location of the patient.
Since an image is worth a thousand words, I’ll try and illustrate the process using images rather than a lot of words. I’m not going to give you the mapping tables etc. as I’m pretty sure the HL7 licensing agreements might prohibit sharing outside our org.
")

(fact "Structure"


      (-> (messages/get-stored-message 4)
          parser/parse
          pprint
          )


      )
