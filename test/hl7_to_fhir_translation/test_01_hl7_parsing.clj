(ns hl7-to-fhir-translation.test-01-hl7-parsing
  (:use midje.sweet)
  (:require [aidbox.messages :as messages]
            [com.nervestaple.hl7-parser.parser :as hl7]
            [clojure.pprint :refer [pprint]])
  (:import[java.util Date]))

(defn test-message []
  (str "MSH|^~\\&|AcmeHIS|StJohn|CATH|StJohn|20061019172719||ORM^O01|"
       (. (new Date) getTime) "|P|2.3" (char hl7/ASCII_CR)
       "PID|||20301||Durden^Tyler^^^Mr.||19700312|M|||88 Punchward Dr.^^Los Angeles^CA^11221^USA|||||||"(char hl7/ASCII_CR)
       "PV1||O|OP^^||||4652^Paulson^Robert|||OP|||||||||9|||||||||||||||||||||||||20061019172717|20061019172718" (char hl7/ASCII_CR)
       "ORC|NW|20061019172719" (char hl7/ASCII_CR)
       "OBR|1|20061019172719||76770^Ultrasound: retroperitoneal^C4|||12349876"))

(def my-delimiters (-> (test-message) hl7/parse :delimiters))

(fact "All messages parsed"
      (->> (range 1 16)
           (map messages/get-stored-message)
           (map hl7/parse)
           doall))

(fact "Create new message"
      (hl7/create-message my-delimiters
                          (hl7/create-segment "MSH"
                                              (hl7/create-field (hl7/pr-delimiters my-delimiters))
                                              (hl7/create-field ["MYAPP"])
                                              (hl7/create-field ["TEST LAB"])
                                              (hl7/create-field ["19202830920"]))) => map?)


;; ADT 4-7

(comment "
MSH: the message header (required everywhere)
EVN: indicating the event that happened (in this case the admission of the patient)
PID: the patient identification segment. Note that demographics can be included within the PID segment itself
NK1: next of kin (which is always good to know in a hospital context in case something goes wrong)
PV1: information about the patient “visit” - this is where you would include the physician info and the current location of the patient.
Since an image is worth a thousand words, I’ll try and illustrate the process using images rather than a lot of words. I’m not going to give you the mapping tables etc. as I’m pretty sure the HL7 licensing agreements might prohibit sharing outside our org.
")

(fact "Structure"


      (-> (messages/get-stored-message 5)
          hl7/parse
          pprint
          )


      )
