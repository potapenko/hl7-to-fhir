(ns aidbox.messages
  (:require [com.nervestaple.hl7-parser.parser :as hl7]
            [com.nervestaple.hl7-parser.message :as hl7-message])
  (:import
   (java.util Date)))

(defn test-message []
  (str "MSH|^~\\&|AcmeHIS|StJohn|CATH|StJohn|20061019172719||ORM^O01|"
       (. (new Date) getTime) "|P|2.3" (char hl7/ASCII_CR)
       "PID|||20301||Durden^Tyler^^^Mr.||19700312|M|||88 Punchward Dr.^^Los Angeles^CA^11221^USA|||||||"(char hl7/ASCII_CR)
       "PV1||O|OP^^||||4652^Paulson^Robert|||OP|||||||||9|||||||||||||||||||||||||20061019172717|20061019172718" (char hl7/ASCII_CR)
       "ORC|NW|20061019172719" (char hl7/ASCII_CR)
       "OBR|1|20061019172719||76770^Ultrasound: retroperitoneal^C4|||12349876"))

(defn- main [& args]


  )

(defn get-stored-message [index]
  (slurp (format "./resources/hl7/%s.txt" index)))



(comment

  (hl7/parse (get-stored-message 1))

  (def my-delimiters (-> (test-message) hl7/parse :delimiters))

  (hl7/create-message my-delimiters
                      (hl7/create-segment "MSH"
                                          (hl7/create-field (hl7/pr-delimiters my-delimiters))
                                          (hl7/create-field ["MYAPP"])
                                          (hl7/create-field ["TEST LAB"])
                                          (hl7/create-field ["19202830920"])))

  )
