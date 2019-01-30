(ns aidbox.domain
  (:require [aidbox.messages :as messages]
            [clojure.pprint :refer [pprint]]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clj-time.coerce :as c]
            [com.nervestaple.hl7-parser.parser :as hl7]))

(def hl7-time-format (f/formatter "yyyyMMddHHmmSS"))

(comment "

MSH: the message header (required everywhere)
EVN: indicating the event that happened (in this case the admission of the patient)
PID: the patient identification segment. Note that demographics can be included within the PID segment itself
NK1: next of kin (which is always good to know in a hospital context in case something goes wrong)
PV1: information about the patient “visit” - this is where you would include the physician info and the current location of the patient.
Since an image is worth a thousand words, I’ll try and illustrate the process using images rather than a lot of words. I’m not going to give you the mapping tables etc. as I’m pretty sure the HL7 licensing agreements might prohibit sharing outside our org.")

(defmulti parse-segment :id)

(defmethod parse-segment "MSH" [segment]
  (let [[_
         [sending-app] [sending-facility]
         [receiving-app] [receiving-facility]
         [when]
         _
         [msg-type event-type]
         [msg-id]
         _ _] (->> segment :fields (map :content))]
    {:from       {:app sending-app :facility sending-facility}
     :to         {:app receiving-app :facility receiving-facility}
     :date       (f/parse hl7-time-format when)
     :msg-type   (keyword msg-type)
     :envet-type (keyword event-type)
     :msg-id     msg-id}))

(defmethod parse-segment "EVN" [segment]
  "not implemented")

(defmethod parse-segment "EVN" [segment]
  "not implemented")

(defmethod parse-segment "PID" [segment]
  "not implemented")

(defmethod parse-segment "NK1" [segment]
  "not implemented")

(defmethod parse-segment "PV1" [segment]
  "not implemented")

(defmethod parse-segment nil [segment]
  "not implemented")

(defmethod parse-segment :default [segment]
  "can't parse")

(defn parse-message [message]
  (->> message
       :segments
       (map parse-segment)))


(comment

  (pprint (hl7/parse (messages/get-stored-message 4)))

  (first (parse-message (hl7/parse (messages/get-stored-message 4))))

  (first (parse-message (hl7/parse (messages/get-stored-message 6))))



  )
