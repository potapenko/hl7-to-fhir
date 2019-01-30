(ns aidbox.domain)

(comment "

MSH: the message header (required everywhere)
EVN: indicating the event that happened (in this case the admission of the patient)
PID: the patient identification segment. Note that demographics can be included within the PID segment itself
NK1: next of kin (which is always good to know in a hospital context in case something goes wrong)
PV1: information about the patient “visit” - this is where you would include the physician info and the current location of the patient.
Since an image is worth a thousand words, I’ll try and illustrate the process using images rather than a lot of words. I’m not going to give you the mapping tables etc. as I’m pretty sure the HL7 licensing agreements might prohibit sharing outside our org.")


(defmulti parse-segment :id)
(defmult parse-field (fn [segment field]))

(defmethod parse-segment :MSH [segment]
  segment)

(defmethod parse-segment :EVN [segment]
  segment)


(comment

  (parse-segment {:id :MSH :fields [1 2 3]})

  )
