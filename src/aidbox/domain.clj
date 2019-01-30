(ns aidbox.domain
  (:require [aidbox.messages :as messages]
            [clj-time.format :as f]
            [clojure.pprint :refer [pprint]]
            [com.nervestaple.hl7-parser.parser :as hl7]))

(def hl7-time-format (f/formatter "yyyyMMddHHmmSS"))

(defmulti parse-segment :id)

(defmethod parse-segment "MSH" [segment]
  (let [[_
         [sending-app] [sending-facility]
         [receiving-app] [receiving-facility]
         [date]
         _
         [msg-type event-type]
         [msg-id]
         _ _] (->> segment :fields (map :content))]
    [:header
     {:sending    {:app sending-app :facility sending-facility}
      :receiving  {:app receiving-app :facility receiving-facility}
      :date       (when date (f/parse hl7-time-format date))
      :msg-type   (keyword msg-type)
      :envet-type (keyword event-type)
      :msg-id     msg-id}]))

(defn parse-message [message]
  (->> message
       :segments
       (map parse-segment)
       (into {})))

(comment

  (pprint (hl7/parse (messages/get-stored-message 4)))

  (first (parse-message (hl7/parse (messages/get-stored-message 4))))

  (second (parse-message (hl7/parse (messages/get-stored-message 6))))



  )
