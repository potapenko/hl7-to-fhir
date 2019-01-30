(ns aidbox.orm
  (:require [aidbox.messages :as messages]
            [clj-time.format :as f]
            [clojure.pprint :refer [pprint]]
            [com.nervestaple.hl7-parser.parser :as hl7]))

(def hl7-time-format (f/formatter "yyyyMMddHHmmSS"))

(defmulti parse-segment :id)

;; Master File Identification
(defmethod parse-segment "MFI" [segment]
  (let [[[master-file-identifier]
         [master-file-application-identifier]
         [file]
         [entered-date-time]
         [effective-date-time]
         [response-level-code]] (->> segment :fields (map :content))]
    [:MFI
     ]))


;; Master File Entry
(defmethod parse-segment "MFE" [segment]
  (let [[[record]
         [mfn-control-id]
         [effective-date-time]
         [primary-key-value]
         [primary-key-value-type]] (->> segment :fields (map :content))]
    [:MFE
     ]))


;; Message Acknowledgement
(defmethod parse-segment "MSA" [segment]
  (let [[[acknowledgement-code]
         [message-control-id]
         [text-message]
         [expected-sequence-number]
         [delayed-ack-type]] (->> segment :fields (map :content))]
    [:MSA
     ]))


;; Event Type
(defmethod parse-segment "EVN" [segment]
  (let [[[invalid-string]
         [date-time-of-event]
         [date-time-planned-event]
         [event-reason-code]] (->> segment :fields (map :content))]
    [:EVN
     ]))


;; Merge Patient Information
(defmethod parse-segment "MRG" [segment]
  (let [[[prior-patient-identifier-list]
         [prior-alternate-patient-id]
         [prior-patient-account-number]
         [prior-patient-id]
         [prior-visit-number]
         [prior-alternate-visit-id]
         [prior-patient-name]] (->> segment :fields (map :content))]
    [:MRG
     ]))


;; Patiet Identification
(defmethod parse-segment "PID" [segment]
  (let [[[set-id]
         [patient-id]
         [patient-identifier-list]
         [alternate-patient-id]
         [patient-name]
         [mother’s-maiden-name]
         [date-time-of-birth]
         [sex]
         [patient-alias]
         [race]
         [patient-address]
         [county-code]
         [phone-number]
         [phone-number]
         [primary-language]
         [marital-status]
         [religion]
         [patient-account-number]
         [ssn-number]
         [driver's-license-number]
         [mother's-identifier]
         [ethnic-group]
         [birth-place]
         [multiple-birth-indicator]
         [birth-order]
         [citizenship]
         [veterans-military-status]
         [nationality]
         [patient-death-date-and-time]
         [patient-death-indicator]
         [identity-unknown-indicator]
         [identity-reliability-code]
         [last-update-date-time]
         [last-update-facility]
         [species-code]
         [breed-code]
         [strain]
         [production-class-code]
         [tribal-citizenship]] (->> segment :fields (map :content))]
    [:PID
     ]))


;; Patient Additional Demographics
(defmethod parse-segment "PD1" [segment]
  (let [[[living-dependency]
         [living-arrangement]
         [patient-primary-facility]
         [patient-primary-care-provider-name]
         [student-indicator]
         [handicap]
         [living-will]
         [organ-donor]
         [separate-bill]
         [duplicate-patient]
         [publicity-code]
         [protection-indicator]] (->> segment :fields (map :content))]
    [:PD1
     ]))


;; Patient Visit
(defmethod parse-segment "PV1" [segment]
  (let [[[set-id]
         [patient-class]
         [assigned-patient-location]
         [admission-type]
         [pre]
         [prior-patient-location]
         [attending-doctor]
         [refering-doctor]
         [consulting-doctor]
         [hospital-service]
         [temporary-location]
         [pre]
         [re]
         [admit-source]
         [ambulatory-status]
         [vip-indicators]
         [admitting-doctor]
         [patient-type]
         [visit-number]
         [financial-class]
         [charge-price-indicator]
         [courtesy-code]
         [credit-rating]
         [contract-code]
         [contract-effective-date]
         [contract-amount]
         [contract-period]
         [interest-code]
         [transfer-to-bad-debt-code]
         [transfer-to-bad-debt-date]
         [bad-debt-agency-code]
         [bad-debt-transfer-amount]
         [bad-debt-recovery-amount]
         [delete-account-indicator]
         [delete-account-date]
         [discharge-disposition]
         [discharged-to-location]
         [diet-type]
         [servicing-facility]
         [bed-status]
         [account-status]
         [pending-location]
         [prior-temporary-location]
         [admit-date-time]
         [discharge-date-time]
         [current-patient-balance]
         [total-charges]
         [total-adjustments]
         [total-payments]
         [alternate-visit-id]
         [visit-indicator]
         [other-healthcare-provider]] (->> segment :fields (map :content))]
    [:PV1
     ]))


;; Patient Visit - Additional Information
(defmethod parse-segment "PV2" [segment]
  (let [[[prior-pending-location]
         [accommodation-code]
         [admit-reason]
         [transfer-reason]
         [patient-valuables]
         [patient-valuables-location]
         [visit-user-code]
         [expected-admit-date-time]
         [expected-discharge-date-time]
         [estimated-length-of-inpatient-stay]
         [actual-length-of-inpatient-stay]
         [visit-description]
         [referral-source-code]
         [previous-service-date]
         [employment-illness-related-indicator]
         [purge-status-code]
         [purge-status-date]
         [special-program-code]
         [retention-indicator]
         [expected-number-of-insurance-plans]
         [visit-publicity-code]
         [visit-protection-indicator]
         [clinic-organization-name]
         [patient-status-code]
         [visit-priority-code]
         [previous-treatment-date]
         [expected-discharge-disposition]
         [signature-on-file-date]
         [first-similar-illness-date]
         [patient-charge-adjustment-code]
         [recurring-service-code]
         [billing-media-code]
         [expected-surgery-date]
         [military-partnership-code]
         [military-non]
         [newborn-baby-indicator]
         [baby-detained-indicator]] (->> segment :fields (map :content))]
    [:PV2
     ]))


;; Diagnosis
(defmethod parse-segment "DG1" [segment]
  (let [[[set-id]
         [diagnosis-coding-method]
         [diagnosis-code]
         [diagnosis-description]
         [diagnosis-date-time]
         [diagnosis-drg-type]
         [major-diagnostic-category]
         [diagnostic-related-group]
         [drg-approval-indicator]
         [drg-grouper-review-code]
         [outlier-type]
         [outlier-days]
         [outlier-cost]
         [grouper-version-and-type]
         [diagnosis-drg-priority]
         [diagnosing-clinician]] (->> segment :fields (map :content))]
    [:DG1
     ]))


;; Financial Transaction
(defmethod parse-segment "FT1" [segment]
  (let [[[set-id]
         [transaction-id]
         [transaction-batch-id]
         [transaction-date]
         [transaction-posting-date]
         [transaction-type]
         [transaction-code]
         [transaction-description]
         [transaction-desc]
         [transaction-quantity]
         [transaction-amount]
         [transaction-amount]
         [department-code]
         [insurance-plan-id]
         [insurance-amount]
         [patient-location]
         [fee-schedule]
         [patient-type]
         [diagnosis-code]
         [performed-by-code]
         [ordered-by-code]
         [unit-cost]
         [filler-order-number]
         [entered-by-code]
         [procedure-code]
         [procedure-code-modifier]] (->> segment :fields (map :content))]
    [:FT1
     ]))


;; Guarantor
(defmethod parse-segment "GT1" [segment]
  (let [[[set-id]
         [guarantor-number]
         [guarantor-name]
         [guarantor-spouse-name]
         [guarantor-address]
         [guarantor-phone]
         [guarantor-phone]
         [guarantor-date-of-birth]
         [guarantor-sex]
         [guarantor-type]
         [guarantor-relationship]
         [guarantor-ssn]
         [guarantor-date]
         [guarantor-date]
         [guarantor-priority]
         [guarantor-employer-name]
         [guarantor-employer-addr]
         [guarantor-employer-phone]
         [guarantor-employee-id-#]
         [guarantor-employmt-status]
         [guarantor-organization-name]
         [guarantor-billing-hold-flag]
         [guarantor-credit-rating-code]
         [guarantor-death-date-and-time]
         [guarantor-death-flag]
         [guarantor-charge-adjustment-code]
         [guarantor-household-annual-income]
         [guarantor-household-size]
         [guarantor-employer-id-number]
         [guarantor-marital-status-code]
         [guarantor-hire-effective-date]
         [employment-stop-date]
         [living-dependency]
         [ambulatory-status]
         [citizenship]
         [primary-language]
         [living-arrangement]
         [publicity-code]
         [protection-indicator]
         [student-indicator]
         [religion]
         [mother's-maiden-name]
         [nationality]
         [ethnic-group]
         [contact-person's-name]
         [contact-person's-telephone-number]
         [contact-reason]
         [contact-relationship]
         [job-title]
         [job-code-class]
         [guarantor-employer's-organization-name]
         [handicap]
         [job-status]
         [guarantor-financial-class]
         [guarantor-race]] (->> segment :fields (map :content))]
    [:GT1
     ]))


;; Insurance
(defmethod parse-segment "IN1" [segment]
  (let [[[set-id]
         [insurance-plan-id]
         [insurance-company-id]
         [insurance-company-name]
         [insurance-company-address]
         [insurance-co-contact-pers]
         [insurance-co-phone-number]
         [group-number]
         [group-name]
         [insured's-group-emp]
         [insured's-group-emp]
         [plan-effective-date]
         [plan-expiration-date]
         [authorization-information]
         [plan-type]
         [name-of-insured]
         [insured's-relation-to-pat]
         [insured's-date-of-birth]
         [insured's-address]
         [assignment-of-benefits]
         [coordination-of-benefits]
         [coord]
         [notice-of-admission-code]
         [notice-of-admission-date]
         [rpt-of-eligibility-code]
         [rpt-of-eligibility-date]
         [release-information-code]
         [pre]
         [verification-date]
         [verification-by]
         [type-of-agreement-code]
         [billing-status]
         [lifetime-reserve-days]
         [delay-before-l]
         [company-plan-code]
         [policy-number]
         [policy-deductible]
         [policy-limit]
         [policy-limit]
         [room-rate]
         [room-rate]
         [insured's-employ-status]
         [insured's-sex]
         [insured's-employer-addr]
         [verification-status]
         [prior-insurance-plan-id]
         [coverage-type]
         [handicap]
         [insured<92>s-id-number]] (->> segment :fields (map :content))]
    [:IN1
     ]))


;; Insurance - Additional Information
(defmethod parse-segment "IN2" [segment]
  (let [[[insured’s-employee-id]
         [insured’s-social-security-number]
         [insured’s-employer’s-name-and-id]
         [employer-information-data]
         [mail-claim-party]
         [medicare-health-ins-card-number]
         [medicaid-case-name]
         [medicaid-case-number]
         [military-sponsor-name]
         [military-id-number]
         [dependent-of-military-recipient]
         [military-organization]
         [military-station]
         [military-service]
         [military-rank-grade]
         [military-status]
         [military-retire-date]
         [military-non]
         [baby-coverage]
         [combine-baby-bill]
         [blood-deductible]
         [special-coverage-approval-name]
         [special-coverage-approval-title]
         [non]
         [payor-id]
         [payor-subscriber-id]
         [eligibility-source]
         [room-coverage-type-amount]
         [policy-type-amount]
         [daily-deductible]
         [living-dependency]
         [ambulatory-status]
         [citizenship]
         [primary-language]
         [living-arrangement]
         [publicity-code]
         [protection-indicator]
         [student-indicator]
         [religion]
         [mother’s-maiden-name]
         [nationality]
         [ethnic-group]
         [marital-status]
         [insured’s-employment-start-date]
         [employment-stop-date]
         [job-title]
         [job-code-class]
         [job-status]
         [employer-contact-person-name]
         [employer-contact-person-phone-number]
         [employer-contact-reason]
         [insured’s-contact-person’s-name]
         [insured’s-contact-person-phone-number]
         [insured’s-contact-person-reason]
         [relationship-to-the-patient-start-date]
         [relationship-to-the-patient-stop-date]
         [insurance-co]
         [insurance-co-contact-phone-number]
         [policy-scope]
         [policy-source]
         [patient-member-number]
         [guarantor’s-relationship-to-insured]
         [insured’s-phone-number]
         [insured’s-employer-phone-number]
         [military-handicapped-program]
         [suspend-flag]
         [copay-limit-flag]
         [stoploss-limit-flag]
         [insured-organization-name-and-id]
         [insured-employer-organization-name-and-id]
         [race]
         [hcfa-patient’s-relationship-to-insured]] (->> segment :fields (map :content))]
    [:IN2
     ]))


;; Insurance - Additional Information, Certification
(defmethod parse-segment "IN3" [segment]
  (let [[[set-id]
         [certification-number]
         [certified-by]
         [certification-required]
         [penalty]
         [certification-date-time]
         [certification-modify-date-time]
         [operator]
         [certification-begin-date]
         [certification-end-date]
         [days]
         [non]
         [non]
         [physician-reviewer]
         [certification-contact]
         [certification-contact-phone-number]
         [appeal-reason]
         [certification-agency]
         [certification-agency-phone-number]
         [pre]
         [case-manager]
         [second-opinion-date]
         [second-opinion-status]
         [second-opinion-documentation-received]
         [second-opinion-physician]] (->> segment :fields (map :content))]
    [:IN3
     ]))


;; Procedures
(defmethod parse-segment "PR1" [segment]
  (let [[[set-id]
         [procedure-coding-method]
         [procedure-code]
         [procedure-description]
         [procedure-date-time]
         [procedure-type]
         [procedure-minutes]
         [anesthesiologist]
         [anesthesia-code]
         [anesthesia-minutes]
         [surgeon]
         [resident-code]
         [consent-code]] (->> segment :fields (map :content))]
    [:PR1
     ]))


;; Error
(defmethod parse-segment "ERR" [segment]
  (let [[[error-code-and-location]] (->> segment :fields (map :content))]
    [:ERR
     ]))


;; Notes and Comments
(defmethod parse-segment "NTE" [segment]
  (let [[[set-id]
         [source-of-comment]
         [comment]
         [comment-type]] (->> segment :fields (map :content))]
    [:NTE
     ]))


;; Scheduling Activity Information
(defmethod parse-segment "SCH" [segment]
  (let [[[placer-appointment-id]
         [filler-appointment-id]
         [occurrence-number]
         [placer-group-number]
         [schedule-id]
         [event-reason]
         [appointment-reason]
         [appointment-type]
         [appointment-duration]
         [appointment-duration-units]
         [appointment-timing-quantity]
         [placer-contact-person]
         [placer-contact-phone-number]
         [placer-contact-address]
         [placer-contact-location]
         [filler-contact-person]
         [filler-contact-phone-number]
         [filler-contact-address]
         [filler-contact-location]
         [entered-by-person]
         [entered-by-phone-number]
         [entered-by-location]
         [parent-placer-appointment-id]
         [parent-filler-appointment-id]
         [filler-status-code]] (->> segment :fields (map :content))]
    [:SCH
     ]))


;; Resource Group
(defmethod parse-segment "RGS" [segment]
  (let [[[set-id]
         [segment-action-code]
         [resource-group-id]] (->> segment :fields (map :content))]
    [:RGS
     ]))


;; Appointment Information - Location resource
(defmethod parse-segment "AIL" [segment]
  (let [[[set-id]
         [segment-action-code]
         [location-resource-id]
         [location-type]
         [location-group]
         [start-date-time]
         [start-date-time-offset]
         [start-date-time-offset-units]
         [duration]
         [duration-units]
         [allow-substitution-code]
         [filler-status-code]] (->> segment :fields (map :content))]
    [:AIL
     ]))


;; Appointment Information - Personnel Resource
(defmethod parse-segment "AIP" [segment]
  (let [[[set-id]
         [segment-action-code]
         [personnel-resource-id]
         [resource-role]
         [resource-group]
         [start-date-time]
         [start-date-time-offset]
         [start-date-time-offset-units]
         [duration]
         [duration-units]
         [allow-substitution-code]
         [filler-status-code]] (->> segment :fields (map :content))]
    [:AIP
     ]))


;; Appointment Information - General Resource
(defmethod parse-segment "AIG" [segment]
  (let [[[set-id]
         [segment-action-code]
         [resource-id]
         [resource-type]
         [resource-group]
         [resource-quantity]
         [resource-quantity-units]
         [start-date-time]
         [start-date-time-offset]
         [start-date-time-offset-units]
         [duration]
         [duration-units]
         [allow-substitution-code]
         [filler-status-code]] (->> segment :fields (map :content))]
    [:AIG
     ]))


;; Accident
(defmethod parse-segment "ACC" [segment]
  (let [[[accident-date-time]
         [accident-code]
         [accident-location]
         [auto-accident-state]
         [accident-job-related-indicator]
         [accident-death-indicator]] (->> segment :fields (map :content))]
    [:ACC
     ]))


;; UB82
(defmethod parse-segment "UB1" [segment]
  (let [[[set-id]
         [blood-deductible]
         [blood-furnished]
         [blood-replaced]
         [blood-not-replaced]
         [co]
         [condition-code]
         [covered-days--]
         [non-covered-days]
         [value-amount]
         [number-of-grace-days]
         [special-program-indicator]
         [psro-ur-approval-indicator]
         [psro-ur-approved-stay]
         [psro-ur-approved-stay]
         [occurrence]
         [occurrence-span]
         [span-start-date]
         [span-end-date]
         [ub]
         [ub]
         [ub]
         [ub]] (->> segment :fields (map :content))]
    [:UB1
     ]))


;; UB92 Data
(defmethod parse-segment "UB2" [segment]
  (let [[[set-id]
         [co]
         [condition-code]
         [covered-days]
         [non]
         [value-amount]
         [occurrence-code]
         [occurrence-span-code-dates]
         [ub92-locator-2]
         [ub92-locator-11]
         [ub92-locator-31]
         [document-control-number]
         [ub92-locator-49]
         [ub92-locator-56]
         [ub92-locator-57]
         [ub92-locator-78]
         [special-visit-count]] (->> segment :fields (map :content))]
    [:UB2
     ]))


;; Next of Kin/Associated Parties
(defmethod parse-segment "NK1" [segment]
  (let [[[set-id]
         [name]
         [relationship]
         [address]
         [phone-number]
         [business-phone-number]
         [contact-role]
         [start-date]
         [end-date]
         [next-of-kin-associated-parties-job-title]
         [next-of-kin-associated-parties-jobcode-class]
         [next-of-kin-associated-parties-employeenumber]
         [organization-name]
         [marital-status]
         [sex]
         [date-time-of-birth]
         [living-dependency]
         [ambulatory-status]
         [citizenship]
         [primary-language]
         [living-arrangement]
         [publicity-code]
         [protection-indicator]
         [student-indicator]
         [religion]
         [mother's-maiden-name]
         [nationality]
         [ethnic-group]
         [contact-reason]
         [contact-person's-name]
         [contact-person's-telephone-number]
         [contact-person's-address]
         [next-of-kin-associated-party's-identifiers]
         [job-status]
         [race]
         [handicap]
         [contact-person-social-security-number]] (->> segment :fields (map :content))]
    [:NK1
     ]))


;; Common Order
(defmethod parse-segment "ORC" [segment]
  (let [[[order-control-code]
         [placer-order-number]
         [filler-order-number]
         [placer-group-number]
         [order-status]
         [response-flag]
         [quantity-timing]
         [parent]
         [transaction-date-time]
         [entered-by]
         [verified-by]
         [ordering-provider]
         [enterer's-location]
         [call-back-phone-number]
         [order-effective-date-time]
         [order-control-code-reason]
         [entering-organization]
         [entering-device]
         [action-by]
         [advanced-beneficiary-notice-code]
         [ordering-facility-name]
         [ordering-facility-address]
         [ordering-facility-phone-number]
         [ordering-provider-address]] (->> segment :fields (map :content))]
    [:ORC
     ]))


;; Observation request
(defmethod parse-segment "OBR" [segment]
  (let [[[set-id]
         [placer-order-number]
         [filler-order-number]
         [universal-service-id]
         [priority]
         [requested-date-time]
         [observation-date-time]
         [observation-end-date-time]
         [collection-volume]
         [collector-identifier]
         [specimen-action-code]
         [danger-code]
         [relevant-clinical-info]
         [specimen-received-date-time]
         [specimen-source]
         [ordering-provider]
         [order-callback-phone-number]
         [placer-field-1]
         [placer-field-2]
         [filler-field-1]
         [filler-field-2]
         [results-rpt-change-date-time]
         [charge-to-practice]
         [diagnostice-serv-sect-id]
         [result-status]
         [parent-result]
         [quantity-timing]
         [result-copies-to]
         [parent]
         [transportation-mode]
         [reason-for-study]
         [principal-result-interpreter]
         [assistant-result-interpreter]
         [technician]
         [transcriptionist]
         [scheduled-date-time]
         [number-of-sample-containers]
         [transport-logistics-of-collected-samples]
         [collector's-comment]
         [transport-arrangement-responsibility]
         [transport-arranged]
         [escort-required]
         [planned-patient-transport-comment]
         [procedure-code]
         [procedure-code-modifier]] (->> segment :fields (map :content))]
    [:OBR
     ]))


;; Observation/Result
(defmethod parse-segment "OBX" [segment]
  (let [[[set-id]
         [value-type]
         [observation-identifier]
         [observation-sub]
         [observation-value]
         [units]
         [reference-range]
         [abnormal-flags]
         [probability]
         [nature-of-abnormal-test]
         [observation-result-status]
         [date-last-obs-normal-value]
         [user-defined-access-checks]
         [date-time-of-the-observation]
         [producer's-id]
         [responsible-observer]
         [observation-method]] (->> segment :fields (map :content))]
    [:OBX
     ]))


;; Pharmacy/Treatment Administration
(defmethod parse-segment "RXA" [segment]
  (let [[[give-sub]
         [administration-sub]
         [date-time-start-of-administration]
         [date-time-end-of-administration]
         [administered-code]
         [administered-amount]
         [administered-units]
         [administered-dosage-form]
         [administration-notes]
         [administering-provider]
         [administered]
         [administered-per]
         [administered-strength]
         [administered-strength-units]
         [substance-lot-number]
         [substance-expiration-date]
         [substance-manufacturer-name]
         [substance-refusal-reason]
         [indication]
         [completion-status]
         [action-code]
         [system-entry-date-time]] (->> segment :fields (map :content))]
    [:RXA
     ]))


;; Pharmacy/Treatment Route
(defmethod parse-segment "RXR" [segment]
  (let [[[route]
         [site]
         [administration-device]
         [administration-method]
         [routing-instruction]] (->> segment :fields (map :content))]
    [:RXR
     ]))


;; Transcription Document Header
(defmethod parse-segment "TXA" [segment]
  (let [[[set-id]
         [document-type]
         [document-content-presentation]
         [activity-date-time]
         [primary-activity-provider-code-name]
         [origination-date-time]
         [transcription-date-time]
         [edit-date-time]
         [originator-code-name]
         [assigned-document-authenticator]
         [transcriptionist-code-name]
         [unique-document-number]
         [parent-document-number]
         [placer-order-number]
         [filler-order-number]
         [unique-document-file-name]
         [document-completion-status]
         [document-confidentiality-status]
         [document-availability-status]
         [document-storage-status]
         [document-change-reason]
         [authentication-person]
         [distributed-copies]] (->> segment :fields (map :content))]
    [:TXA
     ]))


;; Query Acknowledgement
(defmethod parse-segment "QAK" [segment]
  (let [[[query-tag]
         [event-identifier]
         [input-parameter-list]] (->> segment :fields (map :content))]
    [:QAK
     ]))


;; Original Style Query Definition
(defmethod parse-segment "QRD" [segment]
  (let [[[query-date-time]
         [query-format-code]
         [query-priority]
         [query-id]
         [deferred-response-type]
         [deferred-response-date-time]
         [quantity-limited-request]
         [who-subject-filter]
         [what-subject-filter]
         [what-department-data-code]
         [what-data-code-value-qual]
         [query-results-level]] (->> segment :fields (map :content))]
    [:QRD
     ]))


;; Original Style Query Filter
(defmethod parse-segment "QRF" [segment]
  (let [[[where-subject-filter]
         [when-data-start-date-time]
         [when-data-end-date-time]
         [what-user-qualifier]
         [other-qry-subject-filter]
         [which-date-time-qualifier]
         [which-date-time-status-qualifier]
         [date-time-selection-qualifier]
         [when-quantity-timing-qualifier]] (->> segment :fields (map :content))]
    [:QRF
     ]))


;; Message Header
(defmethod parse-segment "MSH" [segment]
  (let [[[field-separator]
         [encoding-characters]
         [sending-application]
         [sending-facility]
         [receiving-application]
         [receiving-facility]
         [date-time-of-message]
         [security]
         [message-type]
         [message-control-id]
         [processing-id]
         [version-id]
         [sequence-number]
         [continuation-pointer]
         [accept-acknowledgment-type]
         [application-acknowledgment-type]
         [country-code]
         [character-set]] (->> segment :fields (map :content))]
    [:MSH
     ]))


(defn parse-message [message]
  (->> message
       :segments
       (map parse-segment)
       (into {})))
