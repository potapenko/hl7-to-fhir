(ns aidbox.orm
  (:require [aidbox.messages :as messages]
            [clj-time.format :as f]
            [clojure.pprint :refer [pprint]]
            [com.nervestaple.hl7-parser.parser :as hl7]))

(def hl7-time-format (f/formatter "yyyyMMddHHmmSS"))

(defmulti parse-segment :id)

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
     {:field-separator field-separator
      :encoding-characters encoding-characters
      :sending-application sending-application
      :sending-facility sending-facility
      :receiving-application receiving-application
      :receiving-facility receiving-facility
      :date-time-of-message date-time-of-message
      :security security
      :message-type message-type
      :message-control-id message-control-id
      :processing-id processing-id
      :version-id version-id
      :sequence-number sequence-number
      :continuation-pointer continuation-pointer
      :accept-acknowledgment-type accept-acknowledgment-type
      :application-acknowledgment-type application-acknowledgment-type
      :country-code country-code
      :character-set character-set}]))

;; Master File Identification
(defmethod parse-segment "MFI" [segment]
  (let [[[master-file-identifier]
         [master-file-application-identifier]
         [file]
         [entered-date-time]
         [effective-date-time]
         [response-level-code]] (->> segment :fields (map :content))]
    [:MFI
     {:master-file-identifier master-file-identifier
      :master-file-application-identifier master-file-application-identifier
      :file file
      :entered-date-time entered-date-time
      :effective-date-time effective-date-time
      :response-level-code response-level-code}]))

;; Master File Entry
(defmethod parse-segment "MFE" [segment]
  (let [[[record]
         [mfn-control-id]
         [effective-date-time]
         [primary-key-value]
         [primary-key-value-type]] (->> segment :fields (map :content))]
    [:MFE
     {:record record
      :mfn-control-id mfn-control-id
      :effective-date-time effective-date-time
      :primary-key-value primary-key-value
      :primary-key-value-type primary-key-value-type}]))

;; Message Acknowledgement
(defmethod parse-segment "MSA" [segment]
  (let [[[acknowledgement-code]
         [message-control-id]
         [text-message]
         [expected-sequence-number]
         [delayed-ack-type]] (->> segment :fields (map :content))]
    [:MSA
     {:acknowledgement-code acknowledgement-code
      :message-control-id message-control-id
      :text-message text-message
      :expected-sequence-number expected-sequence-number
      :delayed-ack-type delayed-ack-type}]))

;; Event Type
(defmethod parse-segment "EVN" [segment]
  (let [[[invalid-string]
         [date-time-of-event]
         [date-time-planned-event]
         [event-reason-code]] (->> segment :fields (map :content))]
    [:EVN
     {:invalid-string invalid-string
      :date-time-of-event date-time-of-event
      :date-time-planned-event date-time-planned-event
      :event-reason-code event-reason-code}]))

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
     {:prior-patient-identifier-list prior-patient-identifier-list
      :prior-alternate-patient-id prior-alternate-patient-id
      :prior-patient-account-number prior-patient-account-number
      :prior-patient-id prior-patient-id
      :prior-visit-number prior-visit-number
      :prior-alternate-visit-id prior-alternate-visit-id
      :prior-patient-name prior-patient-name}]))

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
     {:set-id set-id
      :patient-id patient-id
      :patient-identifier-list patient-identifier-list
      :alternate-patient-id alternate-patient-id
      :patient-name patient-name
      :mother’s-maiden-name mother’s-maiden-name
      :date-time-of-birth date-time-of-birth
      :sex sex
      :patient-alias patient-alias
      :race race
      :patient-address patient-address
      :county-code county-code
      :phone-number phone-number
      :phone-number phone-number
      :primary-language primary-language
      :marital-status marital-status
      :religion religion
      :patient-account-number patient-account-number
      :ssn-number ssn-number
      :driver's-license-number driver's-license-number
      :mother's-identifier mother's-identifier
      :ethnic-group ethnic-group
      :birth-place birth-place
      :multiple-birth-indicator multiple-birth-indicator
      :birth-order birth-order
      :citizenship citizenship
      :veterans-military-status veterans-military-status
      :nationality nationality
      :patient-death-date-and-time patient-death-date-and-time
      :patient-death-indicator patient-death-indicator
      :identity-unknown-indicator identity-unknown-indicator
      :identity-reliability-code identity-reliability-code
      :last-update-date-time last-update-date-time
      :last-update-facility last-update-facility
      :species-code species-code
      :breed-code breed-code
      :strain strain
      :production-class-code production-class-code
      :tribal-citizenship tribal-citizenship}]))

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
     {:living-dependency living-dependency
      :living-arrangement living-arrangement
      :patient-primary-facility patient-primary-facility
      :patient-primary-care-provider-name patient-primary-care-provider-name
      :student-indicator student-indicator
      :handicap handicap
      :living-will living-will
      :organ-donor organ-donor
      :separate-bill separate-bill
      :duplicate-patient duplicate-patient
      :publicity-code publicity-code
      :protection-indicator protection-indicator}]))

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
     {:set-id set-id
      :patient-class patient-class
      :assigned-patient-location assigned-patient-location
      :admission-type admission-type
      :pre pre
      :prior-patient-location prior-patient-location
      :attending-doctor attending-doctor
      :refering-doctor refering-doctor
      :consulting-doctor consulting-doctor
      :hospital-service hospital-service
      :temporary-location temporary-location
      :pre pre
      :re re
      :admit-source admit-source
      :ambulatory-status ambulatory-status
      :vip-indicators vip-indicators
      :admitting-doctor admitting-doctor
      :patient-type patient-type
      :visit-number visit-number
      :financial-class financial-class
      :charge-price-indicator charge-price-indicator
      :courtesy-code courtesy-code
      :credit-rating credit-rating
      :contract-code contract-code
      :contract-effective-date contract-effective-date
      :contract-amount contract-amount
      :contract-period contract-period
      :interest-code interest-code
      :transfer-to-bad-debt-code transfer-to-bad-debt-code
      :transfer-to-bad-debt-date transfer-to-bad-debt-date
      :bad-debt-agency-code bad-debt-agency-code
      :bad-debt-transfer-amount bad-debt-transfer-amount
      :bad-debt-recovery-amount bad-debt-recovery-amount
      :delete-account-indicator delete-account-indicator
      :delete-account-date delete-account-date
      :discharge-disposition discharge-disposition
      :discharged-to-location discharged-to-location
      :diet-type diet-type
      :servicing-facility servicing-facility
      :bed-status bed-status
      :account-status account-status
      :pending-location pending-location
      :prior-temporary-location prior-temporary-location
      :admit-date-time admit-date-time
      :discharge-date-time discharge-date-time
      :current-patient-balance current-patient-balance
      :total-charges total-charges
      :total-adjustments total-adjustments
      :total-payments total-payments
      :alternate-visit-id alternate-visit-id
      :visit-indicator visit-indicator
      :other-healthcare-provider other-healthcare-provider}]))

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
     {:prior-pending-location prior-pending-location
      :accommodation-code accommodation-code
      :admit-reason admit-reason
      :transfer-reason transfer-reason
      :patient-valuables patient-valuables
      :patient-valuables-location patient-valuables-location
      :visit-user-code visit-user-code
      :expected-admit-date-time expected-admit-date-time
      :expected-discharge-date-time expected-discharge-date-time
      :estimated-length-of-inpatient-stay estimated-length-of-inpatient-stay
      :actual-length-of-inpatient-stay actual-length-of-inpatient-stay
      :visit-description visit-description
      :referral-source-code referral-source-code
      :previous-service-date previous-service-date
      :employment-illness-related-indicator employment-illness-related-indicator
      :purge-status-code purge-status-code
      :purge-status-date purge-status-date
      :special-program-code special-program-code
      :retention-indicator retention-indicator
      :expected-number-of-insurance-plans expected-number-of-insurance-plans
      :visit-publicity-code visit-publicity-code
      :visit-protection-indicator visit-protection-indicator
      :clinic-organization-name clinic-organization-name
      :patient-status-code patient-status-code
      :visit-priority-code visit-priority-code
      :previous-treatment-date previous-treatment-date
      :expected-discharge-disposition expected-discharge-disposition
      :signature-on-file-date signature-on-file-date
      :first-similar-illness-date first-similar-illness-date
      :patient-charge-adjustment-code patient-charge-adjustment-code
      :recurring-service-code recurring-service-code
      :billing-media-code billing-media-code
      :expected-surgery-date expected-surgery-date
      :military-partnership-code military-partnership-code
      :military-non military-non
      :newborn-baby-indicator newborn-baby-indicator
      :baby-detained-indicator baby-detained-indicator}]))

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
     {:set-id set-id
      :diagnosis-coding-method diagnosis-coding-method
      :diagnosis-code diagnosis-code
      :diagnosis-description diagnosis-description
      :diagnosis-date-time diagnosis-date-time
      :diagnosis-drg-type diagnosis-drg-type
      :major-diagnostic-category major-diagnostic-category
      :diagnostic-related-group diagnostic-related-group
      :drg-approval-indicator drg-approval-indicator
      :drg-grouper-review-code drg-grouper-review-code
      :outlier-type outlier-type
      :outlier-days outlier-days
      :outlier-cost outlier-cost
      :grouper-version-and-type grouper-version-and-type
      :diagnosis-drg-priority diagnosis-drg-priority
      :diagnosing-clinician diagnosing-clinician}]))

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
     {:set-id set-id
      :transaction-id transaction-id
      :transaction-batch-id transaction-batch-id
      :transaction-date transaction-date
      :transaction-posting-date transaction-posting-date
      :transaction-type transaction-type
      :transaction-code transaction-code
      :transaction-description transaction-description
      :transaction-desc transaction-desc
      :transaction-quantity transaction-quantity
      :transaction-amount transaction-amount
      :transaction-amount transaction-amount
      :department-code department-code
      :insurance-plan-id insurance-plan-id
      :insurance-amount insurance-amount
      :patient-location patient-location
      :fee-schedule fee-schedule
      :patient-type patient-type
      :diagnosis-code diagnosis-code
      :performed-by-code performed-by-code
      :ordered-by-code ordered-by-code
      :unit-cost unit-cost
      :filler-order-number filler-order-number
      :entered-by-code entered-by-code
      :procedure-code procedure-code
      :procedure-code-modifier procedure-code-modifier}]))

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
     {:set-id set-id
      :guarantor-number guarantor-number
      :guarantor-name guarantor-name
      :guarantor-spouse-name guarantor-spouse-name
      :guarantor-address guarantor-address
      :guarantor-phone guarantor-phone
      :guarantor-phone guarantor-phone
      :guarantor-date-of-birth guarantor-date-of-birth
      :guarantor-sex guarantor-sex
      :guarantor-type guarantor-type
      :guarantor-relationship guarantor-relationship
      :guarantor-ssn guarantor-ssn
      :guarantor-date guarantor-date
      :guarantor-date guarantor-date
      :guarantor-priority guarantor-priority
      :guarantor-employer-name guarantor-employer-name
      :guarantor-employer-addr guarantor-employer-addr
      :guarantor-employer-phone guarantor-employer-phone
      :guarantor-employee-id-# guarantor-employee-id-#
      :guarantor-employmt-status guarantor-employmt-status
      :guarantor-organization-name guarantor-organization-name
      :guarantor-billing-hold-flag guarantor-billing-hold-flag
      :guarantor-credit-rating-code guarantor-credit-rating-code
      :guarantor-death-date-and-time guarantor-death-date-and-time
      :guarantor-death-flag guarantor-death-flag
      :guarantor-charge-adjustment-code guarantor-charge-adjustment-code
      :guarantor-household-annual-income guarantor-household-annual-income
      :guarantor-household-size guarantor-household-size
      :guarantor-employer-id-number guarantor-employer-id-number
      :guarantor-marital-status-code guarantor-marital-status-code
      :guarantor-hire-effective-date guarantor-hire-effective-date
      :employment-stop-date employment-stop-date
      :living-dependency living-dependency
      :ambulatory-status ambulatory-status
      :citizenship citizenship
      :primary-language primary-language
      :living-arrangement living-arrangement
      :publicity-code publicity-code
      :protection-indicator protection-indicator
      :student-indicator student-indicator
      :religion religion
      :mother's-maiden-name mother's-maiden-name
      :nationality nationality
      :ethnic-group ethnic-group
      :contact-person's-name contact-person's-name
      :contact-person's-telephone-number contact-person's-telephone-number
      :contact-reason contact-reason
      :contact-relationship contact-relationship
      :job-title job-title
      :job-code-class job-code-class
      :guarantor-employer's-organization-name guarantor-employer's-organization-name
      :handicap handicap
      :job-status job-status
      :guarantor-financial-class guarantor-financial-class
      :guarantor-race guarantor-race}]))

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
     {:set-id set-id
      :insurance-plan-id insurance-plan-id
      :insurance-company-id insurance-company-id
      :insurance-company-name insurance-company-name
      :insurance-company-address insurance-company-address
      :insurance-co-contact-pers insurance-co-contact-pers
      :insurance-co-phone-number insurance-co-phone-number
      :group-number group-number
      :group-name group-name
      :insured's-group-emp insured's-group-emp
      :insured's-group-emp insured's-group-emp
      :plan-effective-date plan-effective-date
      :plan-expiration-date plan-expiration-date
      :authorization-information authorization-information
      :plan-type plan-type
      :name-of-insured name-of-insured
      :insured's-relation-to-pat insured's-relation-to-pat
      :insured's-date-of-birth insured's-date-of-birth
      :insured's-address insured's-address
      :assignment-of-benefits assignment-of-benefits
      :coordination-of-benefits coordination-of-benefits
      :coord coord
      :notice-of-admission-code notice-of-admission-code
      :notice-of-admission-date notice-of-admission-date
      :rpt-of-eligibility-code rpt-of-eligibility-code
      :rpt-of-eligibility-date rpt-of-eligibility-date
      :release-information-code release-information-code
      :pre pre
      :verification-date verification-date
      :verification-by verification-by
      :type-of-agreement-code type-of-agreement-code
      :billing-status billing-status
      :lifetime-reserve-days lifetime-reserve-days
      :delay-before-l delay-before-l
      :company-plan-code company-plan-code
      :policy-number policy-number
      :policy-deductible policy-deductible
      :policy-limit policy-limit
      :policy-limit policy-limit
      :room-rate room-rate
      :room-rate room-rate
      :insured's-employ-status insured's-employ-status
      :insured's-sex insured's-sex
      :insured's-employer-addr insured's-employer-addr
      :verification-status verification-status
      :prior-insurance-plan-id prior-insurance-plan-id
      :coverage-type coverage-type
      :handicap handicap
      :insured<92>s-id-number insured<92>s-id-number}]))

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
     {:insured’s-employee-id insured’s-employee-id
      :insured’s-social-security-number insured’s-social-security-number
      :insured’s-employer’s-name-and-id insured’s-employer’s-name-and-id
      :employer-information-data employer-information-data
      :mail-claim-party mail-claim-party
      :medicare-health-ins-card-number medicare-health-ins-card-number
      :medicaid-case-name medicaid-case-name
      :medicaid-case-number medicaid-case-number
      :military-sponsor-name military-sponsor-name
      :military-id-number military-id-number
      :dependent-of-military-recipient dependent-of-military-recipient
      :military-organization military-organization
      :military-station military-station
      :military-service military-service
      :military-rank-grade military-rank-grade
      :military-status military-status
      :military-retire-date military-retire-date
      :military-non military-non
      :baby-coverage baby-coverage
      :combine-baby-bill combine-baby-bill
      :blood-deductible blood-deductible
      :special-coverage-approval-name special-coverage-approval-name
      :special-coverage-approval-title special-coverage-approval-title
      :non non
      :payor-id payor-id
      :payor-subscriber-id payor-subscriber-id
      :eligibility-source eligibility-source
      :room-coverage-type-amount room-coverage-type-amount
      :policy-type-amount policy-type-amount
      :daily-deductible daily-deductible
      :living-dependency living-dependency
      :ambulatory-status ambulatory-status
      :citizenship citizenship
      :primary-language primary-language
      :living-arrangement living-arrangement
      :publicity-code publicity-code
      :protection-indicator protection-indicator
      :student-indicator student-indicator
      :religion religion
      :mother’s-maiden-name mother’s-maiden-name
      :nationality nationality
      :ethnic-group ethnic-group
      :marital-status marital-status
      :insured’s-employment-start-date insured’s-employment-start-date
      :employment-stop-date employment-stop-date
      :job-title job-title
      :job-code-class job-code-class
      :job-status job-status
      :employer-contact-person-name employer-contact-person-name
      :employer-contact-person-phone-number employer-contact-person-phone-number
      :employer-contact-reason employer-contact-reason
      :insured’s-contact-person’s-name insured’s-contact-person’s-name
      :insured’s-contact-person-phone-number insured’s-contact-person-phone-number
      :insured’s-contact-person-reason insured’s-contact-person-reason
      :relationship-to-the-patient-start-date relationship-to-the-patient-start-date
      :relationship-to-the-patient-stop-date relationship-to-the-patient-stop-date
      :insurance-co insurance-co
      :insurance-co-contact-phone-number insurance-co-contact-phone-number
      :policy-scope policy-scope
      :policy-source policy-source
      :patient-member-number patient-member-number
      :guarantor’s-relationship-to-insured guarantor’s-relationship-to-insured
      :insured’s-phone-number insured’s-phone-number
      :insured’s-employer-phone-number insured’s-employer-phone-number
      :military-handicapped-program military-handicapped-program
      :suspend-flag suspend-flag
      :copay-limit-flag copay-limit-flag
      :stoploss-limit-flag stoploss-limit-flag
      :insured-organization-name-and-id insured-organization-name-and-id
      :insured-employer-organization-name-and-id insured-employer-organization-name-and-id
      :race race
      :hcfa-patient’s-relationship-to-insured hcfa-patient’s-relationship-to-insured}]))

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
     {:set-id set-id
      :certification-number certification-number
      :certified-by certified-by
      :certification-required certification-required
      :penalty penalty
      :certification-date-time certification-date-time
      :certification-modify-date-time certification-modify-date-time
      :operator operator
      :certification-begin-date certification-begin-date
      :certification-end-date certification-end-date
      :days days
      :non non
      :non non
      :physician-reviewer physician-reviewer
      :certification-contact certification-contact
      :certification-contact-phone-number certification-contact-phone-number
      :appeal-reason appeal-reason
      :certification-agency certification-agency
      :certification-agency-phone-number certification-agency-phone-number
      :pre pre
      :case-manager case-manager
      :second-opinion-date second-opinion-date
      :second-opinion-status second-opinion-status
      :second-opinion-documentation-received second-opinion-documentation-received
      :second-opinion-physician second-opinion-physician}]))

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
     {:set-id set-id
      :procedure-coding-method procedure-coding-method
      :procedure-code procedure-code
      :procedure-description procedure-description
      :procedure-date-time procedure-date-time
      :procedure-type procedure-type
      :procedure-minutes procedure-minutes
      :anesthesiologist anesthesiologist
      :anesthesia-code anesthesia-code
      :anesthesia-minutes anesthesia-minutes
      :surgeon surgeon
      :resident-code resident-code
      :consent-code consent-code}]))

;; Error
(defmethod parse-segment "ERR" [segment]
  (let [[[error-code-and-location]] (->> segment :fields (map :content))]
    [:ERR
     {:error-code-and-location error-code-and-location}]))

;; Notes and Comments
(defmethod parse-segment "NTE" [segment]
  (let [[[set-id]
         [source-of-comment]
         [comment]
         [comment-type]] (->> segment :fields (map :content))]
    [:NTE
     {:set-id set-id
      :source-of-comment source-of-comment
      :comment comment
      :comment-type comment-type}]))

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
     {:placer-appointment-id placer-appointment-id
      :filler-appointment-id filler-appointment-id
      :occurrence-number occurrence-number
      :placer-group-number placer-group-number
      :schedule-id schedule-id
      :event-reason event-reason
      :appointment-reason appointment-reason
      :appointment-type appointment-type
      :appointment-duration appointment-duration
      :appointment-duration-units appointment-duration-units
      :appointment-timing-quantity appointment-timing-quantity
      :placer-contact-person placer-contact-person
      :placer-contact-phone-number placer-contact-phone-number
      :placer-contact-address placer-contact-address
      :placer-contact-location placer-contact-location
      :filler-contact-person filler-contact-person
      :filler-contact-phone-number filler-contact-phone-number
      :filler-contact-address filler-contact-address
      :filler-contact-location filler-contact-location
      :entered-by-person entered-by-person
      :entered-by-phone-number entered-by-phone-number
      :entered-by-location entered-by-location
      :parent-placer-appointment-id parent-placer-appointment-id
      :parent-filler-appointment-id parent-filler-appointment-id
      :filler-status-code filler-status-code}]))

;; Resource Group
(defmethod parse-segment "RGS" [segment]
  (let [[[set-id]
         [segment-action-code]
         [resource-group-id]] (->> segment :fields (map :content))]
    [:RGS
     {:set-id set-id
      :segment-action-code segment-action-code
      :resource-group-id resource-group-id}]))

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
     {:set-id set-id
      :segment-action-code segment-action-code
      :location-resource-id location-resource-id
      :location-type location-type
      :location-group location-group
      :start-date-time start-date-time
      :start-date-time-offset start-date-time-offset
      :start-date-time-offset-units start-date-time-offset-units
      :duration duration
      :duration-units duration-units
      :allow-substitution-code allow-substitution-code
      :filler-status-code filler-status-code}]))

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
     {:set-id set-id
      :segment-action-code segment-action-code
      :personnel-resource-id personnel-resource-id
      :resource-role resource-role
      :resource-group resource-group
      :start-date-time start-date-time
      :start-date-time-offset start-date-time-offset
      :start-date-time-offset-units start-date-time-offset-units
      :duration duration
      :duration-units duration-units
      :allow-substitution-code allow-substitution-code
      :filler-status-code filler-status-code}]))

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
     {:set-id set-id
      :segment-action-code segment-action-code
      :resource-id resource-id
      :resource-type resource-type
      :resource-group resource-group
      :resource-quantity resource-quantity
      :resource-quantity-units resource-quantity-units
      :start-date-time start-date-time
      :start-date-time-offset start-date-time-offset
      :start-date-time-offset-units start-date-time-offset-units
      :duration duration
      :duration-units duration-units
      :allow-substitution-code allow-substitution-code
      :filler-status-code filler-status-code}]))

;; Accident
(defmethod parse-segment "ACC" [segment]
  (let [[[accident-date-time]
         [accident-code]
         [accident-location]
         [auto-accident-state]
         [accident-job-related-indicator]
         [accident-death-indicator]] (->> segment :fields (map :content))]
    [:ACC
     {:accident-date-time accident-date-time
      :accident-code accident-code
      :accident-location accident-location
      :auto-accident-state auto-accident-state
      :accident-job-related-indicator accident-job-related-indicator
      :accident-death-indicator accident-death-indicator}]))

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
     {:set-id set-id
      :blood-deductible blood-deductible
      :blood-furnished blood-furnished
      :blood-replaced blood-replaced
      :blood-not-replaced blood-not-replaced
      :co co
      :condition-code condition-code
      :covered-days-- covered-days--
      :non-covered-days non-covered-days
      :value-amount value-amount
      :number-of-grace-days number-of-grace-days
      :special-program-indicator special-program-indicator
      :psro-ur-approval-indicator psro-ur-approval-indicator
      :psro-ur-approved-stay psro-ur-approved-stay
      :psro-ur-approved-stay psro-ur-approved-stay
      :occurrence occurrence
      :occurrence-span occurrence-span
      :span-start-date span-start-date
      :span-end-date span-end-date
      :ub ub
      :ub ub
      :ub ub
      :ub ub}]))

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
     {:set-id set-id
      :co co
      :condition-code condition-code
      :covered-days covered-days
      :non non
      :value-amount value-amount
      :occurrence-code occurrence-code
      :occurrence-span-code-dates occurrence-span-code-dates
      :ub92-locator-2 ub92-locator-2
      :ub92-locator-11 ub92-locator-11
      :ub92-locator-31 ub92-locator-31
      :document-control-number document-control-number
      :ub92-locator-49 ub92-locator-49
      :ub92-locator-56 ub92-locator-56
      :ub92-locator-57 ub92-locator-57
      :ub92-locator-78 ub92-locator-78
      :special-visit-count special-visit-count}]))

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
     {:set-id set-id
      :name name
      :relationship relationship
      :address address
      :phone-number phone-number
      :business-phone-number business-phone-number
      :contact-role contact-role
      :start-date start-date
      :end-date end-date
      :next-of-kin-associated-parties-job-title next-of-kin-associated-parties-job-title
      :next-of-kin-associated-parties-jobcode-class next-of-kin-associated-parties-jobcode-class
      :next-of-kin-associated-parties-employeenumber next-of-kin-associated-parties-employeenumber
      :organization-name organization-name
      :marital-status marital-status
      :sex sex
      :date-time-of-birth date-time-of-birth
      :living-dependency living-dependency
      :ambulatory-status ambulatory-status
      :citizenship citizenship
      :primary-language primary-language
      :living-arrangement living-arrangement
      :publicity-code publicity-code
      :protection-indicator protection-indicator
      :student-indicator student-indicator
      :religion religion
      :mother's-maiden-name mother's-maiden-name
      :nationality nationality
      :ethnic-group ethnic-group
      :contact-reason contact-reason
      :contact-person's-name contact-person's-name
      :contact-person's-telephone-number contact-person's-telephone-number
      :contact-person's-address contact-person's-address
      :next-of-kin-associated-party's-identifiers next-of-kin-associated-party's-identifiers
      :job-status job-status
      :race race
      :handicap handicap
      :contact-person-social-security-number contact-person-social-security-number}]))

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
     {:order-control-code order-control-code
      :placer-order-number placer-order-number
      :filler-order-number filler-order-number
      :placer-group-number placer-group-number
      :order-status order-status
      :response-flag response-flag
      :quantity-timing quantity-timing
      :parent parent
      :transaction-date-time transaction-date-time
      :entered-by entered-by
      :verified-by verified-by
      :ordering-provider ordering-provider
      :enterer's-location enterer's-location
      :call-back-phone-number call-back-phone-number
      :order-effective-date-time order-effective-date-time
      :order-control-code-reason order-control-code-reason
      :entering-organization entering-organization
      :entering-device entering-device
      :action-by action-by
      :advanced-beneficiary-notice-code advanced-beneficiary-notice-code
      :ordering-facility-name ordering-facility-name
      :ordering-facility-address ordering-facility-address
      :ordering-facility-phone-number ordering-facility-phone-number
      :ordering-provider-address ordering-provider-address}]))

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
     {:set-id set-id
      :placer-order-number placer-order-number
      :filler-order-number filler-order-number
      :universal-service-id universal-service-id
      :priority priority
      :requested-date-time requested-date-time
      :observation-date-time observation-date-time
      :observation-end-date-time observation-end-date-time
      :collection-volume collection-volume
      :collector-identifier collector-identifier
      :specimen-action-code specimen-action-code
      :danger-code danger-code
      :relevant-clinical-info relevant-clinical-info
      :specimen-received-date-time specimen-received-date-time
      :specimen-source specimen-source
      :ordering-provider ordering-provider
      :order-callback-phone-number order-callback-phone-number
      :placer-field-1 placer-field-1
      :placer-field-2 placer-field-2
      :filler-field-1 filler-field-1
      :filler-field-2 filler-field-2
      :results-rpt-change-date-time results-rpt-change-date-time
      :charge-to-practice charge-to-practice
      :diagnostice-serv-sect-id diagnostice-serv-sect-id
      :result-status result-status
      :parent-result parent-result
      :quantity-timing quantity-timing
      :result-copies-to result-copies-to
      :parent parent
      :transportation-mode transportation-mode
      :reason-for-study reason-for-study
      :principal-result-interpreter principal-result-interpreter
      :assistant-result-interpreter assistant-result-interpreter
      :technician technician
      :transcriptionist transcriptionist
      :scheduled-date-time scheduled-date-time
      :number-of-sample-containers number-of-sample-containers
      :transport-logistics-of-collected-samples transport-logistics-of-collected-samples
      :collector's-comment collector's-comment
      :transport-arrangement-responsibility transport-arrangement-responsibility
      :transport-arranged transport-arranged
      :escort-required escort-required
      :planned-patient-transport-comment planned-patient-transport-comment
      :procedure-code procedure-code
      :procedure-code-modifier procedure-code-modifier}]))

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
     {:set-id set-id
      :value-type value-type
      :observation-identifier observation-identifier
      :observation-sub observation-sub
      :observation-value observation-value
      :units units
      :reference-range reference-range
      :abnormal-flags abnormal-flags
      :probability probability
      :nature-of-abnormal-test nature-of-abnormal-test
      :observation-result-status observation-result-status
      :date-last-obs-normal-value date-last-obs-normal-value
      :user-defined-access-checks user-defined-access-checks
      :date-time-of-the-observation date-time-of-the-observation
      :producer's-id producer's-id
      :responsible-observer responsible-observer
      :observation-method observation-method}]))

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
     {:give-sub give-sub
      :administration-sub administration-sub
      :date-time-start-of-administration date-time-start-of-administration
      :date-time-end-of-administration date-time-end-of-administration
      :administered-code administered-code
      :administered-amount administered-amount
      :administered-units administered-units
      :administered-dosage-form administered-dosage-form
      :administration-notes administration-notes
      :administering-provider administering-provider
      :administered administered
      :administered-per administered-per
      :administered-strength administered-strength
      :administered-strength-units administered-strength-units
      :substance-lot-number substance-lot-number
      :substance-expiration-date substance-expiration-date
      :substance-manufacturer-name substance-manufacturer-name
      :substance-refusal-reason substance-refusal-reason
      :indication indication
      :completion-status completion-status
      :action-code action-code
      :system-entry-date-time system-entry-date-time}]))

;; Pharmacy/Treatment Route
(defmethod parse-segment "RXR" [segment]
  (let [[[route]
         [site]
         [administration-device]
         [administration-method]
         [routing-instruction]] (->> segment :fields (map :content))]
    [:RXR
     {:route route
      :site site
      :administration-device administration-device
      :administration-method administration-method
      :routing-instruction routing-instruction}]))

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
     {:set-id set-id
      :document-type document-type
      :document-content-presentation document-content-presentation
      :activity-date-time activity-date-time
      :primary-activity-provider-code-name primary-activity-provider-code-name
      :origination-date-time origination-date-time
      :transcription-date-time transcription-date-time
      :edit-date-time edit-date-time
      :originator-code-name originator-code-name
      :assigned-document-authenticator assigned-document-authenticator
      :transcriptionist-code-name transcriptionist-code-name
      :unique-document-number unique-document-number
      :parent-document-number parent-document-number
      :placer-order-number placer-order-number
      :filler-order-number filler-order-number
      :unique-document-file-name unique-document-file-name
      :document-completion-status document-completion-status
      :document-confidentiality-status document-confidentiality-status
      :document-availability-status document-availability-status
      :document-storage-status document-storage-status
      :document-change-reason document-change-reason
      :authentication-person authentication-person
      :distributed-copies distributed-copies}]))

;; Query Acknowledgement
(defmethod parse-segment "QAK" [segment]
  (let [[[query-tag]
         [event-identifier]
         [input-parameter-list]] (->> segment :fields (map :content))]
    [:QAK
     {:query-tag query-tag
      :event-identifier event-identifier
      :input-parameter-list input-parameter-list}]))

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
     {:query-date-time query-date-time
      :query-format-code query-format-code
      :query-priority query-priority
      :query-id query-id
      :deferred-response-type deferred-response-type
      :deferred-response-date-time deferred-response-date-time
      :quantity-limited-request quantity-limited-request
      :who-subject-filter who-subject-filter
      :what-subject-filter what-subject-filter
      :what-department-data-code what-department-data-code
      :what-data-code-value-qual what-data-code-value-qual
      :query-results-level query-results-level}]))

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
     {:where-subject-filter where-subject-filter
      :when-data-start-date-time when-data-start-date-time
      :when-data-end-date-time when-data-end-date-time
      :what-user-qualifier what-user-qualifier
      :other-qry-subject-filter other-qry-subject-filter
      :which-date-time-qualifier which-date-time-qualifier
      :which-date-time-status-qualifier which-date-time-status-qualifier
      :date-time-selection-qualifier date-time-selection-qualifier
      :when-quantity-timing-qualifier when-quantity-timing-qualifier}]))


(defn parse-message [message]
  (->> message
       :segments
       (map parse-segment)
       (into {})))
