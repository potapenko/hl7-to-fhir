(ns aidbox.orm
  (:require [aidbox.messages :as messages]
            [clj-time.format :as f]
            [clojure.pprint :refer [pprint]]
            [com.nervestaple.hl7-parser.parser :as hl7]))

(def hl7-time-format (f/formatter "yyyyMMddHHmmSS"))

(defmulti parse-segment :id)

;; Message Header
(defmethod parse-segment "MSH" [segment]
  (let [[[field-separator] ;; Field Separator (:ST)
         [sending-application] ;; Sending Application (:ST)
         [sending-facility] ;; Sending Facility (:ST)
         [receiving-application] ;; Receiving Application (:ST)
         [receiving-facility] ;; Receiving Facility (:ST)
         [date-time-of-message] ;; Date/Time of Message (:TS)
         [security] ;; Security (:ST)
         [message-type] ;; Message Type (:ID)
         [message-control-id] ;; Message Control ID (:ST)
         [processing-id] ;; Processing ID (:ID)
         [version-id] ;; Version ID (:NM)
         [sequence-number] ;; Sequence Number (:NM)
         [continuation-pointer] ;; Continuation Pointer (:ST)
         [accept-acknowledgment-type] ;; Accept Acknowledgment Type (:ID)
         [application-acknowledgment-type] ;; Application Acknowledgment Type (:ID)
         [country-code] ;; Country Code (:ID)
         [character-set] ;; Character Set (:ID)
           ](->> segment :fields (map :content))]
    [:MSH
     {:field-separator field-separator
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
  (let [[[master-file-identifier] ;; Master File Identifier (:CE)
         [master-file-application-identifier] ;; Master File Application Identifier (:HD)
         [file] ;; File-Level Event Code (:ID)
         [entered-date-time] ;; Entered Date/Time (:TS)
         [effective-date-time] ;; Effective Date/Time (:TS)
         [response-level-code] ;; Response Level Code (:ID)
           ](->> segment :fields (map :content))]
    [:MFI
     {:master-file-identifier master-file-identifier
      :master-file-application-identifier master-file-application-identifier
      :file file
      :entered-date-time entered-date-time
      :effective-date-time effective-date-time
      :response-level-code response-level-code}]))

;; Master File Entry
(defmethod parse-segment "MFE" [segment]
  (let [[[record] ;; Record-Level Event Code (:ID)
         [mfn-control-id] ;; MFN Control ID (:ST)
         [effective-date-time] ;; Effective Date/Time (:TS)
         [primary-key-value] ;; Primary Key Value - MFE (:FT)
         [primary-key-value-type] ;; Primary Key Value Type (:ID)
           ](->> segment :fields (map :content))]
    [:MFE
     {:record record
      :mfn-control-id mfn-control-id
      :effective-date-time effective-date-time
      :primary-key-value primary-key-value
      :primary-key-value-type primary-key-value-type}]))

;; Message Acknowledgement
(defmethod parse-segment "MSA" [segment]
  (let [[[acknowledgement-code] ;; Acknowledgement Code (:ID)
         [message-control-id] ;; Message Control ID (:ST)
         [text-message] ;; Text Message (:ST)
         [expected-sequence-number] ;; Expected Sequence Number (:NM)
         [delayed-ack-type] ;; Delayed Ack Type (:ID)
           ](->> segment :fields (map :content))]
    [:MSA
     {:acknowledgement-code acknowledgement-code
      :message-control-id message-control-id
      :text-message text-message
      :expected-sequence-number expected-sequence-number
      :delayed-ack-type delayed-ack-type}]))

;; Event Type
(defmethod parse-segment "EVN" [segment]
  (let [[[invalid-string] ;;  (:ID)
         [date-time-of-event] ;; Date/Time of Event (:TS)
         [date-time-planned-event] ;; Date/Time Planned Event (:TS)
         [event-reason-code] ;; Event Reason Code (:ID)
           ](->> segment :fields (map :content))]
    [:EVN
     {:invalid-string invalid-string
      :date-time-of-event date-time-of-event
      :date-time-planned-event date-time-planned-event
      :event-reason-code event-reason-code}]))

;; Merge Patient Information
(defmethod parse-segment "MRG" [segment]
  (let [[[prior-patient-identifier-list] ;; Prior Patient Identifier List (:CX)
         [prior-alternate-patient-id] ;; Prior Alternate Patient ID (:CX)
         [prior-patient-account-number] ;; Prior Patient Account Number (:CX)
         [prior-patient-id] ;; Prior Patient ID (:CX)
         [prior-visit-number] ;; Prior Visit Number (:CX)
         [prior-alternate-visit-id] ;; Prior Alternate Visit ID (:CX)
         [prior-patient-name] ;; Prior Patient Name (:XPN)
           ](->> segment :fields (map :content))]
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
  (let [[[set-id] ;; Set ID - PID (:SI)
         [patient-id] ;; Patient ID (:CX)
         [patient-identifier-list] ;; Patient Identifier List (:CX)
         [alternate-patient-id] ;; Alternate Patient ID - PID (:CX)
         [patient-name] ;; Patient Name (:XPN)
         [mother’s-maiden-name] ;; Mother’s Maiden Name (:XPN)
         [date-time-of-birth] ;; Date/Time of Birth (:TS)
         [sex] ;; Sex (:IS)
         [patient-alias] ;; Patient Alias (:XPN)
         [race] ;; Race (:CE)
         [patient-address] ;; Patient Address (:XAD)
         [county-code] ;; County Code (:IS)
         [phone-number] ;; Phone Number - Home (:XTN)
         [phone-number-2] ;; Phone Number - Business (:XTN)
         [primary-language] ;; Primary Language (:CE)
         [marital-status] ;; Marital Status (:CE)
         [religion] ;; Religion (:CE)
         [patient-account-number] ;; Patient Account Number (:CX)
         [ssn-number] ;; SSN Number - Patient (not used) (:ST)
         [driver's-license-number] ;; Driver's License Number - Patient (not used) (:DLN)
         [mother's-identifier] ;; Mother's Identifier (:CX)
         [ethnic-group] ;; Ethnic Group (:CE)
         [birth-place] ;; Birth Place (:ST)
         [multiple-birth-indicator] ;; Multiple Birth Indicator (:ID)
         [birth-order] ;; Birth Order (:NM)
         [citizenship] ;; Citizenship (:CE)
         [veterans-military-status] ;; Veterans Military Status (:CE)
         [nationality] ;; Nationality (:CE)
         [patient-death-date-and-time] ;; Patient Death Date and Time (:TS)
         [patient-death-indicator] ;; Patient Death Indicator (:ID)
         [identity-unknown-indicator] ;; Identity Unknown Indicator (:ID)
         [identity-reliability-code] ;; Identity Reliability Code (:IS)
         [last-update-date-time] ;; Last Update Date/Time (:TS)
         [last-update-facility] ;; Last Update Facility (:HD)
         [species-code] ;; Species Code (:CE)
         [breed-code] ;; Breed Code (:CE)
         [strain] ;; Strain (:ST)
         [production-class-code] ;; Production Class Code (:CE)
         [tribal-citizenship] ;; Tribal Citizenship (:CWE)
           ](->> segment :fields (map :content))]
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
      :phone-number-2 phone-number-2
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
  (let [[[living-dependency] ;; Living Dependency (:IS)
         [living-arrangement] ;; Living Arrangement (:IS)
         [patient-primary-facility] ;; Patient Primary Facility (:XON)
         [patient-primary-care-provider-name] ;; Patient Primary Care Provider Name & ID No. (:XCN)
         [student-indicator] ;; Student Indicator (:IS)
         [handicap] ;; Handicap (:IS)
         [living-will] ;; Living Will (:IS)
         [organ-donor] ;; Organ Donor (:IS)
         [separate-bill] ;; Separate Bill (:ID)
         [duplicate-patient] ;; Duplicate Patient (:CX)
         [publicity-code] ;; Publicity Code (:CE)
         [protection-indicator] ;; Protection Indicator (:ID)
           ](->> segment :fields (map :content))]
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
  (let [[[set-id] ;; Set Id (:SI)
         [patient-class] ;; Patient Class (:ID)
         [assigned-patient-location] ;; Assigned Patient Location (:PL)
         [admission-type] ;; Admission Type (:IS)
         [pre] ;; Pre-Admit Number (:CX)
         [prior-patient-location] ;; Prior Patient Location (:PL)
         [attending-doctor] ;; Attending Doctor (:XCN)
         [refering-doctor] ;; Refering Doctor (:XCN)
         [consulting-doctor] ;; Consulting Doctor (use ROL segment) (:XCN)
         [hospital-service] ;; Hospital Service (:IS)
         [temporary-location] ;; Temporary Location (:PL)
         [pre-2] ;; Pre-Admit Test Indicator (:IS)
         [re] ;; Re-Admission Indicator (:IS)
         [admit-source] ;; Admit Source (:IS)
         [ambulatory-status] ;; Ambulatory Status (:IS)
         [vip-indicators] ;; VIP Indicators (:IS)
         [admitting-doctor] ;; Admitting Doctor (:XCN)
         [patient-type] ;; Patient Type (:IS)
         [visit-number] ;; Visit Number (:CX)
         [financial-class] ;; Financial Class (:FC)
         [charge-price-indicator] ;; Charge Price Indicator (:IS)
         [courtesy-code] ;; Courtesy Code (:IS)
         [credit-rating] ;; Credit Rating (:IS)
         [contract-code] ;; Contract Code (:IS)
         [contract-effective-date] ;; Contract Effective Date (:DT)
         [contract-amount] ;; Contract Amount (:NM)
         [contract-period] ;; Contract Period (:NM)
         [interest-code] ;; Interest Code (:IS)
         [transfer-to-bad-debt-code] ;; Transfer to Bad Debt Code (:IS)
         [transfer-to-bad-debt-date] ;; Transfer to Bad Debt Date (:DT)
         [bad-debt-agency-code] ;; Bad Debt Agency Code (:IS)
         [bad-debt-transfer-amount] ;; Bad Debt Transfer Amount (:NM)
         [bad-debt-recovery-amount] ;; Bad Debt Recovery Amount (:NM)
         [delete-account-indicator] ;; Delete Account Indicator (:IS)
         [delete-account-date] ;; Delete Account Date (:DT)
         [discharge-disposition] ;; Discharge Disposition (:IS)
         [discharged-to-location] ;; Discharged to Location (:DLD)
         [diet-type] ;; Diet Type (:CE)
         [servicing-facility] ;; Servicing Facility (:IS)
         [bed-status] ;; Bed Status (not used) (:IS)
         [account-status] ;; Account Status (:IS)
         [pending-location] ;; Pending Location (:PL)
         [prior-temporary-location] ;; Prior Temporary Location (:PL)
         [admit-date-time] ;; Admit Date/Time (:TS)
         [discharge-date-time] ;; Discharge Date/Time (:TS)
         [current-patient-balance] ;; Current Patient Balance (:NM)
         [total-charges] ;; Total Charges (:NM)
         [total-adjustments] ;; Total Adjustments (:NM)
         [total-payments] ;; Total Payments (:NM)
         [alternate-visit-id] ;; Alternate Visit ID (:CX)
         [visit-indicator] ;; Visit Indicator (:IS)
         [other-healthcare-provider] ;; Other Healthcare Provider (:XCN)
           ](->> segment :fields (map :content))]
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
      :pre-2 pre-2
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
  (let [[[prior-pending-location] ;; Prior Pending Location (:PL)
         [accommodation-code] ;; Accommodation Code (:CE)
         [admit-reason] ;; Admit Reason (:CE)
         [transfer-reason] ;; Transfer Reason (:CE)
         [patient-valuables] ;; Patient Valuables (:ST)
         [patient-valuables-location] ;; Patient Valuables Location (:ST)
         [visit-user-code] ;; Visit User Code (:IS)
         [expected-admit-date-time] ;; Expected Admit Date/Time (:TS)
         [expected-discharge-date-time] ;; Expected Discharge Date/Time (:TS)
         [estimated-length-of-inpatient-stay] ;; Estimated Length of Inpatient Stay (:NM)
         [actual-length-of-inpatient-stay] ;; Actual Length of Inpatient Stay (:NM)
         [visit-description] ;; Visit Description (:ST)
         [referral-source-code] ;; Referral Source Code (:XCN)
         [previous-service-date] ;; Previous Service Date (:DT)
         [employment-illness-related-indicator] ;; Employment Illness Related Indicator (:ID)
         [purge-status-code] ;; Purge Status Code (:IS)
         [purge-status-date] ;; Purge Status Date (:DT)
         [special-program-code] ;; Special Program Code (:IS)
         [retention-indicator] ;; Retention Indicator (:ID)
         [expected-number-of-insurance-plans] ;; Expected Number of Insurance Plans (:NM)
         [visit-publicity-code] ;; Visit Publicity Code (:IS)
         [visit-protection-indicator] ;; Visit Protection Indicator (:ID)
         [clinic-organization-name] ;; Clinic Organization Name (:XON)
         [patient-status-code] ;; Patient Status Code (:IS)
         [visit-priority-code] ;; Visit Priority Code (:IS)
         [previous-treatment-date] ;; Previous Treatment Date (:DT)
         [expected-discharge-disposition] ;; Expected Discharge Disposition (:IS)
         [signature-on-file-date] ;; Signature on File Date (:DT)
         [first-similar-illness-date] ;; First Similar Illness Date (:DT)
         [patient-charge-adjustment-code] ;; Patient Charge Adjustment Code (:CE)
         [recurring-service-code] ;; Recurring Service Code (:IS)
         [billing-media-code] ;; Billing Media Code (:ID)
         [expected-surgery-date] ;; Expected Surgery Date & Time (:TS)
         [military-partnership-code] ;; Military Partnership Code (:ID)
         [military-non] ;; Military Non-Availability Code (:ID)
         [newborn-baby-indicator] ;; Newborn Baby Indicator (:ID)
         [baby-detained-indicator] ;; Baby Detained Indicator (:ID)
           ](->> segment :fields (map :content))]
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
  (let [[[set-id] ;; Set ID - Diagnosis (:SI)
         [diagnosis-coding-method] ;; Diagnosis Coding Method (:ID)
         [diagnosis-code] ;; Diagnosis Code (:ID)
         [diagnosis-description] ;; Diagnosis Description (:ST)
         [diagnosis-date-time] ;; Diagnosis Date/Time (:TS)
         [diagnosis-drg-type] ;; Diagnosis/DRG Type (:ID)
         [major-diagnostic-category] ;; Major Diagnostic Category (:ST)
         [diagnostic-related-group] ;; Diagnostic Related Group (:ID)
         [drg-approval-indicator] ;; DRG Approval Indicator (:ID)
         [drg-grouper-review-code] ;; DRG Grouper Review Code (:ID)
         [outlier-type] ;; Outlier Type (:ID)
         [outlier-days] ;; Outlier Days (:NM)
         [outlier-cost] ;; Outlier Cost (:NM)
         [grouper-version-and-type] ;; Grouper Version and Type (:ST)
         [diagnosis-drg-priority] ;; Diagnosis/DRG priority (:NM)
         [diagnosing-clinician] ;; Diagnosing clinician (:TX)
           ](->> segment :fields (map :content))]
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
  (let [[[set-id] ;; 1 Set ID - Financial Trans (:SI)
         [transaction-id] ;; 2 Transaction ID (:ST)
         [transaction-batch-id] ;; 3 Transaction Batch ID (:ST)
         [transaction-date] ;; 4 Transaction Date (:DT)
         [transaction-posting-date] ;; 5 Transaction Posting Date (:DT)
         [transaction-type] ;; 6 Transaction Type (:ID)
         [transaction-code] ;; 7 Transaction Code (:ID)
         [transaction-description] ;; 8 Transaction Description (:ST)
         [transaction-desc] ;; 9 Transaction Desc. - Alt (:ST)
         [transaction-quantity] ;; 10 Transaction Quantity (:NM)
         [transaction-amount] ;; 11 Transaction Amount - Ext. (:NM)
         [transaction-amount-2] ;; 12 Transaction Amount - Unit (:NM)
         [department-code] ;; 13 Department Code (:ST)
         [insurance-plan-id] ;; 14 Insurance Plan ID (:ID)
         [insurance-amount] ;; 15 Insurance Amount (:NM)
         [patient-location] ;; 16 Patient Location (:ST)
         [fee-schedule] ;; 17 Fee Schedule (:ID)
         [patient-type] ;; 18 Patient Type (:ID)
         [diagnosis-code] ;; 19 Diagnosis Code (:ID)
         [performed-by-code] ;; 20 Performed by Code (:CN)
         [ordered-by-code] ;; 21 Ordered by Code (:CN)
         [unit-cost] ;; 22 Unit Cost (:NM)
         [filler-order-number] ;; 23 Filler Order Number (:EI)
         [entered-by-code] ;; 24 Entered By Code (:XCN)
         [procedure-code] ;; 25 Procedure Code (:CE)
         [procedure-code-modifier] ;; 26 Procedure Code Modifier (:CE)
           ](->> segment :fields (map :content))]
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
      :transaction-amount-2 transaction-amount-2
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
  (let [[[set-id] ;; Set ID - Guarantor (:SI)
         [guarantor-number] ;; Guarantor Number (:ID)
         [guarantor-name] ;; Guarantor Name (:PN)
         [guarantor-spouse-name] ;; Guarantor Spouse Name (:PN)
         [guarantor-address] ;; Guarantor Address (:AD)
         [guarantor-phone] ;; Guarantor Phone - Home (:TN)
         [guarantor-phone-2] ;; Guarantor Phone - Bus. (:TN)
         [guarantor-date-of-birth] ;; Guarantor Date of Birth (:DT)
         [guarantor-sex] ;; Guarantor Sex (:ID)
         [guarantor-type] ;; Guarantor Type (:ID)
         [guarantor-relationship] ;; Guarantor Relationship (:ID)
         [guarantor-ssn] ;; Guarantor SSN (:ST)
         [guarantor-date] ;; Guarantor Date - Begin (:DT)
         [guarantor-date-2] ;; Guarantor Date - End (:DT)
         [guarantor-priority] ;; Guarantor Priority (:NM)
         [guarantor-employer-name] ;; Guarantor Employer Name (:ST)
         [guarantor-employer-addr] ;; Guarantor Employer Addr (:AD)
         [guarantor-employer-phone] ;; Guarantor Employer Phone (:TN)
         [guarantor-employee-id-#] ;; Guarantor Employee ID # (:ST)
         [guarantor-employmt-status] ;; Guarantor Employmt Status (:ID)
         [guarantor-organization-name] ;; Guarantor Organization Name (:XON)
         [guarantor-billing-hold-flag] ;; Guarantor Billing Hold Flag (:ID)
         [guarantor-credit-rating-code] ;; Guarantor Credit Rating Code (:CE)
         [guarantor-death-date-and-time] ;; Guarantor Death Date And Time (:TS)
         [guarantor-death-flag] ;; Guarantor Death Flag (:ID)
         [guarantor-charge-adjustment-code] ;; Guarantor Charge Adjustment Code (:CE)
         [guarantor-household-annual-income] ;; Guarantor Household Annual Income (:CP)
         [guarantor-household-size] ;; Guarantor Household Size (:NM)
         [guarantor-employer-id-number] ;; Guarantor Employer ID Number (:CX)
         [guarantor-marital-status-code] ;; Guarantor Marital Status Code (:CE)
         [guarantor-hire-effective-date] ;; Guarantor Hire Effective Date (:DT)
         [employment-stop-date] ;; Employment Stop Date (:DT)
         [living-dependency] ;; Living Dependency (:IS)
         [ambulatory-status] ;; Ambulatory Status (:IS)
         [citizenship] ;; Citizenship (:CE)
         [primary-language] ;; Primary Language (:CE)
         [living-arrangement] ;; Living Arrangement (:IS)
         [publicity-code] ;; Publicity Code (:CE)
         [protection-indicator] ;; Protection Indicator (:ID)
         [student-indicator] ;; Student Indicator (:IS)
         [religion] ;; Religion (:CE)
         [mother's-maiden-name] ;; Mother's Maiden Name (:XPN)
         [nationality] ;; Nationality (:CE)
         [ethnic-group] ;; Ethnic Group (:CE)
         [contact-person's-name] ;; Contact Person's Name (:XPN)
         [contact-person's-telephone-number] ;; Contact Person's Telephone Number (:XTN)
         [contact-reason] ;; Contact Reason (:CE)
         [contact-relationship] ;; Contact Relationship (:IS)
         [job-title] ;; Job Title (:ST)
         [job-code-class] ;; Job Code/Class (:JCC)
         [guarantor-employer's-organization-name] ;; Guarantor Employer's Organization Name (:XON)
         [handicap] ;; Handicap (:IS)
         [job-status] ;; Job Status (:IS)
         [guarantor-financial-class] ;; Guarantor Financial Class (:FC)
         [guarantor-race] ;; Guarantor Race (:CE)
           ](->> segment :fields (map :content))]
    [:GT1
     {:set-id set-id
      :guarantor-number guarantor-number
      :guarantor-name guarantor-name
      :guarantor-spouse-name guarantor-spouse-name
      :guarantor-address guarantor-address
      :guarantor-phone guarantor-phone
      :guarantor-phone-2 guarantor-phone-2
      :guarantor-date-of-birth guarantor-date-of-birth
      :guarantor-sex guarantor-sex
      :guarantor-type guarantor-type
      :guarantor-relationship guarantor-relationship
      :guarantor-ssn guarantor-ssn
      :guarantor-date guarantor-date
      :guarantor-date-2 guarantor-date-2
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
  (let [[[set-id] ;; Set ID - Insurance (:SI)
         [insurance-plan-id] ;; Insurance Plan ID (:ID)
         [insurance-company-id] ;; Insurance Company ID (:ST)
         [insurance-company-name] ;; Insurance Company Name (:ST)
         [insurance-company-address] ;; Insurance Company Address (:AD)
         [insurance-co-contact-pers] ;; Insurance Co Contact Pers (:PN)
         [insurance-co-phone-number] ;; Insurance Co Phone Number (:TN)
         [group-number] ;; Group Number (:ST)
         [group-name] ;; Group Name (:ST)
         [insured's-group-emp] ;; Insured's Group Emp. ID (:ST)
         [insured's-group-emp-2] ;; Insured's Group Emp. Name (:ST)
         [plan-effective-date] ;; Plan Effective Date (:DT)
         [plan-expiration-date] ;; Plan Expiration Date (:DT)
         [authorization-information] ;; Authorization Information (:ST)
         [plan-type] ;; Plan Type (:ID)
         [name-of-insured] ;; Name of Insured (:PN)
         [insured's-relation-to-pat] ;; Insured's Relation to Pat (:ID)
         [insured's-date-of-birth] ;; Insured's Date of Birth (:DT)
         [insured's-address] ;; Insured's Address (:AD)
         [assignment-of-benefits] ;; Assignment of Benefits (:ID)
         [coordination-of-benefits] ;; Coordination of Benefits (:ID)
         [coord] ;; Coord. of Ben. Priority (:ST)
         [notice-of-admission-code] ;; Notice of Admission Code (:ID)
         [notice-of-admission-date] ;; Notice of Admission Date (:DT)
         [rpt-of-eligibility-code] ;; Rpt of Eligibility Code (:ID)
         [rpt-of-eligibility-date] ;; Rpt of Eligibility Date (:DT)
         [release-information-code] ;; Release Information Code (:ID)
         [pre] ;; Pre-Admit Cert. (PAC) (:ST)
         [verification-date] ;; Verification Date (:DT)
         [verification-by] ;; Verification By (:CM)
         [type-of-agreement-code] ;; Type of Agreement Code (:ID)
         [billing-status] ;; Billing Status (:ID)
         [lifetime-reserve-days] ;; Lifetime Reserve Days (:NM)
         [delay-before-l] ;; Delay Before L. R. Day (:NM)
         [company-plan-code] ;; Company Plan Code (:ST)
         [policy-number] ;; Policy Number (:ST)
         [policy-deductible] ;; Policy Deductible (:NM)
         [policy-limit] ;; Policy Limit - Amount (:NM)
         [policy-limit-2] ;; Policy Limit - Days (:NM)
         [room-rate] ;; Room Rate - Semi-Private (:NM)
         [room-rate-2] ;; Room Rate - Private (:NM)
         [insured's-employ-status] ;; Insured's Employ Status (:ID)
         [insured's-sex] ;; Insured's Sex (:ID)
         [insured's-employer-addr] ;; Insured's Employer Addr (:XAD)
         [verification-status] ;; Verification Status (:ST)
         [prior-insurance-plan-id] ;; Prior Insurance Plan ID (:IS)
         [coverage-type] ;; Coverage Type (:IS)
         [handicap] ;; Handicap (:IS)
         [insured<92>s-id-number] ;; Insured<92>s ID Number (:CX)
           ](->> segment :fields (map :content))]
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
      :insured's-group-emp-2 insured's-group-emp-2
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
      :policy-limit-2 policy-limit-2
      :room-rate room-rate
      :room-rate-2 room-rate-2
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
  (let [[[insured’s-employee-id] ;; Insured’s Employee ID (:CX)
         [insured’s-social-security-number] ;; Insured’s Social Security Number (:ST)
         [insured’s-employer’s-name-and-id] ;; Insured’s Employer’s Name and ID (:XCN)
         [employer-information-data] ;; Employer Information Data (:IS)
         [mail-claim-party] ;; Mail Claim Party (:IS)
         [medicare-health-ins-card-number] ;; Medicare Health Ins Card Number (:ST)
         [medicaid-case-name] ;; Medicaid Case Name (:XPN)
         [medicaid-case-number] ;; Medicaid Case Number (:ST)
         [military-sponsor-name] ;; Military Sponsor Name (:XPN)
         [military-id-number] ;; Military ID Number (:ST)
         [dependent-of-military-recipient] ;; Dependent Of Military Recipient (:CE)
         [military-organization] ;; Military Organization (:ST)
         [military-station] ;; Military Station (:ST)
         [military-service] ;; Military Service (:IS)
         [military-rank-grade] ;; Military Rank/Grade (:IS)
         [military-status] ;; Military Status (:IS)
         [military-retire-date] ;; Military Retire Date (:DT)
         [military-non] ;; Military Non-Avail Cert On File (:ID)
         [baby-coverage] ;; Baby Coverage (:ID)
         [combine-baby-bill] ;; Combine Baby Bill (:ID)
         [blood-deductible] ;; Blood Deductible (:ST)
         [special-coverage-approval-name] ;; Special Coverage Approval Name (:XPN)
         [special-coverage-approval-title] ;; Special Coverage Approval Title (:ST)
         [non] ;; Non-Covered Insurance Code (:IS)
         [payor-id] ;; Payor ID (:CX)
         [payor-subscriber-id] ;; Payor Subscriber ID (:CX)
         [eligibility-source] ;; Eligibility Source (:IS)
         [room-coverage-type-amount] ;; Room Coverage Type/Amount (:CM)
         [policy-type-amount] ;; Policy Type/Amount (:CM)
         [daily-deductible] ;; Daily Deductible (:CM)
         [living-dependency] ;; Living Dependency (:IS)
         [ambulatory-status] ;; Ambulatory Status (:IS)
         [citizenship] ;; Citizenship (:CE)
         [primary-language] ;; Primary Language (:CE)
         [living-arrangement] ;; Living Arrangement (:IS)
         [publicity-code] ;; Publicity Code (:CE)
         [protection-indicator] ;; Protection Indicator (:ID)
         [student-indicator] ;; Student Indicator (:IS)
         [religion] ;; Religion (:CE)
         [mother’s-maiden-name] ;; Mother’s Maiden Name (:XPN)
         [nationality] ;; Nationality (:CE)
         [ethnic-group] ;; Ethnic Group (:CE)
         [marital-status] ;; Marital Status (:CE)
         [insured’s-employment-start-date] ;; Insured’s Employment Start Date (:DT)
         [employment-stop-date] ;; Employment Stop Date (:DT)
         [job-title] ;; Job Title (:ST)
         [job-code-class] ;; Job Code/Class (:JCC)
         [job-status] ;; Job Status (:IS)
         [employer-contact-person-name] ;; Employer Contact Person Name (:XPN)
         [employer-contact-person-phone-number] ;; Employer Contact Person Phone Number (:XTN)
         [employer-contact-reason] ;; Employer Contact Reason (:IS)
         [insured’s-contact-person’s-name] ;; Insured’s Contact Person’s Name (:XPN)
         [insured’s-contact-person-phone-number] ;; Insured’s Contact Person Phone Number (:XTN)
         [insured’s-contact-person-reason] ;; Insured’s Contact Person Reason (:IS)
         [relationship-to-the-patient-start-date] ;; Relationship To The Patient Start Date (:DT)
         [relationship-to-the-patient-stop-date] ;; Relationship To The Patient Stop Date (:DT)
         [insurance-co] ;; Insurance Co. Contact Reason (:IS)
         [insurance-co-contact-phone-number] ;; Insurance Co Contact Phone Number (:XTN)
         [policy-scope] ;; Policy Scope (:IS)
         [policy-source] ;; Policy Source (:IS)
         [patient-member-number] ;; Patient Member Number (:CX)
         [guarantor’s-relationship-to-insured] ;; Guarantor’s Relationship To Insured (:CE)
         [insured’s-phone-number] ;; Insured’s Phone Number - Home (:XTN)
         [insured’s-employer-phone-number] ;; Insured’s Employer Phone Number (:XTN)
         [military-handicapped-program] ;; Military Handicapped Program (:CE)
         [suspend-flag] ;; Suspend Flag (:ID)
         [copay-limit-flag] ;; Copay Limit Flag (:ID)
         [stoploss-limit-flag] ;; Stoploss Limit Flag (:ID)
         [insured-organization-name-and-id] ;; Insured Organization Name And ID (:XON)
         [insured-employer-organization-name-and-id] ;; Insured Employer Organization Name And ID (:XON)
         [race] ;; Race (:CE)
         [hcfa-patient’s-relationship-to-insured] ;; HCFA Patient’s Relationship to Insured (:CE)
           ](->> segment :fields (map :content))]
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
  (let [[[set-id] ;; Set ID - IN3 (:SI)
         [certification-number] ;; Certification Number (:CX)
         [certified-by] ;; Certified By (:XCN)
         [certification-required] ;; Certification Required (:ID)
         [penalty] ;; Penalty (:CM)
         [certification-date-time] ;; Certification Date/Time (:TS)
         [certification-modify-date-time] ;; Certification Modify Date/Time (:TS)
         [operator] ;; Operator (:XCN)
         [certification-begin-date] ;; Certification Begin Date (:DT)
         [certification-end-date] ;; Certification End Date (:DT)
         [days] ;; Days (:CM)
         [non] ;; Non-Concur Code/Description (:CE)
         [non-2] ;; Non-Concur Effective Date/Time (:TS)
         [physician-reviewer] ;; Physician Reviewer (:XCN)
         [certification-contact] ;; Certification Contact (:ST)
         [certification-contact-phone-number] ;; Certification Contact Phone Number (:XTN)
         [appeal-reason] ;; Appeal Reason (:CE)
         [certification-agency] ;; Certification Agency (:CE)
         [certification-agency-phone-number] ;; Certification Agency Phone Number (:XTN)
         [pre] ;; Pre-Certification Req/Window (:CM)
         [case-manager] ;; Case Manager (:ST)
         [second-opinion-date] ;; Second Opinion Date (:DT)
         [second-opinion-status] ;; Second Opinion Status (:IS)
         [second-opinion-documentation-received] ;; Second Opinion Documentation Received (:IS)
         [second-opinion-physician] ;; Second Opinion Physician (:XCN)
           ](->> segment :fields (map :content))]
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
      :non-2 non-2
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
  (let [[[set-id] ;; Set ID - Procedure (:SI)
         [procedure-coding-method] ;; Procedure Coding Method (:ID)
         [procedure-code] ;; Procedure Code (:ID)
         [procedure-description] ;; Procedure Description (:ST)
         [procedure-date-time] ;; Procedure Date/Time (:TS)
         [procedure-type] ;; Procedure Type (:ID)
         [procedure-minutes] ;; Procedure Minutes (:NM)
         [anesthesiologist] ;; Anesthesiologist (:CN)
         [anesthesia-code] ;; Anesthesia Code (:ID)
         [anesthesia-minutes] ;; Anesthesia Minutes (:NM)
         [surgeon] ;; Surgeon (:CN)
         [resident-code] ;; Resident Code (:CN)
         [consent-code] ;; Consent Code (:ID)
           ](->> segment :fields (map :content))]
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
  (let [[[error-code-and-location] ;; Error Code and Location (:ID)
           ](->> segment :fields (map :content))]
    [:ERR
     {:error-code-and-location error-code-and-location}]))

;; Notes and Comments
(defmethod parse-segment "NTE" [segment]
  (let [[[set-id] ;; Set ID (:SI)
         [source-of-comment] ;; Source of Comment (:ID)
         [comment] ;; Comment (:TX)
         [comment-type] ;; Comment Type (:CE)
           ](->> segment :fields (map :content))]
    [:NTE
     {:set-id set-id
      :source-of-comment source-of-comment
      :comment comment
      :comment-type comment-type}]))

;; Scheduling Activity Information
(defmethod parse-segment "SCH" [segment]
  (let [[[placer-appointment-id] ;; Placer Appointment ID (:EI)
         [filler-appointment-id] ;; Filler Appointment ID (:EI)
         [occurrence-number] ;; Occurrence Number (:NM)
         [placer-group-number] ;; Placer Group Number (:EI)
         [schedule-id] ;; Schedule ID (:CE)
         [event-reason] ;; Event Reason (:CE)
         [appointment-reason] ;; Appointment Reason (:CE)
         [appointment-type] ;; Appointment Type (:CE)
         [appointment-duration] ;; Appointment Duration (:NM)
         [appointment-duration-units] ;; Appointment Duration Units (:CE)
         [appointment-timing-quantity] ;; Appointment Timing Quantity (:TQ)
         [placer-contact-person] ;; Placer Contact Person (:XCN)
         [placer-contact-phone-number] ;; Placer Contact Phone Number (:XTN)
         [placer-contact-address] ;; Placer Contact Address (:XAD)
         [placer-contact-location] ;; Placer Contact Location (:PL)
         [filler-contact-person] ;; Filler Contact Person (:XCN)
         [filler-contact-phone-number] ;; Filler Contact Phone Number (:XTN)
         [filler-contact-address] ;; Filler Contact Address (:XAD)
         [filler-contact-location] ;; Filler Contact Location (:PL)
         [entered-by-person] ;; Entered by Person (:XCN)
         [entered-by-phone-number] ;; Entered by Phone Number (:XTN)
         [entered-by-location] ;; Entered by Location (:PL)
         [parent-placer-appointment-id] ;; Parent Placer Appointment ID (:EI)
         [parent-filler-appointment-id] ;; Parent Filler Appointment ID (:EI)
         [filler-status-code] ;; Filler Status Code (:CE)
           ](->> segment :fields (map :content))]
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
  (let [[[set-id] ;; Set ID (:SI)
         [segment-action-code] ;; Segment Action Code (:ID)
         [resource-group-id] ;; Resource Group ID (:CE)
           ](->> segment :fields (map :content))]
    [:RGS
     {:set-id set-id
      :segment-action-code segment-action-code
      :resource-group-id resource-group-id}]))

;; Appointment Information - Location resource
(defmethod parse-segment "AIL" [segment]
  (let [[[set-id] ;; Set ID (:SI)
         [segment-action-code] ;; Segment Action Code (:ID)
         [location-resource-id] ;; Location Resource ID (:PL)
         [location-type] ;; Location Type (:CE)
         [location-group] ;; Location Group (:CE)
         [start-date-time] ;; Start Date/Time (:TS)
         [start-date-time-offset] ;; Start Date/Time Offset (:NM)
         [start-date-time-offset-units] ;; Start Date/Time Offset Units (:CE)
         [duration] ;; Duration (:NM)
         [duration-units] ;; Duration Units (:CE)
         [allow-substitution-code] ;; Allow Substitution Code (:IS)
         [filler-status-code] ;; Filler Status Code (:CE)
           ](->> segment :fields (map :content))]
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
  (let [[[set-id] ;; Set ID (:SI)
         [segment-action-code] ;; Segment Action code (:ID)
         [personnel-resource-id] ;; Personnel Resource ID (:XCN)
         [resource-role] ;; Resource Role (:CE)
         [resource-group] ;; Resource Group (:CE)
         [start-date-time] ;; Start Date/Time (:TS)
         [start-date-time-offset] ;; Start Date/Time Offset (:NM)
         [start-date-time-offset-units] ;; Start Date/Time Offset Units (:CE)
         [duration] ;; Duration (:NM)
         [duration-units] ;; Duration Units (:CE)
         [allow-substitution-code] ;; Allow Substitution Code (:IS)
         [filler-status-code] ;; Filler Status Code (:CE)
           ](->> segment :fields (map :content))]
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
  (let [[[set-id] ;; Set ID - AIG (:SI)
         [segment-action-code] ;; Segment Action Code (:ID)
         [resource-id] ;; Resource ID (:CE)
         [resource-type] ;; Resource Type (:CE)
         [resource-group] ;; Resource Group (:CE)
         [resource-quantity] ;; Resource Quantity (:NM)
         [resource-quantity-units] ;; Resource Quantity Units (:CE)
         [start-date-time] ;; Start Date/Time (:TS)
         [start-date-time-offset] ;; Start Date/Time Offset (:NM)
         [start-date-time-offset-units] ;; Start Date/Time Offset Units (:CE)
         [duration] ;; Duration (:NM)
         [duration-units] ;; Duration Units (:CE)
         [allow-substitution-code] ;; Allow Substitution Code (:IS)
         [filler-status-code] ;; Filler Status Code (:CE)
           ](->> segment :fields (map :content))]
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
  (let [[[accident-date-time] ;; Accident Date/Time (:TS)
         [accident-code] ;; Accident Code (:CE)
         [accident-location] ;; Accident Location (:ST)
         [auto-accident-state] ;; Auto Accident State (:CE)
         [accident-job-related-indicator] ;; Accident Job Related Indicator (:ID)
         [accident-death-indicator] ;; Accident Death Indicator (:ID)
           ](->> segment :fields (map :content))]
    [:ACC
     {:accident-date-time accident-date-time
      :accident-code accident-code
      :accident-location accident-location
      :auto-accident-state auto-accident-state
      :accident-job-related-indicator accident-job-related-indicator
      :accident-death-indicator accident-death-indicator}]))

;; UB82
(defmethod parse-segment "UB1" [segment]
  (let [[[set-id] ;; Set ID - UB1 (:SI)
         [blood-deductible] ;; Blood Deductible (:NM)
         [blood-furnished] ;; Blood Furnished-Pints Of (:NM)
         [blood-replaced] ;; Blood Replaced-Pints (:NM)
         [blood-not-replaced] ;; Blood Not Replaced-Pints (:NM)
         [co] ;; Co-Insurance Days (:NM)
         [condition-code] ;; Condition Code (:IS)
         [covered-days--] ;; Covered Days - (:NM)
         [non-covered-days] ;; Non Covered Days (:NM)
         [value-amount] ;; Value Amount & Code (:CM)
         [number-of-grace-days] ;; Number Of Grace Days (:NM)
         [special-program-indicator] ;; Special Program Indicator (:CE)
         [psro-ur-approval-indicator] ;; PSRO/UR Approval Indicator (:CE)
         [psro-ur-approved-stay] ;; PSRO/UR Approved Stay-Fm (:DT)
         [psro-ur-approved-stay-2] ;; PSRO/UR Approved Stay-To (:DT)
         [occurrence] ;; Occurrence (:CM)
         [occurrence-span] ;; Occurrence Span (:CE)
         [span-start-date] ;; Span Start Date (:DT)
         [span-end-date] ;; Span End Date (:DT)
         [ub] ;; UB-82 Locator (:ST)
         [ub-2] ;; UB-82 Locator (:ST)
         [ub-3] ;; UB-82 Locator (:ST)
         [ub-4] ;; UB-82 Locator (:ST)
           ](->> segment :fields (map :content))]
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
      :psro-ur-approved-stay-2 psro-ur-approved-stay-2
      :occurrence occurrence
      :occurrence-span occurrence-span
      :span-start-date span-start-date
      :span-end-date span-end-date
      :ub ub
      :ub-2 ub-2
      :ub-3 ub-3
      :ub-4 ub-4}]))

;; UB92 Data
(defmethod parse-segment "UB2" [segment]
  (let [[[set-id] ;; Set ID - UB2 (:SI)
         [co] ;; Co-Insurance Days (:ST)
         [condition-code] ;; Condition Code (:IS)
         [covered-days] ;; Covered Days (:ST)
         [non] ;; Non-Covered Days (:ST)
         [value-amount] ;; Value Amount & Code (:CM)
         [occurrence-code] ;; Occurrence Code & Date (:CM)
         [occurrence-span-code-dates] ;; Occurrence Span Code/Dates (:CM)
         [ub92-locator-2] ;; UB92 Locator 2 (State) (:ST)
         [ub92-locator-11] ;; UB92 Locator 11 (State) (:ST)
         [ub92-locator-31] ;; UB92 Locator 31 (National) (:ST)
         [document-control-number] ;; Document Control Number (:ST)
         [ub92-locator-49] ;; UB92 Locator 49 (National) (:ST)
         [ub92-locator-56] ;; UB92 Locator 56 (State) (:ST)
         [ub92-locator-57] ;; UB92 Locator 57 (National) (:ST)
         [ub92-locator-78] ;; UB92 Locator 78 (State) (:ST)
         [special-visit-count] ;; Special Visit Count (:NM)
           ](->> segment :fields (map :content))]
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
  (let [[[set-id] ;; Set ID - NK1 (:SI)
         [name] ;; Name (:XPN)
         [relationship] ;; Relationship (:CE)
         [address] ;; Address (:XAD)
         [phone-number] ;; Phone Number (:XTN)
         [business-phone-number] ;; Business Phone Number (:XTN)
         [contact-role] ;; Contact Role (:CE)
         [start-date] ;; Start Date (:DT)
         [end-date] ;; End Date (:DT)
         [next-of-kin-associated-parties-job-title] ;; Next of Kin / Associated Parties Job Title (:ST)
         [next-of-kin-associated-parties-jobcode-class] ;; Next of Kin / Associated Parties JobCode/Class (:JCC)
         [next-of-kin-associated-parties-employeenumber] ;; Next of Kin / Associated Parties EmployeeNumber (:CX)
         [organization-name] ;; Organization Name - NK1 (:XON)
         [marital-status] ;; Marital Status (:CE)
         [sex] ;; Sex (:IS)
         [date-time-of-birth] ;; Date/Time of Birth (:TS)
         [living-dependency] ;; Living Dependency (:IS)
         [ambulatory-status] ;; Ambulatory Status (:IS)
         [citizenship] ;; Citizenship (:CE)
         [primary-language] ;; Primary Language (:CE)
         [living-arrangement] ;; Living Arrangement (:IS)
         [publicity-code] ;; Publicity Code (:CE)
         [protection-indicator] ;; Protection Indicator (:ID)
         [student-indicator] ;; Student Indicator (:IS)
         [religion] ;; Religion (:CE)
         [mother's-maiden-name] ;; Mother's Maiden Name (:XPN)
         [nationality] ;; Nationality (:CE)
         [ethnic-group] ;; Ethnic Group (:CE)
         [contact-reason] ;; Contact Reason (:CE)
         [contact-person's-name] ;; Contact Person's Name (:XPN)
         [contact-person's-telephone-number] ;; Contact Person's Telephone Number (:XTN)
         [contact-person's-address] ;; Contact Person's Address (:XAD)
         [next-of-kin-associated-party's-identifiers] ;; Next of Kin/Associated Party's Identifiers (:CX)
         [job-status] ;; Job Status (:IS)
         [race] ;; Race (:CE)
         [handicap] ;; Handicap (:IS)
         [contact-person-social-security-number] ;; Contact Person Social Security Number (:ST)
           ](->> segment :fields (map :content))]
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
  (let [[[order-control-code] ;; Order Control Code (:ID)
         [placer-order-number] ;; Placer Order Number (:EI)
         [filler-order-number] ;; Filler Order Number (:EI)
         [placer-group-number] ;; Placer Group Number (:EI)
         [order-status] ;; Order Status (:ID)
         [response-flag] ;; Response Flag (:ID)
         [quantity-timing] ;; Quantity/Timing (:TQ)
         [parent] ;; Parent (:CM)
         [transaction-date-time] ;; Transaction Date/Time (:TS)
         [entered-by] ;; Entered By (:XCN)
         [verified-by] ;; Verified By (:XCN)
         [ordering-provider] ;; Ordering Provider (:XCN)
         [enterer's-location] ;; Enterer's Location (:PL)
         [call-back-phone-number] ;; Call Back Phone Number (:XTN)
         [order-effective-date-time] ;; Order Effective Date/Time (:TS)
         [order-control-code-reason] ;; Order Control Code Reason (:CE)
         [entering-organization] ;; Entering Organization (:CE)
         [entering-device] ;; Entering Device (:CE)
         [action-by] ;; Action By (:XCN)
         [advanced-beneficiary-notice-code] ;; Advanced Beneficiary Notice Code (:CE)
         [ordering-facility-name] ;; Ordering Facility Name (:XON)
         [ordering-facility-address] ;; Ordering Facility Address (:XAD)
         [ordering-facility-phone-number] ;; Ordering Facility Phone Number (:XTN)
         [ordering-provider-address] ;; Ordering Provider Address (:XAD)
           ](->> segment :fields (map :content))]
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
  (let [[[set-id] ;; Set ID (:SI)
         [placer-order-number] ;; Placer Order Number (:EI)
         [filler-order-number] ;; Filler Order Number (:EI)
         [universal-service-id] ;; Universal Service ID (:CE)
         [priority] ;; Priority (:ID)
         [requested-date-time] ;; Requested Date/Time (:TS)
         [observation-date-time] ;; Observation Date/Time (:TS)
         [observation-end-date-time] ;; Observation End Date/Time (:TS)
         [collection-volume] ;; Collection Volume (:CQ)
         [collector-identifier] ;; Collector Identifier (:XCN)
         [specimen-action-code] ;; Specimen Action Code (:ID)
         [danger-code] ;; Danger Code (:CE)
         [relevant-clinical-info] ;; Relevant Clinical Info (:ST)
         [specimen-received-date-time] ;; Specimen Received Date/Time (:TS)
         [specimen-source] ;; Specimen Source (:CM)
         [ordering-provider] ;; Ordering Provider (:XCN)
         [order-callback-phone-number] ;; Order Callback Phone Number (:XTN)
         [placer-field-1] ;; Placer Field 1 (:ST)
         [placer-field-2] ;; Placer Field 2 (:ST)
         [filler-field-1] ;; Filler Field 1 (:ST)
         [filler-field-2] ;; Filler Field 2 (:ST)
         [results-rpt-change-date-time] ;; Results Rpt/Change Date/Time (:TS)
         [charge-to-practice] ;; Charge to Practice (:CM)
         [diagnostice-serv-sect-id] ;; Diagnostice Serv Sect ID (:ID)
         [result-status] ;; Result Status (:ID)
         [parent-result] ;; Parent Result (:CM)
         [quantity-timing] ;; Quantity/Timing (:TQ)
         [result-copies-to] ;; Result Copies To (:XCN)
         [parent] ;; Parent (:CM)
         [transportation-mode] ;; Transportation Mode (:ID)
         [reason-for-study] ;; Reason for Study (:CE)
         [principal-result-interpreter] ;; Principal Result Interpreter (:CM)
         [assistant-result-interpreter] ;; Assistant Result Interpreter (:CM)
         [technician] ;; Technician (:CM)
         [transcriptionist] ;; Transcriptionist (:CM)
         [scheduled-date-time] ;; Scheduled Date/Time (:TS)
         [number-of-sample-containers] ;; Number of Sample Containers (:NM)
         [transport-logistics-of-collected-samples] ;; Transport Logistics of Collected Samples (:CE)
         [collector's-comment] ;; Collector's Comment (:CE)
         [transport-arrangement-responsibility] ;; Transport Arrangement Responsibility (:CE)
         [transport-arranged] ;; Transport Arranged (:ID)
         [escort-required] ;; Escort Required (:ID)
         [planned-patient-transport-comment] ;; Planned Patient Transport Comment (:CE)
         [procedure-code] ;; Procedure Code (:CE)
         [procedure-code-modifier] ;; Procedure Code Modifier (:CE)
           ](->> segment :fields (map :content))]
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
  (let [[[set-id] ;; Set ID (:SI)
         [value-type] ;; Value Type (:ID)
         [observation-identifier] ;; Observation Identifier (:CE)
         [observation-sub] ;; Observation Sub-Id (:ST)
         [observation-value] ;; Observation Value (:FT)
         [units] ;; Units (:CE)
         [reference-range] ;; Reference Range (:ST)
         [abnormal-flags] ;; Abnormal Flags (:ID)
         [probability] ;; Probability (:NM)
         [nature-of-abnormal-test] ;; Nature of Abnormal Test (:ID)
         [observation-result-status] ;; Observation Result Status (:ID)
         [date-last-obs-normal-value] ;; Date Last Obs Normal Value (:TS)
         [user-defined-access-checks] ;; User Defined Access Checks (:ST)
         [date-time-of-the-observation] ;; Date/Time of the Observation (:TS)
         [producer's-id] ;; Producer's ID (:CE)
         [responsible-observer] ;; Responsible Observer (:XCN)
         [observation-method] ;; Observation Method (:CE)
           ](->> segment :fields (map :content))]
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
  (let [[[give-sub] ;; Give Sub-ID Counter (:NM)
         [administration-sub] ;; Administration Sub-ID Counter (:NM)
         [date-time-start-of-administration] ;; Date/Time Start of Administration (:TS)
         [date-time-end-of-administration] ;; Date/Time End of Administration (:TS)
         [administered-code] ;; Administered Code ^CVX (CDC DB) (:CE)
         [administered-amount] ;; Administered Amount (:NM)
         [administered-units] ;; Administered Units (:CE)
         [administered-dosage-form] ;; Administered Dosage Form (:CE)
         [administration-notes] ;; Administration Notes (:CE)
         [administering-provider] ;; Administering Provider (:XCN)
         [administered] ;; Administered-at Location (:CM)
         [administered-per] ;; Administered Per (Time Unit) (:ST)
         [administered-strength] ;; Administered Strength (:NM)
         [administered-strength-units] ;; Administered Strength Units (:CE)
         [substance-lot-number] ;; Substance Lot Number (:ST)
         [substance-expiration-date] ;; Substance Expiration Date (:TS)
         [substance-manufacturer-name] ;; Substance Manufacturer Name ^MVX (:CE)
         [substance-refusal-reason] ;; Substance Refusal Reason (:CE)
         [indication] ;; Indication (:CE)
         [completion-status] ;; Completion Status (:ID)
         [action-code] ;; Action Code-RXA (:ID)
         [system-entry-date-time] ;; System Entry Date/Time (:TS)
           ](->> segment :fields (map :content))]
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
  (let [[[route] ;; Route (p351 in pdf) (:CE)
         [site] ;; Site (possibly SNOMED) (:CE)
         [administration-device] ;; Administration Device (p352 in pdf) (:CE)
         [administration-method] ;; Administration Method (:CE)
         [routing-instruction] ;; Routing Instruction (:CE)
           ](->> segment :fields (map :content))]
    [:RXR
     {:route route
      :site site
      :administration-device administration-device
      :administration-method administration-method
      :routing-instruction routing-instruction}]))

;; Transcription Document Header
(defmethod parse-segment "TXA" [segment]
  (let [[[set-id] ;; Set ID- TXA (:SI)
         [document-type] ;; Document Type (:IS)
         [document-content-presentation] ;; Document Content Presentation (:ID)
         [activity-date-time] ;; Activity Date/Time (:TS)
         [primary-activity-provider-code-name] ;; Primary Activity Provider Code/Name (:XCN)
         [origination-date-time] ;; Origination Date/Time (:TS)
         [transcription-date-time] ;; Transcription Date/Time (:TS)
         [edit-date-time] ;; Edit Date/Time (:TS)
         [originator-code-name] ;; Originator Code/Name (:XCN)
         [assigned-document-authenticator] ;; Assigned Document Authenticator (:XCN)
         [transcriptionist-code-name] ;; Transcriptionist Code/Name (:XCN)
         [unique-document-number] ;; Unique Document Number (:EI)
         [parent-document-number] ;; Parent Document Number (:EI)
         [placer-order-number] ;; Placer Order Number (:EI)
         [filler-order-number] ;; Filler Order Number (:EI)
         [unique-document-file-name] ;; Unique Document File Name (:ST)
         [document-completion-status] ;; Document Completion Status (:ID)
         [document-confidentiality-status] ;; Document Confidentiality Status (:ID)
         [document-availability-status] ;; Document Availability Status (:ID)
         [document-storage-status] ;; Document Storage Status (:ID)
         [document-change-reason] ;; Document Change Reason (:ST)
         [authentication-person] ;; Authentication Person, Time Stamp (:PPN)
         [distributed-copies] ;; Distributed Copies (Code and Name of Recipients) (:XCN)
           ](->> segment :fields (map :content))]
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
  (let [[[query-tag] ;; Query Tag (:ST)
         [event-identifier] ;; Event Identifier (:CE)
         [input-parameter-list] ;; Input Parameter List (:QIP)
           ](->> segment :fields (map :content))]
    [:QAK
     {:query-tag query-tag
      :event-identifier event-identifier
      :input-parameter-list input-parameter-list}]))

;; Original Style Query Definition
(defmethod parse-segment "QRD" [segment]
  (let [[[query-date-time] ;; Query Date/Time (:TS)
         [query-format-code] ;; Query Format Code - usually: R (:ID)
         [query-priority] ;; Query Priority - usually: I (:ID)
         [query-id] ;; Query ID (unique ID assigned by querying app) (:ST)
         [deferred-response-type] ;; Deferred Response Type (not used w/ .3 == I) (:ID)
         [deferred-response-date-time] ;; Deferred Response Date/Time (not used w/ .3 == I) (:TS)
         [quantity-limited-request] ;; Quantity Limited Request (not used) (:CQ)
         [who-subject-filter] ;; Who Subject Filter (Queried Patient information) (:XCN)
         [what-subject-filter] ;; What Subject Filter - usually: VXI (:CE)
         [what-department-data-code] ;; What Department Data Code (specific for VXI) (:CE)
         [what-data-code-value-qual] ;; What Data Code Value Qual (result range criteria) (:CM)
         [query-results-level] ;; Query Results Level (:ID)
           ](->> segment :fields (map :content))]
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
  (let [[[where-subject-filter] ;; Where Subject Filter (department,system,etc: LAB~HEMO) (:ST)
         [when-data-start-date-time] ;; When Data Start Date/Time - Backwards only (:TS)
         [when-data-end-date-time] ;; When Data End Date/Time - Backwards only (:TS)
         [what-user-qualifier] ;; What User Qualifier (extra limitation) (:ST)
         [other-qry-subject-filter] ;; Other QRY Subject Filter (limit of 10 repeats for VXQ) see hl7_notes.txt (:ST)
         [which-date-time-qualifier] ;; Which Date/Time Qualifier (range of .2/.3) - usually: ANY (:ID)
         [which-date-time-status-qualifier] ;; Which Date/Time Status Qualifier - usually: CFN or FIN (current final value, final only) (:ID)
         [date-time-selection-qualifier] ;; Date/Time Selection Qualifier (value ordering (1ST,LST,ALL,REV) - usually:REV (reverse cronological) (:ID)
         [when-quantity-timing-qualifier] ;; When Quantity/Timing Qualifier (replaces .2/.3) (:TQ)
           ](->> segment :fields (map :content))]
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
