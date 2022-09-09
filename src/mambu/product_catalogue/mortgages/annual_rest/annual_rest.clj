;;; POC for supporting AnnualRest mortgages in Mambu
;;; #bookmark= #bookmark= 42c4bbb6-a79c-4e57-9cf0-a6c3fffd6cdc
(ns mambu.product_catalogue.mortgages.annual_rest.annual_rest
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]
            [http.api.mambu.demo.loyalty_points :as lpd]
            [http.api.mambu.demo.credit_card.zap_cust :as zap]
            [http.api.mambu.experiments.loan_schedule :as ext]
            [http.api.mambu.solutions.int_change.int_change :as inst]
            [java-time :as t]
            [clojure.string :as str]
            ))


(def ARESTCALC-PROD-KEY (atom "8a19cff4831dda0801831e7b819f037a")) ;; new3 = 8a19a6bb8321062b0183214d806f0b0c (365 day-count) new2 = 8a19cff4831dda0801831e7b819f037a (30/360 day-count) old = 8a19c04783160b4c01831665a0220798
(def ARESTPRIN-PROD-KEY (atom "8a19d2348317536001831d1d70ea2fbe")) ;; new2= 8a19d2348317536001831d1d70ea2fbe old=8a19c04783160b4c01831674fe4e0816
(def ARESTINT-PROD-KEY (atom "8a19c04783160b4c0183167b5d1e0841"))  ;; new2= 8a19a6bb8321062b01832157ca870887 (365 day-count) old= 8a19c04783160b4c0183167b5d1e0841 (30/360 day-count)
(def PAYSETTLE-PROD-KEY (atom "8a19c04783160b4c01831689a2f52145"))
(defonce CUSTID (atom nil))
(defonce CUSTKEY (atom nil))
(defonce ARESTCALC-ACCID (atom nil))
(defonce PRINONLY-ACCID (atom nil))
(defonce INTONLY-ACCID (atom nil))
(defonce TIMEZONE (atom "Europe/London"))
(defonce DATEOFFSET (atom "+01:00"))
(defonce OVERPAYMENT-THRESHOLD (atom 1000.00))

;;*************************************************************
;; Mambu-core API calls
;;

(defn call-api
  ([api context attr-to-return]
   (let [res (call-api api context)
         val (get res attr-to-return)]
     val))
  ([api context]
   (let  [res (steps/apply-api api context)]
     (:last-call res))))

(defn get-client-api [context]
  {:url (str "{{*env*}}/clients/" (:custid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-loan-account [context]
  {:url (str "{{*env*}}/loans/" (:accid context))
   :method api/GET
   :query-params {}
   :body {}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn zap-cust [context]
  (let  [cust-key (get (call-api get-client-api context) "encodedKey")
         _ (prn "cust-key" cust-key)
         context1 (assoc context :cust-key cust-key)] 
    (doall (zap/remove-all-open-dep-accounts context1))     
    (doall (ext/zap-all-loans2 cust-key))
    (steps/apply-api zap/exit-customer2 context1)))

(defn change-periodic-payment-api [context]
  {:url (str "{{*env*}}/loans/" (:accid context) ":changePeriodicPayment")
   :method api/POST
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :query-params {}
   :body  {"periodicPayment" (:amount context)
           "valueDate" (:value-date context)}})

(defn create-credit-arrangement [context]
  {:url (str "{{*env*}}/creditarrangements")
   :method api/POST
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :query-params {}
   :body  {"amount" (:credit-limit context)
           "availableCreditAmount" (:credit-limit context)
           "expireDate" (str "2100-08-23T00:00:00" @DATEOFFSET)
           "exposureLimitType" "OUTSTANDING_AMOUNT"
           "holderKey" (:cust-key context)
           "holderType" "CLIENT"
           "notes" ""
           "startDate" (str "2019-08-23T00:00:00" @DATEOFFSET)
           "state" "APPROVED"
           }})

(defn addLoanToCreditLine [context]
  {:url (str "{{*env*}}/linesofcredit/" (:ca-id context) "/loans/" (:accid context))
   :method api/POST
   :headers {"Content-Type" "application/json"}})

(defn get-deposit-accounts-api [context]
  {:url (str "{{*env*}}/deposits/")
   :method api/GET
    :query-params {
        "detailsLevel" "FULL"
        "accountHolderType" "CLIENT"
        "accountHolderId" (:custid context)
        }
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-loan-accounts-api [context]
  {:url (str "{{*env*}}/loans/")
   :method api/GET
   :query-params {"detailsLevel" "FULL"
                  "accountHolderType" "CLIENT"
                  "accountHolderId" (:custid context)}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

;; API for creating a fixed-interest loan
(defn create-installment-loan-api1 [context]
  {:url (str "{{*env*}}/loans")
   :method api/POST
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :query-params {}
   :body  {"loanAmount" (:amount context)
           "loanName" (:acc-name context)
           "accountHolderKey" (:cust-key context)
           "productTypeKey" (:prod-key context)
           "accountHolderType" "CLIENT"
           "interestFromArrearsAccrued" 0.0
           "interestSettings" {"interestRate" (:interest-rate context)}
           "scheduleSettings" {"periodicPayment" (:periodic-payment context)
                               "gracePeriod" (:grace_period context)
                               "repaymentInstallments" (:num-installments context)}}})

;; API for creating a variable-interest loan
(defn create-installment-loan-api2 [context]
  {:url (str "{{*env*}}/loans")
   :method api/POST
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :query-params {}
   :body  {"loanAmount" (:amount context)
           "loanName" (:acc-name context)
           "accountHolderKey" (:cust-key context)
           "productTypeKey" (:prod-key context)
           "accountHolderType" "CLIENT"
           "interestFromArrearsAccrued" 0.0
           "interestSettings" {"interestSpread" (:interest-rate-spread context)}
           "scheduleSettings" {"periodicPayment" (:periodic-payment context)
                               "gracePeriod" (:grace_period context)
                               "repaymentInstallments" (:num-installments context)}}})


(defn get-loan-schedule [context]
  (let [api-call (fn [context0]
                   {:url (str "{{*env*}}/loans/" (:accid context0) "/schedule")
                    :method api/GET
                    :query-params {"detailsLevel" "FULL"}
                    :headers {"Accept" "application/vnd.mambu.v2+json"
                              "Content-Type" "application/json"}})]
    (steps/apply-api api-call context)))

(defn repayment-loan-api [context]
  {:url (str "{{*env*}}/loans/" (:loanAccountId context) "/repayment-transactions")
   :method api/POST
   :body (merge {"amount" (:amount context)
                 "externalId" (:externalId context)
                 "installmentEncodedKey" (:installmentEncodedKey context)
                 "notes" "Adjust amount of principal interest paid on"}
                (when (:custom-prin-only context)
                  {"customPaymentAmounts" [{"amount" (:amount context)
                                            "customPaymentAmountType" "PRINCIPAL"}]}))
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

            


(defn loan-interest-rate-change-api [context]
  {:url (str "{{*env*}}/loans/" (:loanAccountId context) ":changeInterestRate")
   :method api/POST
   :body {"interestRate" (:interest-rate context)
          "notes" "Changed interest-rate",
          "valueDate" (:value-date context)}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

;;*************************************************************
;; Higher Level functions
;;

(defn match-productkey [context accListID]
  (let [accList (accListID context)
        productKey (:productKey context)]
    (filter #(= (get % "productTypeKey") productKey) accList)))

(defn find-customer-account [context]
  (let [context1 (steps/apply-api get-deposit-accounts-api context :trans_details)
        resList (match-productkey context1 :trans_details)]
    (if (> (count resList) 0)
      (first resList)
      ;; No deposit account match look in loan accounts
      (let [context1 (steps/apply-api get-loan-accounts-api context :trans_details)
            resList2 (match-productkey context1 :trans_details)]
        (if (> (count resList2) 0)
          (first resList2)
          nil)))))

(defn find-customer-account-id [context]
  (let [res (find-customer-account context)]
    (when res (get res "id"))))


(defn get-payment-settlment-account [context]
(let [context1 (merge context {:productKey @PAYSETTLE-PROD-KEY})
  accid (find-customer-account-id context1)]
  {"id" accid}
  )
)

(defn create-annualrest-customer-and-accounts [num context]
  {:context (merge {:show-only false ; when true only prints steps, doesn't execute
             :verbose true ;; print out step :label(s)
             :first-name "AnnualRest", :last-name (str "Tester " num)
             :branchid "8a19c04783160b4c01831665a0220795"
             ;; **** Debug Only settings next - allows you to jump to individual steps
             :cust-key "8a19c1c8821634d6018217a3282e5798"
             :regsaver-id "OXVX863"
             :bonus-id "GSOT125"
             :custid "861492689"} context)
   :xjump-to-step [:step2 :one-only] ; set ":id :jump-here" in a step 
   :steps [;; [STEP-1] Create Customer
           {:label "[STEP-1] Create Customer"
            :request lpd/create-customer
            :post-filter [;(steps/save-last-to-context :cust-create)
                          (steps/save-part-to-context ["encodedKey"] :cust-key)
                          (steps/save-part-to-context ["id"] :custid)]}

           ;; [STEP-1b] Create CA
           {:label "Create credit-arrangement"
            :request create-credit-arrangement
            :post-filter [(steps/save-part-to-context ["id"] :ca-id)]}


           ;; [STEP-2] Create Create Annual Rest - Calculator
           {:label "[STEP-2] Create Annual Rest - Calculator"
            :id :step2
            :pre-filter [(steps/save-value-to-context @ARESTCALC-PROD-KEY :prod-key)
                         (steps/save-context-value-to-context :annual-interest-rate :interest-rate)
                         (steps/save-context-value-to-context :num-yearly-installments :num-installments)
                         (steps/save-value-to-context "AnnualRest Calculator" :acc-name)]
            :request create-installment-loan-api1
            :post-filter [(steps/save-part-to-context ["id"] :arest-calc-id)]}

           ;; [STEP-3] Create Create Annual Rest - PrincipalOnly
           {:label "[STEP-3] Create Create Annual Rest - PrincipalOnly"
            :pre-filter [(steps/save-value-to-context @ARESTPRIN-PROD-KEY :prod-key)
                         (steps/save-value-to-context "AnnualRest Principal" :acc-name)
                         (steps/save-value-to-context (:periodic-principal-payment context) :periodic-payment)
                         (steps/save-context-value-to-context :num-monthly-installments :num-installments)
                         (steps/save-value-to-context 0 :interest-rate)]
            :request create-installment-loan-api1
            :post-filter [(steps/save-part-to-context ["id"] :arest-prin-id)]}

          ;; [STEP-4] Create Create Annual Rest - InterestOnly
           {:label "[STEP-4] Create Create Annual Rest - InterestOnly"
            :pre-filter [(steps/save-value-to-context @ARESTINT-PROD-KEY :prod-key)
                         (steps/save-value-to-context nil :periodic-payment)
                         (steps/save-context-value-to-context :num-monthly-installments :num-installments)
                         (steps/save-context-value-to-context :annual-interest-rate :interest-rate)
                         (steps/save-value-to-context (- (:num-monthly-installments context) 1) :grace_period)

                         (steps/save-value-to-context "AnnualRest Interest" :acc-name)]
            :request create-installment-loan-api1
            :post-filter [(steps/save-part-to-context ["id"] :arest-int-id)]}


           ;; [STEP-4b] Add  Accounts to Credit-Arrangement
           {:label "Add accounts to credit-arrangement"
            :pre-filter [(steps/save-context-value-to-context :arest-prin-id :accid)]
            :request addLoanToCreditLine}
           {:pre-filter [(steps/save-context-value-to-context :arest-int-id :accid)]
            :request addLoanToCreditLine}


           ;; [STEP-5] Approve AnnualRest Account
           {:label "[STEP-5a] Approve Calculator"
            :pre-filter [(steps/save-context-value-to-context :arest-calc-id :loanAccountId)]
            :request ext/approveLoanAccount}
           {:label "[STEP-5b] Approve PrinOnly"
            :pre-filter [(steps/save-context-value-to-context :arest-prin-id :loanAccountId)]
            :request ext/approveLoanAccount}
           {:label "[STEP-5c] Approve IntOnly"
            :pre-filter [(steps/save-context-value-to-context :arest-int-id :loanAccountId)]
            :request ext/approveLoanAccount}

           {:label "[STEP-5d.1] Approve Payment/Settlement account"
            :request get-payment-settlment-account
            :post-filter [(steps/save-part-to-context ["id"] :accid)]}
           {:label "[STEP-5d.2] step2"
            :request lpd/approveDepositAccount}

            ;; [STEP-6] Disburse AnnualRest Account
           {:label "[STEP-6a] Disburse Calculator"
            :pre-filter [(steps/save-context-value-to-context :arest-calc-id :loanAccountId)
                         (steps/save-context-value-to-context :annual-start-date :value-date)
                         (steps/save-context-value-to-context :annual-first-date :first-date)]
            :request ext/disburse-loan-api}
           {:label "[STEP-6b] Disburse PrinOnly"
            :pre-filter [(steps/save-context-value-to-context :arest-prin-id :loanAccountId)
                         (steps/save-context-value-to-context :monthly-start-date :value-date)
                         (steps/save-context-value-to-context :monthly-first-date :first-date)]
            :request ext/disburse-loan-api}
           {:label "[STEP-6b] Disburse IntOnly"
            :pre-filter [(steps/save-context-value-to-context :arest-int-id :loanAccountId)
                         (steps/save-context-value-to-context :monthly-start-date :value-date)
                         (steps/save-context-value-to-context :monthly-first-date :first-date)]
            :request ext/disburse-loan-api}]})

(declare set-initial-payment-settings)
(defn create-new-annualrest-customer [num context0 ]
  (let [context (merge {:amount 250000
                        :credit-limit 1000000 ;; credit-arrangement max limit
                        :annual-interest-rate 5.5
                        :num-monthly-installments 300
                        :num-yearly-installments 25
                        :periodic-principal-payment 123
                        :grace_period 0} context0)
        res-obj (steps/process-collection (create-annualrest-customer-and-accounts num context))
        arest-calc-id (:arest-calc-id res-obj)
        arest-prin-id (:arest-prin-id res-obj)
        arest-int-id (:arest-int-id res-obj)
        custid (:custid res-obj)
        cust-key (:cust-key res-obj)
        _ (reset! ARESTCALC-ACCID arest-calc-id)
        _ (reset! PRINONLY-ACCID arest-prin-id)
        _ (reset! INTONLY-ACCID arest-int-id)
        _ (reset! CUSTID custid)
        _ (reset! CUSTKEY cust-key)
        _ (set-initial-payment-settings (:monthly-start-date context0))
        ]
    (prn (str "New Customer + AnnualRest Mortgage  created - custid=" custid " calc-account=" arest-calc-id " prin-account=" arest-prin-id " int-account=" arest-int-id)) 
    ))

(defn today-date []
  (.format (java.text.SimpleDateFormat. "yyyy-MM-dd") (new java.util.Date)))

(defn today-date+1 []
  (let [todayStr (today-date)
        parts (str/split todayStr #"-")
        year (first parts)
        month (second parts)
        day (nth parts 2)
        ;; need next line see https://clojuredocs.org/clojure.core/read-string 
        day (clojure.string/replace day #"^0+(?!$)" "")]
    (str year "-" month "-" (format "%02d" (+ (read-string day) 1)))))

(defn date+1 [change-date]
  (let [todayStr (subs change-date 0 10)
        parts (str/split todayStr #"-")
        year (first parts)
        month (second parts)
        day (nth parts 2)
        ;; need next line see https://clojuredocs.org/clojure.core/read-string 
        day (clojure.string/replace day #"^0+(?!$)" "")]
    (str year "-" month "-" (format "%02d" (+ (read-string day) 1)))))


(defn addtime [date-str]
  (ext/adjust-timezone2 (str date-str "T00:00:50" @DATEOFFSET) @TIMEZONE))

(defn add-month [date1 num]
  (let [date1-local (t/local-date "yyyy-MM-dd" (subs date1 0 10))]
    (str (t/adjust date1-local t/plus (t/months num))))) 

(defn get-next-loan-instalment-after [accid change-date]
  (let [sch-list (get-in  (inst/get-loan-schedule {:accid accid}) [:last-call "installments"])
        filt-list (inst/filter-schedule-after sch-list change-date)]
    (first filt-list)))

(defn get-previous-loan-instalment-before [accid change-date]
  (let [sch-list (get-in  (inst/get-loan-schedule {:accid accid}) [:last-call "installments"])
        filt-list (if change-date (inst/filter-schedule-before sch-list change-date)
                      sch-list)]
    (if change-date
      (first (reverse filt-list))
      ;; get 2nd to last from  filt-list   
      (first (rest (reverse filt-list))))))

(defn set-initial-payment-settings
  ([] (set-initial-payment-settings @ARESTCALC-ACCID @PRINONLY-ACCID @INTONLY-ACCID (today-date)))
  ([change-date] (set-initial-payment-settings @ARESTCALC-ACCID @PRINONLY-ACCID @INTONLY-ACCID change-date))
  ([calcid prinid _intid change-date]
   (let [inst-obj (get-next-loan-instalment-after calcid change-date) 
         prin-exp (get-in inst-obj ["principal" "amount" "expected"])
         int-exp (get-in inst-obj ["interest" "amount" "expected"])
         monthly-prin (/ prin-exp 12.0)
         monthly-int (/ int-exp 12.0)
         _ (prn (str "set-initial-payment-settings: prin-monthly-amount=" monthly-prin " int-monthly-amount=" monthly-int))]
     (call-api change-periodic-payment-api {:accid prinid
                                            :amount monthly-prin
                                            :value-date (addtime (date+1 change-date))}))
   @CUSTID))

;; On a yearly basis we will adjust the principal that the interest-only account is working against
(defn adjust-intonly-principal-amount [calcid intid change-date]
  (let [inst-obj (get-previous-loan-instalment-before calcid change-date)
        last-years-prin (get-in inst-obj ["principal" "amount" "expected"])
        _ (assert inst-obj (str "ERROR: adjust-intonly-principal-amount - change-date wrong: " change-date))]
    (call-api repayment-loan-api {:loanAccountId intid :amount last-years-prin :externalId (api/uuid)})))

;; Next function to be called for each annual-rest account yearly
(defn set-annual-payment-settings-update
  ([] (set-annual-payment-settings-update @ARESTCALC-ACCID @PRINONLY-ACCID @INTONLY-ACCID (today-date) true))
  ([change-date] (set-annual-payment-settings-update @ARESTCALC-ACCID @PRINONLY-ACCID @INTONLY-ACCID change-date true))
  ([change-date adjustIntOnly?] (set-annual-payment-settings-update @ARESTCALC-ACCID @PRINONLY-ACCID @INTONLY-ACCID change-date adjustIntOnly?))
  ([calcid prinid intid change-date adjustIntOnly?]
   (let [inst-obj (get-next-loan-instalment-after calcid change-date)  
         prin-exp (get-in inst-obj ["principal" "amount" "expected"])
         int-exp (get-in inst-obj ["interest" "amount" "expected"])
         monthly-prin (/ prin-exp 12.0)
         monthly-int (/ int-exp 12.0)
         _ (prn (str "set-annual-payment-settings-update: prin-monthly-amount=" monthly-prin " int-monthly-amount=" monthly-int))] 
     ;; Adjust the principal outstanding on the InterestOnly account
     (when adjustIntOnly? (adjust-intonly-principal-amount calcid intid change-date))
     ;; Change the monthly pricipal amount
     (prn "Change periodic payment amount")
     (call-api change-periodic-payment-api {:accid prinid
                                            :amount monthly-prin
                                            :value-date (addtime change-date)}))
   ;;@CUSTID
   ))

;; Next function changes the interest-rate associated with the annual-rest aaccount
(defn change-interest-rate [calcid intid change-date interest-rate]
  (prn (str "change-interest-rate - calculator account: " calcid " to " interest-rate " from " change-date))
  (call-api loan-interest-rate-change-api
            {:loanAccountId calcid ;; @ARESTCALC-ACCID @INTONLY-ACCID
             :interest-rate interest-rate
             :value-date (addtime change-date)})
  (prn (str "change-interest-rate - interest-only account: " intid " to " interest-rate " from " change-date))
  (call-api loan-interest-rate-change-api
            {:loanAccountId intid ;; @ARESTCALC-ACCID @INTONLY-ACCID
             :interest-rate interest-rate
             :value-date (addtime change-date)}))

(defn clear-outstanding-calculator-balances[]
;; TBD - Clear outstanding-balances on annual-rest calculator account
;; Not needed for POC
)

(defn filter-schedule-pending [loan-sch-list]
  (filter (fn [inst]
            (let [inst-state (get inst "state")]
              (or (= inst-state "PENDING") (= inst-state "LATE")))) loan-sch-list))

;; apply-to-end?=true only works for PrinOnly account
(defn repayment-against-principle [accid amount apply-to-end?]
  (let [
        ;;instal-list (get-in  (inst/get-loan-schedule {:accid accid}) [:last-call "installments"])
        ;;pending-list (and apply-to-end? (filter-schedule-pending instal-list))
        ;;last-but-one-inst (and apply-to-end? (first (rest (reverse pending-list))))
        ;;instal-update-key (when apply-to-end? (get last-but-one-inst "encodedKey"))
        ]
    ;;(call-api repayment-loan-api {:loanAccountId accid :amount amount :externalId (api/uuid) :installmentEncodedKey instal-update-key})
    (call-api repayment-loan-api {:loanAccountId accid :amount amount :externalId (api/uuid) :custom-prin-only apply-to-end?})
    
    ))

  (defn get-earliest-change-date [accid ]
    (let [instal-list (get-in  (inst/get-loan-schedule {:accid accid}) [:last-call "installments"])
          pending-list (filter-schedule-pending instal-list)
          first-pend-inst (first pending-list)
          dueDate (subs (get first-pend-inst "dueDate") 0 10)]
      dueDate))

;; Next function supports overpayments being applied to an annual-rest account
;; If the overpayment exceeds @OVERPAYMENT-THRESHOLD then a reduction in principal happens immediately
(defn process-overpayment [calcid prinid intid amount]
  (if (>= amount @OVERPAYMENT-THRESHOLD)
    (let [_ (prn "Applying an overpayment (above threshold)")
          _ (prn "Clear calculator balances")
          _ (clear-outstanding-calculator-balances)
          _ (prn "Update PrinOnly account")
          ;;_ (call-api repayment-loan-api {:loanAccountId prinid :amount amount :externalId (api/uuid)})
          _ (repayment-against-principle prinid amount true)
          _ (prn "Update IntOnly account")
          ;;_ (call-api repayment-loan-api {:loanAccountId intid :amount amount :externalId (api/uuid)})
          _ (repayment-against-principle intid amount false)
          _ (prn "Update Calculator account")
          ;;_ (call-api repayment-loan-api {:loanAccountId calcid :amount amount :externalId (api/uuid)})
           _ (repayment-against-principle calcid amount false)
          _ (prn "Update Payment Amount")
          earliest-change-date (get-earliest-change-date @PRINONLY-ACCID)
          ]

      (set-annual-payment-settings-update earliest-change-date false))
    ;; Below threshold, just reduce principal on prinOnly account
    (do
    (prn "Applying an overpayment (below threshold) - just update PrinOnly account")
    (call-api repayment-loan-api {:loanAccountId prinid :amount amount :externalId (api/uuid)}))))

(defn get-dates []
  {;;:annual-start-date (addtime (today-date))
   ;;:annual-first-date (ext/adjust-timezone2 (str "2023-01-01T13:37:50" @DATEOFFSET) @TIMEZONE)
   :annual-start-date (addtime "2022-01-01")
   :annual-first-date (addtime "2023-01-01")
   :monthly-start-date (addtime (today-date))
   :monthly-first-date (addtime (add-month (today-date) 1))})

(defn get-dates2 []
  {:annual-start-date (addtime "2022-01-01")
   :annual-first-date (addtime "2023-01-01")
   :monthly-start-date (addtime "2022-01-01")
   :monthly-first-date (addtime "2022-02-01")})

(api/setenv "env17")

;;********************************************************************************
;; Functions to execute to show AnnualRest lifecycle events

(comment

;; [0] Optional configure
;; Change the following if your tenant is on a different timezone
(reset! TIMEZONE "Europe/London")
(reset! DATEOFFSET "+1:00") ;; adjust this for daylight savings

;; [1] Setup a new AnnualRest customer
(create-new-annualrest-customer 21 (get-dates))

;; [2] Change interest-rate
(change-interest-rate @ARESTCALC-ACCID @INTONLY-ACCID "2023-01-01" 10.0)

;; [3] Apply the yearly update
(set-annual-payment-settings-update "2023-01-01")

;; [4] Process an overpayment (above or below threshold @OVERPAYMENT-THRESHOLD)
(process-overpayment @ARESTCALC-ACCID @PRINONLY-ACCID @INTONLY-ACCID 50000)

;; [5] Call next function to zap client and accounts
(zap-cust {:custid @CUSTID})
(zap-cust {:custid "798965158"})



;; [6] Testing a specific example scenario
(let [ 
      prin-on-20220101 56789.00 
      ;;prin-on-20220101 (- 56789.00 350.74)
      ](create-new-annualrest-customer 992 (merge (get-dates2) {:amount prin-on-20220101 :annual-interest-rate 5.44})))
(change-interest-rate @ARESTCALC-ACCID @INTONLY-ACCID "2022-04-01" 5.69)
(change-interest-rate @ARESTCALC-ACCID @INTONLY-ACCID "2022-10-01" 5.99)
(change-interest-rate @ARESTCALC-ACCID @INTONLY-ACCID "2022-12-15" 6.39)






;; Debug functions
(get-in  (inst/get-loan-schedule {:accid @PRINONLY-ACCID}) [:last-call "installments"])

(call-api ext/disburse-loan-api {:loanAccountId @INTONLY-ACCID :value-date (addtime "2021-01-01") :first-date (addtime "2021-02-01")})

(set-initial-payment-settings "2021-01-01")

(- 60000 1182.81)
(- 58817.19 56789)
;;

)