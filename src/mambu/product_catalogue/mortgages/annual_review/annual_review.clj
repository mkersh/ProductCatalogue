(ns mambu.product_catalogue.mortgages.annual_review.annual_review 
 (:require [http.api.json_helper :as api]
           [http.api.api_pipe :as steps]
           [http.api.mambu.demo.loyalty_points :as lpd]
           [http.api.mambu.experiments.loan_schedule :as ext]
           [http.api.mambu.solutions.int_change.int_change :as inst]
           [mambu.product_catalogue.mortgages.annual_rest.annual_rest :as rest]))

(def AREVCALC-PROD-KEY (atom "8a19a6bb8321062b0183216c27e14354")) 
(def AREVMAIN-PROD-KEY (atom "8a19a6bb8321062b018321c386ea1ba9")) 
(defonce AREV-ACCID (atom nil))
(defonce CUSTID (atom nil))
(defonce CUSTKEY (atom nil))


;;*************************************************************
;; Higher Level functions
;;

(defn create-annualreview-customer-and-accounts [num context]
  {:context (merge {:show-only false ; when true only prints steps, doesn't execute
                    :verbose true ;; print out step :label(s)
                    :throw-errors true
                    :first-name "AnnualReview", :last-name (str "Tester " num)
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

           
           ;; [STEP-2] Create  AnnualReview account
           {:label "[STEP-2] Create Create Annual Review account"
            :pre-filter [(steps/save-value-to-context @AREVMAIN-PROD-KEY :prod-key)
                         (steps/save-value-to-context "AnnualReview" :acc-name) 
                         (steps/save-context-value-to-context :num-monthly-installments :num-installments)
                         (steps/save-context-value-to-context :annual-interest-rate :interest-rate)]
            :request rest/create-installment-loan-api1
            :post-filter [(steps/save-part-to-context ["id"] :arev-accid)]}

          
           ;; [STEP-3] Approve AnnualReview Account
           {:label "[STEP-3] Approve AnnualReview Account"
            :pre-filter [(steps/save-context-value-to-context :arev-accid :loanAccountId)]
            :request ext/approveLoanAccount}
           
            ;; [STEP-4] Disburse AnnualReview Account
           {:label "[STEP-4] Disburse Calculator"
            :pre-filter [(steps/save-context-value-to-context :arev-accid :loanAccountId)
                         (steps/save-context-value-to-context :loan-start-date :value-date)
                         (steps/save-context-value-to-context :loan-first-date :first-date)]
            :request ext/disburse-loan-api}]})
           
(declare get-dates)
(defn get-periodic-amount [calc-template amount int-rate num-inst-remain]
  (let [inst-list (get (steps/call-api inst/get-product-schedule-preview
                                       {:template-product calc-template
                                        :disbursement-date  (:loan-start-date (get-dates))
                                        :first-payment-date  (:loan-first-date (get-dates))
                                        :amount amount
                                        :interest-rate int-rate
                                        :num-instalments num-inst-remain}) "installments")
        inst-obj (first inst-list)
        prn-amt (get-in inst-obj ["principal" "amount" "expected"])
        inr-amt (get-in inst-obj ["interest" "amount" "expected"])
        monthly-amount (read-string (api/round-num (+ prn-amt inr-amt)))]
    monthly-amount))


(defn create-new-annualreview-customer [num context0]
  (let [context (merge {:amount 250000
                        :annual-interest-rate 5.5
                        :num-monthly-installments 300 
                        :grace_period 0} context0)
        periodic-amount (get-periodic-amount @AREVCALC-PROD-KEY (:amount context) (:annual-interest-rate context) (:num-monthly-installments context))
        context (merge context {:periodic-payment periodic-amount})
        res-obj (steps/process-collection (create-annualreview-customer-and-accounts num context))
        arev-accid (:arev-accid res-obj)
        custid (:custid res-obj)
        cust-key (:cust-key res-obj)
        _ (reset! AREV-ACCID arev-accid)
        _ (reset! CUSTID custid)
        _ (reset! CUSTKEY cust-key)]
    (prn (str "New Customer + AnnualReview Mortgage  created - custid=" custid " arv-account=" arev-accid))))



(defn principal-paid [filt-list-before]
  (let [prin-paid-total (reduce
                         (fn [res inst-obj]
                           (let [prin-paid (get-in inst-obj ["principal" "amount" "paid"])]
                             (+ res prin-paid)))
                         0 filt-list-before)
        prin-paid-total (read-string (api/round-num prin-paid-total))]
    prin-paid-total))

;; Determine if customer monthly-payments need to change
(defn annual-review-account-update [accid change-date]
  (let [sch-list (get-in  (inst/get-loan-schedule {:accid accid}) [:last-call "installments"])
        filt-list-after (inst/filter-schedule-after sch-list change-date)
        ;;filt-list-before (inst/filter-schedule-before sch-list change-date)
        ;;filt-list-before (reverse (rest (reverse filt-list-before)))
        ;;num-instals-before (count filt-list-before)
        num-instals-after (+ 1 (count filt-list-after))
        ;;principal-paid (principal-paid filt-list-before)
        acc-obj (steps/call-api rest/get-loan-account {:accid accid})
        principal-remain (get-in acc-obj ["balances" "principalBalance"])
        cur-periodic-amount (get-in acc-obj ["scheduleSettings" "periodicPayment"])
        cur-int-rate (get-in acc-obj ["interestSettings" "interestRate"])
        periodic-amount (get-periodic-amount @AREVCALC-PROD-KEY principal-remain cur-int-rate num-instals-after)
        _ (prn "num-instals-left:" num-instals-after)
        _ (prn "principal-remain:" principal-remain)
        _ (prn (str "cur-periodic-amount=" cur-periodic-amount " NEW periodic-amount " periodic-amount))
        ;;_ (assert false "STOP WHILST TESTING")
        ] 
    (when (not= cur-periodic-amount periodic-amount)
      (steps/call-api rest/change-periodic-payment-api
                      {:accid accid
                       :amount periodic-amount
                       :value-date (rest/addtime change-date)})))) 

(defn get-dates []
  {:loan-start-date (rest/addtime "2022-01-01")
   :loan-first-date (rest/addtime "2022-02-01")})
   


(api/setenv "env17")

(comment 

;; [1] Setup a new AnnualReview customer
 (create-new-annualreview-customer 3 (get-dates))

;; [2] Perform an annual review update
(annual-review-account-update @AREV-ACCID "2023-01-01")
;; [2.1] Simulate repaymeents being made prior to revieew
(steps/call-api rest/repayment-loan-api {:loanAccountId @AREV-ACCID :amount 20000.00 :externalId (api/uuid)})


;; [3] Call next function to zap client and accounts
 (rest/zap-cust {:custid @CUSTID})



;; ****************************************************
;; Function calls used whilst testing
;;
(steps/call-api rest/create-installment-loan-api1
       {:amount 250000
        :acc-name "AnnualReview"
        :cust-key @CUSTKEY
        :prod-key @AREVMAIN-PROD-KEY
        :interest-rate 5.5
        :periodic-payment 1535.21
        :grace_period 0
        :num-installments 300})


 (get-periodic-amount @AREVCALC-PROD-KEY 250000 5.5 300)
 (steps/call-api inst/get-product-schedule-preview {:template-product @AREVCALC-PROD-KEY
                                                    :disbursement-date  (:loan-start-date (get-dates))
                                                    :first-payment-date  (:loan-first-date (get-dates))
                                                    :amount 250000
                                                    :interest-rate 5.5
                                                    :num-instalments 300})
 )

                