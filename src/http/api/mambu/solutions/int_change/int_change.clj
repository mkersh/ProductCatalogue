;;; This library allows you to see the affects of a future interest-rate-change on variable-interest-rate loan-accounts
;;; See step [1] below
;;;
(ns http.api.mambu.solutions.int_change.int_change
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]
            [clojure.pprint :as pp]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [http.api.mambu.experiments.loan_schedule :as ext]))


;; Maps product-key(s) to the :preview-template to use
;; If no match then :default will be used (if defined)
;; TBD - If needed we could add support for interest-only products into this map also
(defonce PRODKEY_PREVIEW_MAP
  (atom
   {"8a19c83b8263a34b018264866b3622c3" {:preview-template "8a19c83b8263a34b0182643120c80b69"}
    "8a19dd48826ccb1201826df7fcf34214" {:preview-template "8a19dd48826ccb1201826dfc794b49dc"}
    :default {:preview-template "8a19c83b8263a34b0182643120c80b69"}}))

(comment 
(reset! PRODKEY_PREVIEW_MAP {"8a19c83b8263a34b018264866b3622c3" {:preview-template "8a19c83b8263a34b0182643120c80b69"}
                            "8a19dd48826ccb1201826df7fcf34214" {:preview-template "8a19dd48826ccb1201826dfc794b49dc"}
                            :default {:preview-template "8a19c83b8263a34b0182643120c80b69"}})
)

;; If you don't want to add notes to each loan-account then change this atom to false
(defonce ADD_BASERATE_CHANGE_NOTES (atom true)) ;; add baseerate changes to loan-account notes

;; Next atoms where used whilst testing
(defonce VALUE_DATE (atom nil))
(defonce FIRST_DATE (atom nil))
(defonce PRODUCT_KEY (atom "8a19c83b8263a34b018264866b3622c3"))
(defonce CUSTKEY (atom "8a19dd48826ccb1201826e02f7034a6d"))

;;--------------------------------------------------------------------
;; Print to CSV functions
;; So that you can view in a spreadsheet tool

(defonce BASERATE-UPDATES-ROOT (atom "BASERATE-UPDATES/"))

(defn get-file-path [fn]
  (str @BASERATE-UPDATES-ROOT fn))

(defn dump-sched-to-csv [instal-list]
  (let [next-instal (first instal-list) 
        rest-instal (rest instal-list)
        instal-num  (get next-instal "number")
        interest_expected (get-in next-instal ["interest" "amount" "expected"])
        principal_expected (get-in next-instal ["principal" "amount" "expected"])
        total-payment-due (+ (bigdec principal_expected) (bigdec interest_expected))
        due-date (subs (get next-instal "dueDate") 0 10)
        ]
    (println
     (str
      instal-num ","
      due-date ","
      interest_expected ","
      principal_expected "," 
      total-payment-due))
    ;; recurse to next line
    (when (not-empty rest-instal) (dump-sched-to-csv rest-instal))))

(defn save-to-csv-file [fn inst-list]
  (let [fpath (get-file-path fn)]
    (io/make-parents fpath)
    (spit fpath "" :append false)
    (with-open [out-data (io/writer fpath)]
      (binding [*out* out-data]
        (println "#, DueDate, Interest Expected, Principal Expected, Total Amount Due")
        (dump-sched-to-csv inst-list)))))


(defn get-mini-schedule-notes
  ([change-date base-rate-change instal-list]
   (get-mini-schedule-notes change-date base-rate-change instal-list 0
                            (str "#base-rate-change-start(" change-date ")<br/>"
                                 "rate-change = " base-rate-change "%:<br/>"
                                 "Next 6 months schedule:"
                                 "<table><tr><th>#</th> <th>due-date</th> <th>interest</th> <th>principal</th> <th>total-payment</th> </tr>")))
  ([change-date base-rate-change instal-list cc notes-str]
   (let [next-instal (first instal-list)
         rest-instal (rest instal-list)
         instal-num  (get next-instal "number")
         interest_expected (get-in next-instal ["interest" "amount" "expected"])
         principal_expected (get-in next-instal ["principal" "amount" "expected"])
         total-payment-due (+ (bigdec principal_expected) (bigdec interest_expected))
         due-date (subs (get next-instal "dueDate") 0 10)
         notes-str (str notes-str "<tr><td>"  instal-num "</td><td>" due-date "</td><td>" interest_expected "</td><td>" principal_expected "</td><td>" total-payment-due "</td></tr>")]
    ;; recurse to next line
     (if (and (not-empty rest-instal) (< cc 5))
       (get-mini-schedule-notes change-date base-rate-change rest-instal (inc cc) notes-str)
       (str notes-str "<tr><td>...</td><td></td><td></td><td></td><td></td><tr></table>"
            "#base-rate-change-end"  "<br/><br/>")))))

(defn remove-previous-base-rate-note [change-date]
  (fn [notes]
    (if notes (let [st-str (str "#base-rate-change-start(" change-date ")<br/>")
                    end-str "#base-rate-change-end<br/><br/>"
                    st-pos (str/index-of notes st-str 0)
                    end-pos (if st-pos (str/index-of notes end-str st-pos) nil)]
                (if (and st-pos end-pos)
                  (let [str1 (subs notes 0 st-pos)
                        str2 (subs notes (+ end-pos (count end-str)))]
                    (str str1 str2))
                  notes))
        notes)))

(defn delete-directory-recursive
  "Recursively delete a directory."
  [^java.io.File file]
  ;; when `file` is a directory, list its entries and call this
  ;; function with each entry. can't `recur` here as it's not a tail
  ;; position, sadly. could cause a stack overflow for many entries?
  (when (.isDirectory file)
    (doseq [file-in-dir (.listFiles file)]
      (delete-directory-recursive file-in-dir)))
  ;; delete the file or directory. if it it's a file, it's easily
  ;; deletable. if it's a directory, we already have deleted all its
  ;; contents with the code above (remember?)
  (io/delete-file file))

(defn delete-BASERATE-UPDATES-ROOT []
  (try (delete-directory-recursive (clojure.java.io/file @BASERATE-UPDATES-ROOT))
       (catch Exception _ "Nothing to DELETE")))

;;--------------------------------------------------------------------
;; Mambu-core API endpoints
;;


(defn create-installment-loan-api [context]
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
(defn create-loan-account [accname options]
  (let [res (steps/apply-api create-installment-loan-api
                             {:cust-key (:cust-key options)
                              :prod-key (:prod-key options)
                              :amount (:amount options)
                              :periodic-payment (:periodic-payment options)
                              :acc-name accname
                              :interest-rate-spread (:interest-rate-spread options)
                              :grace_period (:grace_period options)
                              :num-installments (:num-installments options)})
        id (get-in res [:last-call "id"])]
    id))

(defn search-for-loan-api [context]
  {:url (str "{{*env*}}/loans:search")
   :method api/POST
   :query-params {"detailsLevel" "FULL"}
   :body {"filterCriteria" [{"field" "productTypeKey"
                             "operator" "EQUALS"
                             "value" (:prod-key context)}]}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-all-loans-api [_context]
  {:url (str "{{*env*}}/loans")
   :method api/GET
   :query-params {"detailsLevel" "FULL"} 
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-loan-account [context]
  {:url (str "{{*env*}}/loans/" (:accid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn patch-loan-notes-api [context]
  (let [clear-previous-notes (:clear-previous-notes context)
        filter-fn (:filter-function context)
        add-note (:add-note context)
        old-notes (if clear-previous-notes "" (steps/call-api get-loan-account context "notes"))
        old-notes (if filter-fn (filter-fn old-notes) old-notes)
        new-notes (str add-note "<br/>" old-notes)]
    {:url (str "{{*env*}}/loans/" (:accid context))
     :method api/PATCH
     :body  [{"op" "REPLACE"
              "path" "notes"
              "value" new-notes}]
     :headers {"Accept" "application/vnd.mambu.v2+json"
               "Content-Type" "application/json"}}))


    

(defn get-loan-schedule [context]
  (let [api-call (fn [context0]
                   {:url (str "{{*env*}}/loans/" (:accid context0) "/schedule")
                    :method api/GET
                    :query-params {"detailsLevel" "FULL"}
                    :headers {"Accept" "application/vnd.mambu.v2+json"
                              "Content-Type" "application/json"}})]
    (steps/apply-api api-call context)))

(defn get-product-schedule-preview [context]
  {:url (str "{{*env*}}/loans:previewSchedule")
   :method api/POST
   :query-params {}
   :body {"disbursementDetails" {"expectedDisbursementDate" (:disbursement-date context)
                                 "firstRepaymentDate" (:first-payment-date context)}
          "loanAmount" (:amount context)
          "productTypeKey" (:template-product context)
          "interestSettings" {"interestRate" (:interest-rate context)},
          "scheduleSettings" {"gracePeriod" 0
                              "periodicPayment" (:periodic-amount context)
                              "repaymentInstallments" (:num-instalments context)
                              "repaymentPeriodCount" 1
                              "repaymentPeriodUnit" "MONTHS"}}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

;;--------------------------------------------------------------------
;; Functions to create test data - only need for testing
;;

(defn create-test-variable-rate-loans [num1]
  (loop [cnt 0]
    (if (= cnt num1) true
        (let  [accid (create-loan-account (str "TestVarIntRate" cnt)
                                          {:cust-key @CUSTKEY
                                           :prod-key @PRODUCT_KEY
                                           :amount 100000.00
                                           :periodic-payment nil
                                           :interest-rate-spread 0
                                           :grace_period 0
                                           :num-installments 300})
               _ (prn (str "Created a new loan " accid))
               _ (steps/apply-api ext/approveLoanAccount {:loanAccountId accid})
               ;;_ (prn "Approved")
               _ (steps/apply-api ext/disburse-loan-api {:loanAccountId accid :value-date @VALUE_DATE :first-date @FIRST_DATE})
               ;;_ (prn "Disbursed")
               ] 
          (recur (inc cnt))))))

;;--------------------------------------------------------------------
;; Higher level functions to update a loan-schedule
;; See get-loan-schedule-updates-after-baserate-change and work upwards
;;

;; calculate the expected-principal, either for an entire loan-account ( if int-rate-change-date=nil)
;; or upto a certain int-rate-change-date
(defn calculate-principal-from-schedule [loan-sch-list int-rate-change-date]
  (loop [prin-total 0
         inst-list loan-sch-list]
    (if (empty? inst-list) prin-total
        (let [inst (first inst-list)
              inst-num (get inst "number")
              prin-exp (bigdec (get-in inst ["principal" "amount" "expected"]))
              due-date (get inst "dueDate")
              new-prin-total (+ prin-total prin-exp)
              ;; _ (prn (str inst-num " due-date= " due-date " prin-expected= " prin-exp " total= " new-prin-total))
              ]

          (if (or (nil? int-rate-change-date)
                  (<= (compare due-date int-rate-change-date) 0))
            (recur new-prin-total (rest inst-list))
            new-prin-total)))))

;; calculate the principal-remaining at a certain int-rate-change-date
;; NOTE: This is the expected principal-remaining according to the contract, which is represented by the loan-sch-list
(defn principal-remain-at-date [loan-sch-list int-rate-change-date]
  (let [total-principal (calculate-principal-from-schedule loan-sch-list nil)
        prin-expected-at-date (calculate-principal-from-schedule loan-sch-list int-rate-change-date)
        prin-remain-at-date (- total-principal prin-expected-at-date)]
    prin-remain-at-date))


;; function to filter a loan-sch-list based on due-date of the instalments
;; Depending on the op passed you can use to return instals before a change-date or after
(defn filter-schedule [loan-sch-list int-rate-change-date op]
  (filter (fn [inst]
            (let [inst-due-date (get inst "dueDate")
                  inst-due-date (subs inst-due-date 0 10)]
              (op (compare inst-due-date int-rate-change-date) 0))) loan-sch-list))

;; return all the instalments after a given int-rate-change-date
(defn filter-schedule-after [loan-sch-list int-rate-change-date]
  (filter-schedule loan-sch-list int-rate-change-date >))

;; return all the instalments before or equal to a given int-rate-change-date
(defn filter-schedule-before [loan-sch-list int-rate-change-date]
  (filter-schedule loan-sch-list int-rate-change-date <=))

;; function to allow us to filter an acc-list based on attr-selector-list
(defn filter-accounts [acc-list attr-selector-list attr-comp-func]
  (filter (fn [acc]
            (let [acc-attr-val (get-in acc attr-selector-list)]
              (attr-comp-func acc-attr-val)))
          acc-list))

;; Modify the instalment-number of all the intalments in inst-list
(defn inc-instal-number [inst-list inc-num-str]
  (let [inc-num (Integer/parseInt inc-num-str)
        inc-num (dec inc-num)]
    (map (fn [inst]
           (let [inst-num-str (get inst "number")
                 inst-num (Integer/parseInt inst-num-str)
                 ;;_ (prn "updating " inst-num)
                 updated-inst-num (+ inst-num inc-num)]

             (assoc inst "number" (str updated-inst-num)))) inst-list)))

(defn get-product-preview-template [product-key]
  (let [preview-template (get @PRODKEY_PREVIEW_MAP product-key)]
    (if preview-template (:preview-template preview-template)
        (let [default-template (get @PRODKEY_PREVIEW_MAP :default)]
          (assert default-template (str "ERROR: Cannot find a preview-template for product=" product-key))
          (prn (str "Using default preview-template for " product-key))
          (:preview-template default-template))
        )))

;; Next function calculates the loan-schedule changes for a given loan-account
(defn get-loan-schedule-update [change-date base-rate-change]
  (fn [ret loan-acc]
    (let
     [accid (get loan-acc "id")
      product-key (get loan-acc "productTypeKey")
      ;;_ (prn "loan account details" product-key)
      ;;_ (pp/pprint loan-acc)
      ;;_ (assert false)
      sch-list (get-in  (get-loan-schedule {:accid accid}) [:last-call "installments"]) 
      filt-list (filter-schedule-after sch-list change-date)]
      (when (seq filt-list)
        (let [filt-list-before (filter-schedule-before sch-list change-date) 
              first-inst-to-change (first filt-list)
              inst-num (get first-inst-to-change "number")
              first-payment-date (get (first filt-list) "dueDate")
              inst-count (count filt-list)
              current-interest-rate (get-in loan-acc ["interestSettings" "interestRate"])
              interest-spread (get-in loan-acc ["interestSettings" "interestSpread"])
              interest-rate (+ current-interest-rate base-rate-change interest-spread)
              contract-principal-remain (principal-remain-at-date sch-list change-date)
              ;; use a preview-product linked to the actual product to calculate the new loan-schedule after the base-rate-change
              schedule-updates (steps/call-api get-product-schedule-preview {:template-product (get-product-preview-template product-key)
                                                                             :disbursement-date  (ext/adjust-timezone2 (str  change-date "T13:37:50+01:00") "Europe/London")
                                                                             :first-payment-date first-payment-date
                                                                             :amount contract-principal-remain
                                                                             :interest-rate interest-rate
                                                                             :num-instalments inst-count})
              updates-inst-list (get schedule-updates "installments")
              updates-inst-list (inc-instal-number updates-inst-list inst-num)
              merged-inst-list (concat filt-list-before updates-inst-list)
              ;; preview does not include late-interest. So backdated changes not supported
              ;;_ (save-to-csv-file (str accid "/" accid "-preview.csv") updates-inst-list)
              _ (save-to-csv-file (str accid "/" accid "-current.csv") sch-list) 
              _ (save-to-csv-file (str accid "/" accid "-update-" change-date ".csv") merged-inst-list) 
              update-notes (get-mini-schedule-notes change-date base-rate-change updates-inst-list)
              _   (when @ADD_BASERATE_CHANGE_NOTES (steps/call-api patch-loan-notes-api {:accid accid :add-note update-notes :filter-function (remove-previous-base-rate-note change-date)}))]
          (prn (str "Updating loan " accid " Modifying " inst-count " instalments, principal " contract-principal-remain " first instalment to change " inst-num))
          ;;(pp/pprint merged-inst-list)
          ret)))))


(defn get-single-loan-schedule-update [change-date base-rate-change accid]
(let [acc-detail (steps/call-api get-loan-account {:accid accid})]
  ((get-loan-schedule-update change-date base-rate-change) nil acc-detail))
)

;; produce loan-schdule updates for all variable-interest-rate loans
;; NOTE: Assuming that all variable-interest-rate loans are affected by a base-rate-change
;;       If this is not the case, we would need to adjust baserate-loans below
;; updates stored in "BASERATE-UPDATES" folder and (optionally) as a note against each loan-account
(defn get-loan-schedule-updates-after-baserate-change [change-date base-rate-change]
  (let [all-loans (steps/call-api get-all-loans-api {})
        active-loans (filter-accounts all-loans ["accountState"] (fn [acc-attr-val] (= (subs acc-attr-val 0 6) "ACTIVE")))
        baserate-loans  (filter-accounts active-loans ["interestSettings" "interestRateSource"] (fn [acc-attr-val] (= acc-attr-val "INDEX_INTEREST_RATE") ))
        ]
    (reduce (get-loan-schedule-update change-date base-rate-change) nil baserate-loans)))

;; [0] Next line sets the Mambu tenant to use
(api/setenv "env17")

(comment
  ;; [1]  Apply an interest-rate change to all variable-interest-rate loans
  ;; WARNING: Only works for future dated interest-rate changes because of limitations with get-product-schedule-preview
  ;; See "BASERATE-UPDATES" folder for output
  ;; Notes also added to each loan-account effected (if @ADD_BASERATE_CHANGE_NOTES=true)
  (get-loan-schedule-updates-after-baserate-change "2022-09-01" 0.5)
  ;; [1.2] Next function will get the updates for a single loan-account
  (get-single-loan-schedule-update "2022-09-01" 0.5 "VMPL903")
  ;; [1.2] control whether notes are added to the loan-account
  (reset! ADD_BASERATE_CHANGE_NOTES true)
  (reset! ADD_BASERATE_CHANGE_NOTES false)

  ;; [1.3]  Do a 0% interest-rate change to prove that current.csv and update.csv files are the same
  ;; i.e. prove that my method/approach works
  (get-loan-schedule-updates-after-baserate-change "2022-09-01" 0.0)


  ;; [2] (optional) create some test loan to test performance of the above
  (reset! CUSTKEY "8a19dd48826ccb1201826e02f7034a6d") ;; 649579292 on seukdemo
  (reset! VALUE_DATE (ext/adjust-timezone2 (str "2021-01-01T00:00:50+01:00") "Europe/London")) ;; Change these dates as required
  (reset! FIRST_DATE (ext/adjust-timezone2 (str "2021-02-01T13:37:50+01:00") "Europe/London"))
  (create-test-variable-rate-loans 10)
  ;; If you want to delete all loans attached to a customer
  (ext/zap-all-loans2 @CUSTKEY)

  ;;--------------------------------------------------------------------
  ;; Tests used whilst debugging/developing
  ;;


  ;; clear the updates folder 
  (delete-BASERATE-UPDATES-ROOT)
  @BASERATE-UPDATES-ROOT


  (steps/call-api search-for-loan-api {:prod-key "8a19d7a57f87b3e5017f8d18cdd558a1"})

  (steps/call-api get-all-loans-api {})

  (reset! VALUE_DATE (ext/adjust-timezone2 (str "2022" "-08-03T00:00:50+01:00") "Europe/London")) ;; Change these dates as required
  (reset! FIRST_DATE (ext/adjust-timezone2 (str "2022" "-09-03T13:37:50+01:00") "Europe/London"))

  (steps/call-api get-product-schedule-preview {:template-product (:preview-template (:default @PRODKEY_PREVIEW_MAP))
                                                :disbursement-date @VALUE_DATE
                                                :first-payment-date @FIRST_DATE
                                                :amount 10000
                                                :interest-rate 5.9
                                                :num-instalments 12})


  (steps/call-api get-loan-account {:accid "VMPL903"})
  (get-loan-schedule {:accid "VMPL903"})
  (steps/call-api patch-loan-notes-api {:accid "VMPL903" :add-note "" :clear-previous-notes true}) ;; clear notes
  (steps/call-api patch-loan-notes-api {:accid "VMPL903" :add-note "**Line 1 notes" :clear-previous-notes true})
  (steps/call-api patch-loan-notes-api {:accid "VMPL903" :add-note "**Line 2 notes" :clear-previous-notes false})
  (steps/call-api patch-loan-notes-api {:accid "VMPL903" :add-note "**Line 3 notes" :clear-previous-notes false})


  (let
   [sch-list (get-in  (get-loan-schedule {:accid "VMPL903"}) [:last-call "installments"])
    prin-remain (principal-remain-at-date sch-list "2015-11-01")]

    (prn "Principal Remain: " prin-remain))


 ;; 
  )