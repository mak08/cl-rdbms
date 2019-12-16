;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Copyright      (c)  2014
;;; Last Modified  <michael 2019-12-14 21:34:52>

(pg-socket:with-open-connection (c "crm" :user "crmadmin" ) 
  (with-connection (c)
    (with-transaction ()
      (?insert-into 'pm.person
                    :columns ['entity_id 'firstname 'lastname]
                    :values (list [(create-uuid) "Woody" "Woodpecker"]
                                  [(create-uuid) "Woody" "Allen"])))))

(pg-socket:with-open-connection (c "crm") 
  (with-connection (c)
    (with-transaction ()
      (create-instance 'pm.person :firstname "Woody" :lastname "Harrelson"))))

(pg-socket:with-open-connection (c "crm") 
  (with-connection (c)
    (with-transaction ()
      (retrieve 'pm.person))))

(pg-socket:with-open-connection (c "crm") 
  (with-connection (c)
      (setf woody (retrieve-instance 'pm.person :lastname "Allen"))))

(pg-socket:with-open-connection (c "crm" :user "crmadmin") 
  (sql:sql-query c "SELECT * FROM pm.person"))

(pg-socket:with-open-connection (c "crm" :user "crmadmin") 
  (with-connection (c)
    (with-transaction ()
      (setf wp (create-instance 'pm.product
                                :name "WebPhone2"
                                :description "Web Phone 2"
                                :version 1.0)))))


(pg-socket:with-open-connection (c "crm" :user "crmadmin") 
  (with-connection (c)
    (with-transaction ()
      (create-instance 'pm.sales_order 
                       :admindata (create-instance 'pm.admindata :created "now")
                       :items (loop for k below 5 collect (create-instance 'pm.item 
                                                                           :seqno (* k 1000)
                                                                           :admindata  (create-instance 'pm.admindata :created "now")))))))

(pg-socket:with-open-connection (c "crm") 
        (with-connection (c)
            (get-element so 'items)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transactions

(pg-socket:with-open-connection 
    (c "crm" :user "crmadmin" :password "crmadmin")
  (with-connection (c)
    (with-transaction (:isolation :serializable)
      (let ((sale-orders (retrieve 'pm.sales_order :lock-mode :update)))
        )
      ;; While this transaction runs, select ... for update nowait
      ;; will fail on all other connections.
      (break)
      ;; Now try to select the same rows on another connection
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Concurrent modification: share lock not granted because of existing uüdate lock

(pg-socket:with-open-connection 
    (first "crm" :user "crmadmin" :password "crmadmin")
  (pg-socket:with-open-connection 
      (second "crm" :user "crmadmin" :password "crmadmin")
    (with-connection (first)
      (with-transaction (:isolation :serializable)
        (retrieve 'pm.sales_order :lock-mode :update)
        (with-connection (second)
          (with-transaction (:isolation :serializable)
            (retrieve 'pm.sales_order :lock-mode :share)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Concurrent modification: two shared locks, no updates => ok

(pg-socket:with-open-connection 
    (first "crm" :user "crmadmin" :password "crmadmin")
  (pg-socket:with-open-connection 
      (second "crm" :user "crmadmin" :password "crmadmin")
    (with-connection (first)
      (with-transaction (:isolation :serializable)
        (retrieve 'pm.sales_order :lock-mode :share)
        (with-connection (second)
          (with-transaction (:isolation :serializable)
            (retrieve 'pm.sales_order :lock-mode :share)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Concurrent modificatio: two shared locks, second tx tries update
;;; ==> Caution, UPDATE will WAIT ! Use SELECT ... FOR UPDATE NOWAIT before the update! 

(pg-socket:with-open-connection 
    (first "crm" :user "crmadmin" :password "crmadmin")
  (pg-socket:with-open-connection 
      (second "crm" :user "crmadmin" :password "crmadmin")
    (with-connection (first)
      (with-transaction (:isolation :serializable)
        (retrieve 'pm.sales_order :lock-mode :share)
        (with-connection (second)
          (with-transaction (:isolation :serializable)
            (let ((sales-orders (retrieve 'pm.sales_order :lock-mode :share)))
              (set-element (first sales-orders) 'total_amount 150))))))))


(pg-socket:with-open-connection 
    (first "crm" :user "crmadmin" :password "crmadmin")
  (pg-socket:with-open-connection 
      (second "crm" :user "crmadmin" :password "crmadmin")
    (with-connection (first)
      (with-transaction (:isolation :serializable)
        (let ((so-1 (first
                     (retrieve 'pm.sales_order :lock-mode :share))))
          (with-connection (second)
            (with-transaction (:isolation :serializable)
              (let ((so-2 (first
                           (retrieve 'pm.sales_order :lock-mode :share))))
                (set-element (first sales-orders) 'total_amount 150)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; "Salesorder" performace test
(time
 (pg-socket:with-open-connection (c "crm" :user "crmadmin")
   (with-connection (c)
     (with-transaction ()
       (let* ((addresses (loop
                            for k below 5
                            collect (create-instance 'pm.address
                                                     :street (format () "Kurfürstendamm ~d" k)
                                                     :zipcode 1000
                                                     :city "berlin")))
              (woody (create-instance 'pm.person
                                      :firstname "Woody"
                                      :lastname "Woodpecker"
                                      :home_address (first addresses)
                                      :ship_to_address (second addresses)
                                      :addresses addresses))
              (webphone (create-instance 'pm.product
                                         :name "WebPhone")))
         (loop for k below 1000 do
              (create-instance 'pm.sales_order
                               :entrydate "now"
                               :customer woody
                               :ship_to (get-element woody 'ship_to_address)
                               :admindata (create-instance 'pm.admindata :created "now" :last_changed "now")
                               :items (list (create-instance 'pm.item
                                                             :seqno 1000
                                                             :admindata (create-instance 'pm.admindata :created "now")
                                                             :product webphone)))))))))
;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
