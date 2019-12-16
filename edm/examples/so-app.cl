;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    Demo: Application data model, application logic and demo data with CBAF
;;; Copyright      (c) michael 2012
;;; License
;;; Last Modified  <michael 2019-12-14 21:32:26>


(defpackage "TEST"
  (:use "COMMON-LISP" "SQL" "DBI"  "ALCM")
  (:shadow "class" "type" "length"))
 
(in-package test)
 
(setf logging:*logging* t)
(setf logging:*log-stream* t)
(setf logging:*log-level* 2)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DB users
  
(defparameter *crm-db*
  (sql:make-dbinfo :name "crm"
                   :owner "crmadmin"))
(defparameter *crmadmin*
  (sql:make-userinfo :name "crmadmin"
                     :password "crmadmin"))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entities: Sales Order, Item, Product, Admindata, Person, Employee, Address

(defentity (sales_order
            (:schema "so"))
  (:field entrydate :type date)
  (:field so_nr :type integer)
  (:reference responsible :target-entity employee)
  (:field total_amount :type money)
  (:field net_amount :type money) 
  (:field tax_rate :type decimal)
  (:reference customer :target-entity person)
  (:reference recipient :target-entity person)
  (:delegation bill_to :target-entity address)
  (:delegation ship_to :target-entity address)
  (:delegation admindata :target-entity admindata)
  (:composition items :target-entity  item :cardinality :many))

(defentity (purchase_order
            (:schema "so"))
  (:field entrydate :type date)
  (:reference responsible :target-entity employee)
  (:field total_amount :type money)
  (:delegation supplier :target-entity person)
  (:composition items :target-entity  po_item :cardinality :many)) 

(defentity (item
            (:schema "so"))
  (:field seqno :type integer)
  (:delegation admindata :target-entity admindata)
  (:delegation product :target-entity product)
  (:field quantity :type decimal))

(defentity (po_item
            (:schema "so"))
  (:field seqno :type integer)
  (:delegation admindata :target-entity admindata)
  (:delegation product :target-entity product)
  (:field quantity :type decimal))
 
(defentity (admindata
            (:schema "so"))
  (:field created :type timestamp :default-value "now")
  (:field last_changed :type timestamp :default-value "now"))
 
(defentity (person
            (:schema "so")
            (:key "first$last" (firstname lastname)))
  (:field firstname :type (string 200))
  (:field lastname :type (string 200))
  (:field hobby :type (string 200) :default-value "Sailing")
  (:delegation home_address :target-entity address :cardinality :one)
  (:delegation ship_to_address :target-entity address :cardinality :one)
  (:reference addresses :target-entity address :cardinality :many))
 
(defentity (address
            (:schema "so"))
  (:field street :type (string 200))
  (:field zipcode :type (string 6))
  (:field city :type (string 100)))
 
(defentity (employee
            (:schema "so")
            (:key "name" (firstname lastname)))
  (:field firstname :type (string 200))
  (:field lastname :type (string 200)))
 
(defentity (product
            (:schema "so"))
  (:field name :type identifier)
  (:field description :type (string 200))
  (:field version :type (string 200))
  (:field base_quantity :type (string 200) :screen-name "Base quantity")
  (:field sales_org :type (string 200) :screen-name "Sales org"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Backend implementation for Grid & Form UIs ("application layer")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Retrieve all sales order
(http:register-function '|list|)

(defun |list| (request response &rest args)
  (http:set-cookie response :|myID| "5")
  (setf (http:http-header response :|Content-Type|) "application/json")
  (pg-socket:with-open-connection (c "crm" :user "crmadmin")
    (with-connection (c)
      (let ((sales-orders (retrieve 'so.sales_order :lock-mode :share))
            (s (make-string-output-stream)))
        (dolist (so sales-orders)
          (get-element so 'responsible))
        (dolist (so sales-orders)
          (get-element so 'items))
        (http:json s sales-orders)
        (setf (http:http-body response) (get-output-stream-string s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Retrieve sales order instance 
(http:register-function '|get-sales-order|)

(defun |get-sales-order| (request response &rest args)
  (setf (http:http-header response :|Content-Type|) "application/json")
  (pg-socket:with-open-connection (c "crm" :user "crmadmin")
    (with-connection (c)
      (let ((sales-order (apply #'retrieve-instance 'so.sales_order :lock-mode :share args))
            (string-stream (make-string-output-stream)))
        (get-element sales-order 'responsible)
        (http:json string-stream sales-order)
        (setf (http:http-body response)
              (get-output-stream-string string-stream))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create the schema on load (no deployment)

(defparameter *so-schema*
  (dbi:create-db-schema "so"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Deploy or update schema & Create test data

;; (redeploy "so" "crm" "crmadmin" "crmadmin" :redeploy t)


(defun refresh-data (&key (redeploy nil))
  (pg-socket:with-open-connection (c "crm" :user "crmadmin")
    (with-connection (c)
      (with-transaction ()
        (if redeploy
            (alcm::update-schema% *so-schema* "crmadmin" :redeploy t)
            (alcm::clear-schema% *so-schema*))
        (logging:log-info "Creating Data...")
        (let ((logging:*log-level* 0))
          (let* ((addresses (loop
                               for k below 5
                               collect (create-instance 'so.address
                                                        :street (format () "Kurfürstendamm ~d" k)
                                                        :zipcode 1000
                                                        :city "berlin")))
                 (customer (create-instance 'so.person
                                            :firstname "Woody"
                                            :lastname "Woodpecker"
                                            :home_address (first addresses)
                                            :ship_to_address (second addresses)
                                            :addresses addresses))
                 (responsible (create-instance 'so.employee
                                            :firstname "Clerk"
                                            :lastname "Kent"))
                 (webphone (create-instance 'so.product
                                            :name "WebPhone")))
            (loop for k below 500 do
                 (create-instance 'so.sales_order
                                  :entrydate "now"
                                  :so_nr k
                                  :responsible responsible
                                  :customer customer
                                  :ship_to (get-element customer 'ship_to_address)
                                  :admindata (create-instance 'so.admindata :created "now" :last_changed "now")
                                  :total_amount (random 5000.0)
                                  :items (list (create-instance 'so.item
                                                                :seqno 1000
                                                                :admindata (create-instance 'so.admindata :created "now")
                                                                :product webphone
                                                                :quantity (1+ (random 100)))
                                               (create-instance 'so.item
                                                                :seqno 2000
                                                                :admindata (create-instance 'so.admindata :created "now")
                                                                :product webphone
                                                                :quantity (1+ (random 100))))))))
        (logging:log-info "Creating Data... done.")))))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
