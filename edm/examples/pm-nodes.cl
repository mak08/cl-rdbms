;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Copyright      (c) michael 2012
;;; License
;;; Last Modified  <michael 2019-12-14 21:32:59>
 
(defpackage "TEST"
  (:use "COMMON-LISP" "SQL" "DBI"  "ALCM")
  (:shadow "class" "type" "length"))
 
(in-package test)
 
(setf logging:*logging* t)
(setf logging:*log-stream* t)
(setf logging:*log-level* 2)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
 
(defparameter *postgres-db*
  (sql:make-dbinfo :name "postgres"
                   :owner "postgres"))
(defparameter *postgres*
  (sql:make-userinfo :name "postgres"
                     :password "pg-07"))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *crm-db*
  (sql:make-dbinfo :name "crm"
                   :owner "crmadmin"))
(defparameter *crmadmin*
  (sql:make-userinfo :name "crmadmin"
                     :password "crmadmin"))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *user01*
  (sql:make-userinfo :name "user01"
                     :password "user01"))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defun redeploy (schema
                 &key
                 (database "crm")
                 (user *crmadmin*)
                 (redeploy nil))
  (alcm:update-schema database
                      user
                      (sql:get-schema-by-name schema)
                      :redeploy redeploy))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Product Model
 
(defentity (sales_order
            (:schema "pm"))
  (:field entrydate :type date)
  (:delegation responsible :target-entity employee)
  (:field total_amount :type money)
  (:field net_amount :type money) 
  (:field tax_rate :type decimal)
  (:delegation customer :target-entity person)
  (:delegation recipient :target-entity person)
  (:delegation bill_to :target-entity address)
  (:delegation ship_to :target-entity address) 
  (:composition admindata :target-entity admindata)
  (:composition items :target-entity  item :cardinality :many))
 
(defentity (item
            (:schema "pm"))
  (:field seqno :type integer)
  (:delegation admindata :target-entity admindata)
  (:reference product :target-entity product)
  (:field quantity :type decimal))
 
(defentity (admindata
            (:schema "pm"))
  (:field created :type timestamp :default-value "now")
  (:field last_changed :type timestamp :default-value "now"))
 
(defentity (person
            (:schema "pm")
            (:key "first$last" (firstname lastname)))
  (:field firstname :type (string 200))
  (:field lastname :type (string 200))
  (:field hobby :type (string 200) :default-value "Sailing")
  (:reference home_address :target-entity address :cardinality :one)
  (:reference ship_to_address :target-entity address :cardinality :one)
  (:reference addresses :target-entity address :cardinality :many))
 
(defentity (address
            (:schema "pm"))
  (:field street :type (string 200))
  (:field zipcode :type (string 6))
  (:field city :type (string 100)))
 
(defentity (employee
            (:schema "pm")
            (:key "name" (firstname lastname)))
  (:field firstname :type (string 200))
  (:field lastname :type (string 200)))
 
(defentity (product
            (:schema "pm"))
  (:field name :type identifier)
  (:field description :type (string 200))
  (:field version :type (string 200))
  (:field base_quantity :type (string 200) :screen-name "Base quantity")
  (:field sales_org :type (string 200) :screen-name "Sales org")
  (:reference model :target-entity product_model))
 
(defentity (product_model
            (:schema "pm")
            (:key "name" (name))
            (:key "class" (class)))
  (:field name :type identifier)
  (:field class :type (string 200))
  (:field version :type (string 200))
  (:delegation responsible :target-entity employee)
  (:delegation designer :target-entity employee)
  (:reference products :target-entity product :cardinality :many)
  (:reference properties :target-entity property :cardinality :many))
 
(defentity (property
            (:schema "pm")
            (:key "name" (name)))
  (:field name :type identifier)
  (:field description :type (string 200))
  (:delegation datatype :target-entity datatype))
 
(defentity (datatype
            (:schema "pm")
            (:key "name" (name)))
  (:field name :type (string 50))
  (:field type :type (string 50))
  (:field length :type integer))
 
(defentity (configuration
            (:schema "pm"))  
  (:field model :type identifier)
  (:field version :type identifier)
  (:composition valuations :target-entity valuation :cardinality :many))
 
(defentity (valuation
            (:schema "pm"))
  (:field value :type string)
  (:reference property :target-entity property :cardinality :one))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example
;;;
 
(setf *pm-schema*
      (dbi:create-db-schema "pm"))
 
(defun test ()
  (with-open-connection (x "crm")
    (with-open-connection (y "crm")
      (with-connection (x)
        (with-transaction (:isolation :repeatable-read)                                       
          (let ((i-x (retrieve-instance 'pm.property :name "Size" :lock-mode :none)))
            (format t "~a~%" "read X")
            ;; (retrieve-instance 'pm.property :name "Size" :lock-mode :update)
            ;; (format t "~a~%" "modify X")
            (with-connection (y)
              (with-transaction (:isolation :repeatable-read)                                 
                (let ((i-y (retrieve-instance 'pm.property :name "Size" :lock-mode :none)))
                  (format t "~a~%" "read Y")
                  (retrieve-instance 'pm.property :name "Size" :lock-mode :update)
                  (set-element i-x 'description "i-y")
                  (format t "~a~%" "modify Y"))))
            (format t "~a~%" "before modify X")
            (retrieve-instance 'pm.property :name "Size" :lock-mode :update)
            (format t "~a~%" "modify X")))))))
 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
