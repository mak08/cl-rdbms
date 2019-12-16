;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Copyright      (c) michael 2012
;;; License
;;; Last Modified  <michael 2019-12-14 21:33:45>

(defpackage "TEST"
  (:use "COMMON-LISP" "ALCM" "DBI")
  (:shadow "class" "type" "length"))

(in-package test)

(setf logging:*logging* t)
(setf logging:*log-level* 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Product Model

(defentity (sales_order
            (:schema "so"))
  (:field entrydate :type date)
  (:field duedate :type date)
  (:composition admindata :target-entity admindata)
  (:reference  items :target-entity item :cardinality :many))

(defentity (item
          (:schema "so"))
  (:field seqno :type integer)
  (:field unit :type (string 3))
  (:reference product :target-entity product)
  (:field quantity :type decimal))

(defentity (admindata
          (:schema "so"))
  (:delegation responsible :target-entity employee)
  (:delegation customer :target-entity person))

(defentity (product
          (:schema "so"))
  (:field name :type identifier)
  (:field description :type (string 200))
  (:field version :type (string 200))
  (:field base_quantity :type (string 200) :screen-name "Base quantity")
  (:field sales_org :type (string 200) :screen-name "Sales org"))

(defentity (person
          (:schema "so")
          (:key "name" (firstname lastname birthday))
          (:key "id" (id)))
  (:field id :type (string 32))
  (:field firstname :type (string 200))
  (:field lastname :type (string 200))
  (:field birthday :type (string 200)))

(defentity (employee
          (:schema "so")
          (:key "name" (firstname lastname)))
  (:field firstname :type (string 200))
  (:field lastname :type (string 200)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example
;;; 

(setf *so-schema*
      (create-db-schema "so"))

#|

(with-open-connection (c "crm")
  (with-connection (c)
    (with-transaction (:isolation :repeatable-read)
      (create-instance 'so.sales_order ))))

(with-open-connection (c "crm")
  (with-connection (c)
    (with-transaction (:isolation :repeatable-read)
      (?select '* :from 'so.sales_order ))))

(setf so
      (with-open-connection (c "crm")
        (with-connection (c)
          (with-transaction (:isolation :repeatable-read)
            (retrieve-instance 'so.sales_order :node_id "1a96808e-0aa0-4462-9da8-4ca0f8c1b698")))))

(with-open-connection (c "crm")
  (with-connection (c)
    (with-transaction (:isolation :repeatable-read)
      (get-element so 'items))))

|#

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
