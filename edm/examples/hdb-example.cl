;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    Extended Entity Query Language
;;; Copyright      (c) Michael Kappert 2014
;;; Last Modified  <michael 2019-12-14 21:32:51>

(setf logging:*log-level* 3)
(setf *print-circle* t)

;; namespace sap.example;
;;  
;; @Schema: 'D037165'
;; context MyCompany {
;;  
;;     type MyName {
;;         first  : String(80);
;;         middle : String(80);
;;         last   : String(80);
;;     };
;;  
;;     entity MyAddress {
;;         key id      : Integer;
;;         employee_id : Integer;
;;         kind        : String(10);
;;         street      : String(80);
;;         number      : Integer;
;;         city        : String(80);
;;         zip         : Integer;
;;     };
;;  
;;     entity MyOrgunit {
;;         key id    : Integer;
;;         name      : String(80);
;;         boardarea : String(20);
;;         manager   : Association[0..1] to MyEmployee { id };
;;     };
;;  
;;     entity MyEmployee {
;;         key id  : Integer;
;;         name    : MyName;
;;         salary  : Decimal(22,2);
;;         address : Association[0..2] to MyAddress { employee_id };
;;         org     : Association[0..1] to MyOrgunit { id };
;;     };
;; };

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Translation of MyCompany example

(defpackage "MYCOMPANY"
  (:use  "COMMON-LISP" "SQL" "DBI"  "ALCM"))

(in-package mycompany)

(defentity (MyName
            (:schema "D037165"))
  (:field first :type (string 80))
  (:field middle :type (string 80))
  (:field last :type (string 80)))

(defentity (MyAddress
            (:schema "D037165"))
  (:field kind :type (string 10))
  (:field street :type (string 80))
  (:field number :type integer)
  (:field city :type (string 80))
  (:field zip :type integer))

(defentity (MyOrgunit
            (:schema "D037165")
            (:key "name_key" (name)))
  (:field name :type (string 80))
  (:field boardarea :type (string 20))
  (:reference manager :target-entity MyEmployee))

(defentity (MyEmployee
            (:schema "D037165")
            (:key "id_key" (id)))
  (:field id :type integer)
  (:field salary :type money)
  (:composition name :target-entity MyName)
  (:composition address :target-entity MyAddress :cardinality :many)
  (:reference org :target-entity MyOrgunit))

(defparameter *D037165-schema*
  (dbi:create-db-schema "D037165"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create test data

(defun refresh-data ()
  (alcm::clear-schema% (get-schema-by-name "D037165"))
  (let* ((org-units
           (list (create-instance 'D037165.myorgunit :name "Board" :boardarea "B")
                 (create-instance 'D037165.myorgunit :name "Sales" :boardarea "BS")
                 (create-instance 'D037165.myorgunit :name "Procurement" :boardarea "BP")))
         (addresses (list
                     (create-instance 'D037165.myaddress :street "Goethestrasse" :number 8 :city "Muenchen" :zip 8000 :kind "work")
                     (create-instance 'D037165.myaddress :street "Goethestrasse" :number 8 :city "Muenchen" :zip 8000 :kind "home")
                     (create-instance 'D037165.myaddress :street "Lessingstrasse" :number 7 :city "Muenchen" :zip 8000 :kind "home")))
         (e1
           (let* ((name (create-instance 'D037165.myname :first "Woody" :last "Woodpecker")))
             (create-instance 'D037165.myemployee :name name :address (list (first addresses) (second addresses)) :org (first org-units) :salary 125000)))
         (e2
           (let* ((name (create-instance 'D037165.myname :first "Mickey" :last "Mouse")))
             (create-instance 'D037165.myemployee :name name :address (list (first addresses) (third addresses)) :org (second org-units) :salary 42500)))
         (e3
           (let* ((name (create-instance 'D037165.myname :first "Donald" :last "Duck")))
             (create-instance 'D037165.myemployee :name name :address (list (first addresses) (third addresses)) :org (third org-units) :salary 34500))))
    (set-element (second org-units) 'manager e2)
    (set-element (third org-units) 'manager e3)
    (list e1 e2 e3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example Query

(defparameter *ee-query*
  ;; SELECT FROM Employee[org.boardarea = 'TIP' AND salary > '100000']
  ;;   { name,
  ;;     address[kind='home']{ street, city},
  ;;     salary
  ;;   }
  "D037165.MyEmployee[org.boardarea = 'B' AND salary > '100000'] { name, address[kind='home']{ street, city}, salary }")

(defun eequery (connection string)
  (sql:sql-query connection
                 (dbi::translate-query
                  (dbi::parse-ee-query string))))
   

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
