;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    Extended Entity Query Language
;;; Copyright      (c) Michael Kappert 2014
;;; Last Modified  <michael 2019-12-14 21:32:41>

(setf logging:*log-level* 2)
(setf *print-circle* t)

;; namespace sap.example;
;;  
;; @Schema: 'ABC'
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
            (:schema "ABC"))
  (:field first :type (string 80))
  (:field middle :type (string 80))
  (:field last :type (string 80)))

(defentity (MyAddress
            (:schema "ABC"))
  (:field kind :type (string 10))
  (:field street :type (string 80))
  (:field number :type integer)
  (:field city :type (string 80))
  (:field zip :type integer))

(defentity (MyOrgunit
            (:schema "ABC")
            (:key "name_key" (name)))
  (:field name :type (string 80))
  (:field boardarea :type (string 20))
  (:reference manager :target-entity MyEmployee))

(defentity (MyEmployee
            (:schema "ABC")
            (:key "id_key" (id)))
  (:field id :type integer)
  (:field salary :type money)
  (:composition name :target-entity MyName)
  (:composition address :target-entity MyAddress :cardinality :many)
  (:reference org :target-entity MyOrgunit))

(defparameter *abc-schema*
  (dbi:create-db-schema "ABC"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create test data

(defun refresh-data ()
  (alcm::redeploy "ABC" "crm" "crmadmin" "crmadmin" :redeploy t)
  (pg-socket:with-open-connection (c "crm" :user "crmadmin" :password "crmadmin")
    (with-connection (c) 
      (with-transaction ()
        (let* ((org-unit (create-instance 'abc.myorgunit :name "TIP" :boardarea "TIP"))
               (e1
                (let* ((name (create-instance 'abc.myname :first "Woody" :last "Woodpecker"))
                       (address1 (create-instance 'abc.myaddress :street "Goethestraße" :number 8 :city "München" :zip 8000 :kind "home"))
                       (address2 (create-instance 'abc.myaddress :street "Goethestraße" :number 8 :city "München" :zip 8000 :kind "work"))
                       (employee (create-instance 'abc.myemployee :name name :address (list address1 address2) :org org-unit :salary 125000)))
                  employee))
              (e2
                (let* ((name (create-instance 'abc.myname :first "Mickey" :last "Mouse"))
                       (address (create-instance 'abc.myaddress :street "Lessingstraße" :number 7 :city "München" :zip 8000 :kind "home"))
                       (employee (create-instance 'abc.myemployee :name name :address (list address) :org org-unit :salary 42500)))
                  employee)))
          (set-element org-unit 'manager e1)
          (list e1 e2))))))

(defun test ()
  (pg-socket:with-open-connection (c "crm" :user "crmadmin" :password "crmadmin")
    (with-connection (c) 
      (?select '(first middle last city street salary abc.myorgunit.name) :from
               (?inner-join
                (?inner-join
                 (?inner-join 
                  (?inner-join 'abc.myemployee 
                               'abc.myemployee$n1$org 
                               :on (?= 'entity_id 'source_id))
                  'abc.myorgunit
                  :on (?= 'target_id 'abc.myorgunit.entity_id))
                 'abc.myname
                 :on (?= 'abc.myemployee.entity_id 'abc.myname.parent_id))
                'abc.myaddress
                :on (?and (?= 'abc.myemployee.entity_id 'abc.myaddress.parent_id) (?= 'abc.myaddress.kind "home")))
               :where (?and (?= 'boardarea "TIP")
                            (?>= 'salary 100000))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example Query

;; SELECT FROM Employee[org.boardarea = 'TIP' AND salary > '100.000']
;; { name,
;;   address[kind='home'] { street, city},
;;   salary
;; }


(defun eequery (connection string)
  (sql:sql-query connection
                 (dbi::translate-query
                  (dbi::parse-ee-query string))))
   


#+()(?filter (list 'name
               (?filter (list 'street 'city)
                        :from 'address
                        :where (?= 'kind "home"))
               'salary)
         :from 'myemployee
         :where (?and (?= (list 'org 'boardarea) "TIP") (?> 'salary 100000)))

"
SELECT
    ABC.MYNAME.FIRST,
    ABC.MYNAME.MIDDLE,
    ABC.MYNAME.LAST,
    ABC.MYADDRESS.CITY,
    ABC.MYADDRESS.STREET,
    ABC.MYEMPLOYEE.SALARY,
    ABC.MYORGUNIT.NAME
 FROM
               ABC.MYEMPLOYEE
    INNER JOIN ABC.MYEMPLOYEE$N1$ORG    ON (ENTITY_ID = SOURCE_ID)
    INNER JOIN ABC.MYORGUNIT            ON (TARGET_ID = ABC.MYORGUNIT.ENTITY_ID)
    INNER JOIN ABC.MYNAME               ON (ABC.MYEMPLOYEE.ENTITY_ID = ABC.MYNAME.PARENT_ID)
    INNER JOIN ABC.MYADDRESS            ON (ABC.MYEMPLOYEE.ENTITY_ID = ABC.MYADDRESS.PARENT_ID) AND (ABC.MYADDRESS.KIND = 'home')
 WHERE ((ABC.MYORGUNIT.BOARDAREA = 'TIP') AND (SALARY >= 100000))  
"

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
