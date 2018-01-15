;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author         Michael Kappert
;;; Copyright      (c) Michael Kappert 2011
;;; Created        2011-10-20 22:15:22 22:15:22
;;; Last Modified  <michael 2018-01-15 20:51:31>
;;; Description    db interface

(in-package :sql)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use Case
;;    - create for on UI, enter values, create table for form, persist entered values

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - Architecture:
;;   - who keeps db connection(s)?
;;     -> client+db, client keep connections
;;     -> client+server+db, client or server keeps connections
;;
;; - Think about setup procedure:
;;   0- where do definitions come from?
;;      when are they changed?
;;      - predefined/user defined/patch&upgrade
;;      who owns them?
;;      - software author/ software user
;;      on which level are they made?
;;      -  cdoc > ? > table
;;         * cdoc is the user interface level
;;         * table is the database level
;;         - is there anything in between?
;;   1- when to define table struct?
;;   2- when to create table?
;;   3- other actions, eg store definition in repository, log etc?
;;   4- when to populate table?
;;
;;   a. Execute definitions: Do 1+2 together, populate later
;;   b. Partially execute: create struct, defer table creation?
;;   b. Write definitions to repository: Decide later what to do when


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; DB Schema
;;
;; A schema is a collection of db table definitions, not a namespace.
;; The schema is used in two situations:
;; - When building the application, the types derived from the schema must
;;   be created/imported.
;; - When deploying the application, the corresponding DB tables must be
;;   created

(defvar *schema-lib* (make-hash-table :test #'equalp))

(defvar *schema* nil)

(defmacro defschema (name &rest definitions)
  (let ((owner)
        (tabdefs))
    (loop :for definition :in definitions
       :do (ecase (car definition)
             (:owner
               (setf owner (cadr definition)))
             (:table
               (push `(deftable ,@(cdr definition) :schema ,name)
                     tabdefs))))
    `(setf (gethash ,name *schema-lib*)
                    (make-schema :name ,name
                                 :owner ,owner
                                 :tables (reverse (list ,@tabdefs))))))

(defun get-schema-by-name (name)
  (gethash name *schema-lib*))

(defmethod use-schema ((name string))
  (use-schema (get-schema-by-name name)))
(defmethod use-schema ((schema schema))
  (loop
     :for tabdef :in (schema-tables schema)
     :do (ensure-tuple-class tabdef))
  (values schema))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper for defschema

(defmacro deftable (name &key schema columns constraints)
  (let ((coldefs
          (loop :for coldef :in columns
             :collect `(make-coldef :name ,@coldef)))
        (constraints
          (loop :for constraint :in constraints :collect
             (destructuring-bind (&key primary-key foreign-key unique-key
                                       columns referenced-table referenced-columns)
                 constraint
               (cond (primary-key
                      `(make-primary-key :name ,primary-key
                                         :columns ',columns))
                     (foreign-key
                      `(make-foreign-key :name ,foreign-key
                                         :schema ,schema
                                         :columns ',columns
                                         :referenced-table-schema ,schema
                                         :referenced-table ',referenced-table
                                         :referenced-columns ',referenced-columns))
                     (unique-key
                      `(make-unique-key :name ,unique-key
                                        :columns ',columns)))))))
    `(create-tabdef :name ,name
                    :schema ,schema
                    :columns (list ,@coldefs)
                    :constraints (list ,@constraints))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SQL tables and tuples

;; Tables arise in many situations:
;; - represent the content of a DB table
;; - represent the result of a query
;; - represent the result of a join
;; - represent a projecting of a table
;; -> $tuple types could be constructed from DB table definitions
;; -> Must be possible to create $tuple types for Joins on-the-fly.
;; -> Must be possible to extract values given only the field path specified in the join!
;; -> $tuples are created by the application program(mer) and inserted
;;    into a $table, DB table (one-by-one or through a $table).
;;    -> Must be possible for the program(mer) to create a suitable type
;; -> Need to extract DB field name if $tuple is to be inserted in DB!

;; For now, we ignore the possibility of defining the same table on different
;; databases/in different schemas but with different rows.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The $tuple root class
(defclass tuple () ())
(defmethod print-object ((thing tuple) stream)
  (let* ((tuple-class (class-of thing))
         (slots-and-values
           (mapcar (lambda (slot)
                     (let ((slot-name (slot-definition-name slot)))
                       (list
                        slot-name
                        (cond
                          ((slot-boundp thing slot-name)
                            (slot-value thing slot-name))
                          (t
                            "DEFAULT")))))
                   (class-slots tuple-class))))
    (format stream "<~a ~:{~a: ~a~:^, ~}>"
            (class-name (class-of thing))
            slots-and-values)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DB table tuples
(defclass db-tuple (tuple)
  ;; Use this for tuple types auto-defined from declare-table.
  ;; This class has the convention that the class name must match the
  ;; DB table name it was generated for.
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transient tuple types 
(defclass transient-tuple (tuple)
  ;; Use this for tuple types auto-defined for the select-list of a query
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ensure-tuple-class% (super name slotspecs)
  (let ((slot-specs
          (loop
             :for slotspec :in slotspecs
             :collect (destructuring-bind (slotname
                                           &key
                                           (initform nil initform-supplied-p)
                                           (reader slotname)
                                           (writer (list 'setf slotname)))
                          slotspec
                        (if initform-supplied-p
                            (list :name slotname
                                  :initform initform
                                  :initfunction (lambda () initform)
                                  :readers (list reader)
                                  :writers (list writer)
                                  :initargs (list (make-keyword (format () "~a" slotname))))
                            (list :name slotname
                                  :readers (list reader)
                                  :writers (list writer)
                                  :initargs (list (make-keyword (format () "~a" slotname)))))))))
    (ensure-class name
                  :direct-superclasses (list super)
                  :direct-slots slot-specs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table

(defclass transient-table ()
  ((tuple-class :accessor tuple-class :initarg :tuple-class)
   (tuples :accessor tuples :initarg :tuples
           :initform (make-array 10 :adjustable t :fill-pointer 0))))

(defmethod print-object ((table transient-table) stream)
  (format stream "<TABLE OF ~a~%~a>~%"
          (class-name (tuple-class table))
          (tuples table)))

(defmethod create-transient-table ((class string))
  (make-instance 'transient-table :tuple-class (find-class (read-from-string class))))

(defmethod create-transient-table ((class symbol))
  (make-instance 'transient-table :tuple-class (find-class class)))

(defmethod create-transient-table ((class class))
  (make-instance 'transient-table :tuple-class class))

(defmethod append-tuple ((table transient-table) (tuple null))
  (let ((tuple (make-instance (tuple-class table))))
    (vector-push-extend tuple (tuples table))
    tuple))

(defmethod append-tuple ((table transient-table) (tuple list))
  (let ((tuple (apply #'make-instance (tuple-class table) tuple)))
    (vector-push-extend tuple (tuples table))
    tuple))

(defmethod append-tuple ((table transient-table) (tuple tuple))
  (unless (eq (tuple-class table) (class-of tuple))
    (error "Cannot insert tuple ~a of type ~a into ~a"
           tuple
           (class-name (class-of tuple))
           table))
  (vector-push-extend tuple (tuples table))
  tuple)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fetch results

(defun default-field-mapper (field)
  field)

(defmethod sql:fetch ((transient-table transient-table) (result list) &key (field-mapper #'default-field-mapper))
  (let ((columns (car result))
        (rows (cadr result)))
    (let* ((package (symbol-package (class-name (tuple-class transient-table))))
           (num-cols (length columns))
           (num-rows (length rows))
           (slots (mapcar #'slot-definition-name (class-direct-slots (tuple-class transient-table))))
           (columns
             (loop
                :for k :below num-cols
                :for column =  (nth k columns)
                :collect (let ((colsym (find-symbol (string-upcase column) package)))
                           (or (find (funcall field-mapper colsym) slots)
                               (warn "Table ~a has no column ~a, valid columns are ~a. Values will be discarded."
                                     transient-table column slots))))))
      (dolist (row rows)
        (let ((tuple (append-tuple transient-table nil)))
          (loop
             :for column :in columns
             :for colnum :below num-cols
             :for value :in row
             :when column
             :do (setf (slot-value tuple column) value))))
      (values transient-table
              columns
              num-rows))))

(defmethod sql:fetch ((tabspec list) result &key (field-mapper (lambda (field) field)))
  (let* ((tuple-class (ensure-tuple-class tabspec))
         (transient-table (create-transient-table tuple-class)))
    (fetch transient-table result :field-mapper field-mapper))) 

(defmethod sql:fetch ((tabname string) result &key (field-mapper (lambda (field) field)))
  (let ((transient-table (create-transient-table tabname)))
    (fetch transient-table result :field-mapper field-mapper)))

(defmethod sql:fetch ((tabname symbol) result &key (field-mapper (lambda (field) field)))
  (let ((transient-table (create-transient-table tabname)))
    (fetch transient-table result :field-mapper field-mapper)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper for use-schema

(defmethod ensure-tuple-class ((tabdef tabdef))
  (let ((name (tabdef-name tabdef))
        (schema (tabdef-schema tabdef))
        (slotnames (mapcar #'coldef-name (tabdef-columns tabdef))))
    (ensure-tuple-class% 'db-tuple (read-from-string (concatenate 'string schema "." name))
                       (mapcar (lambda (s) (list (read-from-string s))) slotnames))))

(defvar *tuple-class-ht* (make-hash-table :test #'equal))

(let ((count 0))

  (defmethod ensure-tuple-class ((colspecs list))
    (or (gethash (cons *package* colspecs) *tuple-class-ht*)
        (setf (gethash (cons *package* colspecs) *tuple-class-ht*)
              (let ((name (format () "ANON-TUPLE-CLASS-~d" (incf count)))
                    (schema (package-name *package*))
                    (slotnames colspecs))
                (ensure-tuple-class% 'transient-tuple (read-from-string (concatenate 'string schema "." name))
                                   (mapcar (lambda (s) (list (read-from-string s))) slotnames))))))
  
  (defmethod ensure-tuple-class ((colspec symbol))
    (ensure-tuple-class (list colspec)))
  
)

(defun make-keyword (string)
  (intern (string-upcase string) 'keyword))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
