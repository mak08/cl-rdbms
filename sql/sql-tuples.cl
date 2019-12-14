;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    db interface
;;; Author         Michael Kappert 2011
;;; Last Modified  <michael 2019-12-14 10:59:53>

(in-package :sql)

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

(defmethod tuple-table ((tuple tuple))
  (class-name (class-of tuple)))

(defmethod tuple-columns ((tuple tuple))
  (mapcar #'slot-definition-name (class-slots (class-of tuple))))

(defmethod tuple-values ((tuple tuple))
  (let ((slot-names (mapcar #'slot-definition-name (class-slots (class-of tuple)))))
    (mapcar (lambda (slot)
              (if (slot-boundp tuple slot)
                  (slot-value tuple slot)
                  nil))
            slot-names)))

(defmethod ?insert ((values tuple)
                    &key
                      (into (tuple-table values))
                      (columns (tuple-columns values)))
  (?insert (tuple-values values) :into into :columns columns))

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
