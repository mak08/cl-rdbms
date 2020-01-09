;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    db interface
;;; Author         Michael Kappert 2011
;;; Last Modified  <michael 2020-01-09 22:18:52>

(in-package :sql)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQL tables and tuples
;;;
;;; Tables arise in many situations:
;;; - represent the content of a DB table
;;; - represent the result of a query
;;; - represent the result of a join
;;; - represent a projecting of a table
;;; -> $tuple types could be constructed from DB table definitions
;;; -> Must be possible to create $tuple types for Joins on-the-fly.
;;; -> Must be possible to extract values given only the field path specified in the join!
;;; -> $tuples are created by the application program(mer) and inserted
;;;    into a $table, DB table (one-by-one or through a $table).
;;;    -> Must be possible for the program(mer) to create a suitable type
;;; -> Need to extract DB field name if $tuple is to be inserted in DB!

;;; For now, we ignore the possibility of defining the same table on different
;;; databases/in different schemas but with different rows.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ToDo:
;;;   In order to implement ?update and ?upsert on tuples, it must be possible to
;;;   obtain information about the primary and unique keys of the table associated
;;;   with the tuple class.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tuples

(defclass tuple () ())
(defmethod print-object ((thing tuple) stream)
  (let* ((tuple-class (class-of thing))
         (slots-and-values
          (loop
             :for slot :in (class-slots tuple-class)
             :for slot-name =  (slot-definition-name slot)
             :unless (eq slot-name 'tabdef)
             :collect (list
                       slot-name
                       (cond
                         ((slot-boundp thing slot-name)
                          (slot-value thing slot-name))
                         (t
                          "DEFAULT"))))))
    (format stream "<~a ~:{~a: ~a~:^, ~}>"
            (class-name (class-of thing))
            slots-and-values)))

(defvar *tuple-slot-name-ht* (make-hash-table :test 'eq))

(defmethod tuple-table ((tuple tuple))
  (class-name (class-of tuple)))

(defmethod tuple-columns ((tuple tuple))
  (let ((tuple-class (class-of tuple)))
    (or (gethash tuple-class *tuple-slot-name-ht*)
        (setf (gethash tuple-class *tuple-slot-name-ht*)
              (loop
                 :for slot :in (class-slots tuple-class)
                 :for slot-name =  (slot-definition-name slot)
                 :unless (eq slot-name 'tabdef)
                 :collect slot-name)))))

;;; We use a special value, +default+, to distunguish between nil and default values,
;;; because we can't pass an unbound variable to sql-serialize

(defvar +default+ (gensym))

(defmethod tuple-values ((tuple tuple))
  (let ((slot-names (tuple-columns tuple)))
    (mapcar (lambda (slot)
              (if (slot-boundp tuple slot)
                  (slot-value tuple slot)
                  +default+))
            slot-names)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DB table tuples
(defclass db-tuple (tuple)
  ;; Use this for tuple types auto-defined from declare-table.
  ;; This class has the convention that the class name must match the
  ;; DB table name it was generated for.
  (
   ;; This slot is actually redefined from scratch in each subclass,
   ;; rather than modifying just the  initform... Leave it here for documentation.
   (tabdef :reader tabdef :initarg :tabdef :initform nil :allocation :class)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transient tuple types 
(defclass transient-tuple (tuple)
  ;; Use this for tuple types auto-defined for the select-list of a query
  ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API functions

(defmethod ?insert ((values tuple)
                    &key
                      (into (tuple-table values))
                      (columns (tuple-columns values)))
  (loop
     :for v :in (tuple-values values)
     :for c :in columns
     :unless (eq v +default+)
     :collect v :into column-values
     :and :collect c :into filled-columns
     :finally
       (sql:sql-exec
        *current-connection*
        (make-sql-insert :table into
                         :columns filled-columns
                         :values column-values))))

;;; ToDo: UPDATE/UPSERT requires a key, but there may
;;; be severel unique keys that could be used. 
(defgeneric ?upsert (tuple &key &allow-other-keys))

(defmethod ?upsert ((tuple db-tuple)
                    &key
                      (into (tuple-table tuple))
                      (columns (tuple-columns tuple)))
  (let* ((tabdef (tabdef tuple))
         (pk-columns (tabdef-pk-columns tabdef)))
    (loop
       :for v :in (tuple-values tuple)
       :for c :in columns
       :unless (or (null v) (eq v +default+))
       :if (tabdef-pk-column-p tabdef c)
       :collect v :into key-values :and :collect c :into key-columns
       :else :collect v :into attr-values :and :collect c :into attr-columns
       :finally (sql:sql-exec
                 *current-connection*
                 (make-sql-upsert :table into
                                  :columns (append key-columns attr-columns)
                                  :values (append key-values attr-values)
                                  :key-columns key-columns
                                  :update (make-sql-update :table into
                                                           :expression (make-sql-assignment :colex attr-columns
                                                                                            :valex attr-values)
                                                           :condition (?and (loop
                                                                           :for c :in key-columns
                                                                           :for v :in key-values
                                                                           :collect (?= c v)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating tuple classes

(defun ensure-tuple-class% (super tabdef name slotspecs)
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
    (when tabdef
      ;;; Create a class slot holding the table definition.
      (push (list :name 'tabdef
                  :initform tabdef
                  :initfunction (lambda () tabdef)
                  :readers (list 'tabdef)
                  :allocation :class)
            slot-specs))
    (let ((tuple-class
           (ensure-class name
                         :direct-superclasses (list super)
                         :direct-slots slot-specs)))
      ;; Class remains EQ under redefinition. Remove from tuple-slot-name hash.
      (remhash tuple-class *tuple-slot-name-ht*)
      tuple-class)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transient tables

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
;;; Fetching results

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

(defmethod sql:fetch ((tuple tuple) (result list) &key (field-mapper #'default-field-mapper))
  (let ((columns (car result))
        (rows (cadr result)))
    (let* ((tuple-class (class-of tuple))
           (package (symbol-package (class-name tuple-class)))
           (num-cols (length columns))
           (num-rows (length rows))
           (slots (mapcar #'slot-definition-name (class-direct-slots tuple-class)))
           (columns
             (loop
                :for k :below num-cols
                :for column =  (nth k columns)
                :collect (let ((colsym (find-symbol (string-upcase column) package)))
                           (or (find (funcall field-mapper colsym) slots)
                               (warn "Tuple ~a has no column ~a, valid columns are ~a. Values will be discarded."
                                     tuple column slots))))))
      (when (> num-rows 1)
        (warn "Result has more than one rows. Excess rows will be discarded."))
      (loop
             :for column :in columns
             :for colnum :below num-cols
             :for value :in (car rows)
             :when column
         :do (setf (slot-value tuple column) value))
      (values tuple
              columns
              num-rows))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 

(defgeneric output-container-for-spec (spec &key mode))

(defmethod output-container-for-spec ((spec list) &key (mode :multi))
  (let ((tuple-class (ensure-tuple-class spec)))
    (ecase mode
      (:multi
       (create-transient-table tuple-class))
      (:single
       (make-instance tuple-class)))))

(defmethod output-container-for-spec ((spec symbol) &key (mode :multi))
  (ecase mode
    (:multi
     (create-transient-table spec))
    (:single
     (make-instance spec))))

(defmethod output-container-for-spec ((spec string) &key (mode :multi))
  (ecase mode
    (:multi
     (create-transient-table spec))
    (:single
     (make-instance (find-symbol (string-upcase spec) *package*)))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper for use-schema

(defmethod ensure-tuple-class ((tabdef tabdef))
  (let ((name (tabdef-name tabdef))
        (schema (tabdef-schema tabdef))
        (slotnames (mapcar #'coldef-name (tabdef-columns tabdef))))
    (ensure-tuple-class% 'db-tuple tabdef (read-from-string (concatenate 'string schema "." name))
                       (mapcar (lambda (s) (list (read-from-string s))) slotnames))))

(defvar *tuple-class-ht* (make-hash-table :test #'equal))

(let ((count 0))

  (defmethod ensure-tuple-class ((colspecs list))
    (or (gethash (cons *package* colspecs) *tuple-class-ht*)
        (setf (gethash (cons *package* colspecs) *tuple-class-ht*)
              (let ((name (format () "ANON-TUPLE-CLASS-~d" (incf count)))
                    (schema (package-name *package*))
                    (slotnames colspecs))
                (ensure-tuple-class% 'transient-tuple nil (read-from-string (concatenate 'string schema "." name))
                                   (mapcar (lambda (s) (list (read-from-string s))) slotnames))))))
  
  (defmethod ensure-tuple-class ((colspec symbol))
    (ensure-tuple-class (list colspec)))
  
)

(defun make-keyword (string)
  (intern (string-upcase string) 'keyword))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
