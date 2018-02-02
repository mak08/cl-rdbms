;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael 2014
;;; Last Modified <michael 2018-01-27 20:54:52>

(in-package :sql)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; t, number, string, list

(defmethod serialize-for-connection ((connection t) (thing t) stream)
  (format stream "~a" thing))

(defmethod serialize-for-connection ((connection t) (thing number) stream)
  (format stream "'~a'" thing))

(defmethod serialize-for-connection ((connection t) (thing string) stream)
  (format stream "'~a'" thing))

(defmethod serialize-for-connection ((connection t) (thing list) stream)
  (!{} connection thing stream))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DDL statements

(defmethod serialize-for-connection ((conn t) (stmt sql-truncate) stream)
  (format stream "TRUNCATE TABLE ~a.~a ~:[RESTRICT~;CASCADE~];"
          (sql-truncate-schema stmt)
          (sql-truncate-name stmt)
          (sql-truncate-cascade stmt)))

(defmethod serialize-for-connection ((connection t) (thing tabdef) stream)
  (when (tabdef-schema thing)
    (format stream "~a." (tabdef-schema thing)))
  (format stream "~a (" (tabdef-name thing))
  (!{} connection (tabdef-columns thing) stream)
  (when (tabdef-constraints thing)
    (format stream ", "))
  (!{} connection (tabdef-constraints thing) stream)
  (format stream ")"))

(defmethod serialize-for-connection ((connection t) (thing coldef) stream)
  (format stream "~a" (coldef-name thing))
  (format stream " ~a"
          (map-db-type connection (coldef-datatype thing)))
  (@[] connection (coldef-collation thing) stream)
  (@[] connection (coldef-default-value thing) stream :prefix " DEFAULT ")
  (@[] connection (coldef-constraint thing) stream :prefix " "))

(defmethod serialize-for-connection ((connection t) (thing colcon) stream)
  (@[] connection (colcon-label thing) stream :prefix "CONSTRAINT ")
  (if (colcon-notnull thing)
      (format stream " NOT NULL")
      (format stream " NULL"))
  (if (colcon-references thing)
      (format stream " REFERENCES ")
      (serialize-for-connection connection (colcon-references thing) stream)))

(defmethod serialize-for-connection ((connection t) (thing colref) stream)
  (format stream "~a ~@[(~a)~]"
          (colref-table thing)
          (colref-column thing)))

(defmethod serialize-for-connection ((connection t) (thing primary-key) stream)
  (format stream "CONSTRAINT ~a PRIMARY KEY (~{~a~^, ~}) INITIALLY IMMEDIATE"
          (primary-key-name thing)
          (primary-key-columns thing)))

(defmethod serialize-for-connection ((connection t) (thing unique-key) stream)
  (format stream "CONSTRAINT ~a UNIQUE (~{~a~^, ~}) INITIALLY IMMEDIATE"
          (unique-key-name thing)
          (unique-key-columns thing)))

(defmethod serialize-for-connection ((connection t) (thing foreign-key) stream)
  (format stream "CONSTRAINT ~a FOREIGN KEY (~{~a~^, ~}) REFERENCES ~a.~a(~{~a~^, ~}) ON DELETE ~a ON UPDATE ~a INITIALLY DEFERRED"
          (foreign-key-name thing)
          (foreign-key-columns thing)
          (foreign-key-referenced-table-schema thing)
          (foreign-key-referenced-table thing)
          (foreign-key-referenced-columns thing)
          (ecase (foreign-key-on-delete thing)
            ((nil) "NO ACTION")
            (:cascade "CASCADE")
            (:restrict "RESTRICT"))
          (ecase (foreign-key-on-update thing)
            ((nil) "NO ACTION")
            (:cascade "CASCADE")
            (:restrict "RESTRICT"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DML

(defmethod serialize-for-connection ((connection t) (thing sql-query) stream)
  (format stream "SELECT ")
  (serialize-for-connection connection (sql-query-sellist thing) stream)
  (format stream " ")
  (serialize-for-connection connection (sql-query-table-expression thing) stream))

(defmethod serialize-for-connection ((connection t) (thing sql-insert) stream)
  (format stream "INSERT INTO ~a " (sql-insert-table thing))
  (@[] connection (sql-insert-columns thing) stream)
  ;; Printing "VALUES" here also doesn't work. S-F-C needs a context parameter.
  (serialize-for-connection connection (sql-insert-values thing) stream))


(defmethod serialize-for-connection ((connection t) (thing sql-update) stream)
  (format stream "UPDATE ~a SET " (sql-update-table thing))
  (serialize-for-connection connection (sql-update-expression thing) stream)
  (format stream " WHERE ")
  (serialize-for-connection connection (sql-update-condition thing) stream)
  (format stream ";"))

(defmethod serialize-for-connection ((connection t) (thing sql-delete) stream)
  (format stream "DELETE FROM ~a" (sql-delete-table thing))
  (@[] connection (sql-delete-condition thing) stream :prefix " WHERE ")
  (format stream ";"))

(defmethod serialize-for-connection ((connection t) (thing subquery) stream)
  (format stream "(SELECT ")
  (serialize-for-connection connection (subquery-sellist thing) stream)
  (serialize-for-connection connection (subquery-table-expression thing) stream)
  (format stream ")")
  (format stream "~@[ AS ~a~]" (subquery-alias thing)))

(defmethod serialize-for-connection ((connection t) (thing join) stream)
  (format stream "(")
  (serialize-for-connection connection (join-left thing) stream)
  (format stream " ~a JOIN "  (join-type thing))
  (serialize-for-connection connection (join-right thing) stream)
  (@[] connection (join-on thing) stream :prefix " ON ")
  (@[] connection (join-using thing) stream :prefix " USING ")
  (format stream ")"))


(defmethod serialize-for-connection ((connection t) (thing select-list) stream)
  (when (select-list-distinct-p thing)
    (format stream "DISTINCT "))
  (!{} connection (select-list-items thing) stream))

(defmethod serialize-for-connection ((connection t) (thing select-item) stream)
  (@[] connection (select-item-table thing) stream :suffix ".")
  (@[] connection (select-item-colspec thing) stream)
  (@[] connection (select-item-alias thing) stream :prefix " AS "))

(defmethod serialize-for-connection ((connection t) (thing sql-table-expression) stream)
  (@[] connection (sql-table-expression-from thing) stream :prefix "FROM ")
  (@[] connection (sql-table-expression-where thing) stream :prefix " WHERE ")
  (@[] connection (sql-table-expression-groupby thing) stream :prefix " GROUP BY ")
  (@[] connection (sql-table-expression-having thing) stream :prefix " HAVING ")
  (format stream " ~a "
          (ecase (sql-table-expression-lock-mode thing)
            (:none "")
            (:share "FOR SHARE")
            (:update "FOR UPDATE")))
  (format stream "~@[NOWAIT~]" (sql-table-expression-nowait thing)))

(defmethod serialize-for-connection ((connection t) (thing sql-table-reference) stream)
  (format stream "~a~@[ AS ~a~]~@[~a~]"
          (sql-table-reference-table-name thing)
          (sql-table-reference-alias thing)
          (sql-table-reference-columns thing)))

(defmethod serialize-for-connection ((connection t) (thing sql-tuple) stream)
  ;; This is appropriate for INSERT but not for other references of TUPLE
  (format stream " VALUES (")
  (!{} connection (sql-tuple-elements thing) stream)
  (format stream ")"))

(defmethod serialize-for-connection ((connection t) (thing sql-junction) stream)
  (cond ((null (sql-junction-args thing))
         (cond ((string= (sql-junction-op thing) "and")
                (format stream "true"))
               ((string= (sql-junction-op thing) "or")
                (format stream "false"))
               (t
                (error "Unhandled expression ~a" thing))))
        (t
         (!{} connection (sql-junction-args thing) stream :separator (sql-junction-op thing)))))

(defmethod serialize-for-connection ((connection t) (thing sql-negation) stream)
  (format stream "(NOT ")
  (serialize-for-connection connection (sql-negation-argument thing) stream)
  (format stream ")"))

(defmethod serialize-for-connection ((connection t) (thing sql-function) stream)
  (format stream "~a(" (sql-function-name thing))
  (!{} connection (sql-function-arguments thing) stream)
  (format stream ")"))

(defmethod serialize-for-connection ((connection t) (thing sql-comparison) stream)
  (format stream "(")
  (serialize-for-connection connection (sql-comparison-column thing) stream)
  (format stream " ~a " (sql-comparison-op thing))
  (serialize-for-connection connection (sql-comparison-value thing) stream)
  (format stream ")"))

(defmethod serialize-for-connection ((connection t) (thing sql-range-predicate) stream)
  (format stream "(")
  (serialize-for-connection connection (sql-range-predicate-column thing) stream)
  (format stream " BETWEEN ")
  (serialize-for-connection connection (sql-range-predicate-lower thing) stream)
  (format stream " AND ")
  (serialize-for-connection connection (sql-range-predicate-upper thing) stream))

(defmethod serialize-for-connection ((connection t) (thing sql-alnum-expression) stream)
  (format stream "(")
  (serialize-for-connection connection (sql-alnum-expression-left thing) stream)
  (format stream "~a" (sql-alnum-expression-op thing))
  (serialize-for-connection connection (sql-alnum-expression-right thing) stream)
  (format stream ")"))

(defmethod serialize-for-connection ((connection t) (thing sql-range-enum) stream)
  (format stream "(")
  (!{} connection (sql-range-enum-values thing) stream)
  (format stream ")"))

;; Assignment (in UPDATE)

(defmethod serialize-for-connection ((connection t) (thing sql-assignment) stream)
  (serialize-for-connection connection (sql-assignment-colex thing) stream)
  (format stream " = ")
  (serialize-for-connection connection (sql-assignment-valex thing) stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tuples for Insert

(defmethod serialize-for-connection (connection (thing tuple) stream)
  (let* ((tuple-class (class-of thing))
         (slot-names (mapcar #'slot-definition-name
                             (class-slots tuple-class))))
    (format stream "(~{~a~^, ~})" slot-names)
    (format stream " VALUES (")
    (!{} connection
         (loop :for slot-name :in slot-names
            :collect (cond
                       ((slot-boundp thing slot-name)
                        (slot-value thing slot-name))
                       (t
                        'DEFAULT)))
         stream)
    (format stream ")")))

(defmethod serialize-for-connection (connection (thing transient-table) stream)
  (declare (ignorable colon-p atsign-p))
  (let* ((slot-names (mapcar #'slot-definition-name
                             (class-slots (tuple-class thing)))))
    (format stream "(~{~a~^, ~})" slot-names)
    (format stream " VALUES (")
    (!{} connection
         (loop :for slot-name :in slot-names
            :collect (cond
                       ((slot-boundp thing slot-name)
                        (slot-value thing slot-name))
                       (t
                        'DEFAULT)))
         stream)
    (format stream ")")))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
