;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Copyright      (c)  2015
;;; Last Modified  <michael 2018-01-12 00:25:47>

(in-package :sql)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defmethod sql:serialize-for-connection ((connection hdb-odbc::odbc-connection) (thing sql-insert) stream)
  (format stream "INSERT INTO ~a " (sql-insert-table thing))
  (@[] connection (sql-insert-columns thing) stream)
  (format stream " VALUES ")
  (@[] connection (sql-insert-values thing) stream))

(defmethod serialize-for-connection ((connection t) (thing sql-update) stream)
  (format stream "UPDATE ~a SET " (sql-update-table thing))
  (serialize-for-connection connection (sql-update-expression thing) stream)
  (format stream " WHERE ")
  (serialize-for-connection connection (sql-update-condition thing) stream))

(defmethod serialize-for-connection ((connection t) (thing sql-delete) stream)
  (format stream "DELETE FROM ~a ~@[WHERE~] " (sql-delete-table thing) (sql-delete-condition thing))
  (serialize-for-connection connection (sql-delete-condition thing) stream))

(defmethod serialize-for-connection ((conn t) (stmt sql-truncate) stream)
  (when (sql-truncate-cascade stmt)
    (warn "TRUNCATE ... CASCADE is not supported by HANA"))
  (format stream "TRUNCATE TABLE ~a.~a"
          (sql-truncate-schema stmt)
          (sql-truncate-name stmt)))

(defmethod sql:serialize-for-connection ((connection hdb-odbc::odbc-connection) (thing tabdef) stream)
  (when (tabdef-schema thing)
    (format stream "\"~a\"." (tabdef-schema thing)))
  (format stream "~a (" (tabdef-name thing))
  (!{} connection (tabdef-columns thing) stream)
  (when (tabdef-constraints thing)
    (format stream ", "))
  (!{} connection (tabdef-constraints thing) stream)
  (format stream ")"))

(defmethod sql:serialize-for-connection ((connection hdb-odbc::odbc-connection) (thing primary-key) stream)
  (format stream "CONSTRAINT ~a PRIMARY KEY (~{~a~^, ~})"
          (primary-key-name thing)
          (primary-key-columns thing)))

(defmethod sql:serialize-for-connection ((connection hdb-odbc::odbc-connection) (thing unique-key) stream)
  (format stream "CONSTRAINT ~a UNIQUE (~{~a~^, ~})"
          (unique-key-name thing)
          (unique-key-columns thing)))

(defmethod sql:serialize-for-connection ((connection t) (thing foreign-key) stream)
  (format stream "CONSTRAINT ~a FOREIGN KEY (~{~a~^, ~}) REFERENCES ~a(~{~a~^, ~}) ON DELETE ~a ON UPDATE ~a"
          (foreign-key-name thing)
          (foreign-key-columns thing)
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

(defmethod serialize-for-connection ((connection t) (thing sql-junction) stream)
  (cond ((null (sql-junction-args thing))
         (cond ((string= (sql-junction-op thing) "and")
                (format stream "1 = 1"))
               ((string= (sql-junction-op thing) "or")
                (format stream "1 = 0"))
               (t
                (error "Unhandled expression ~a" thing))))
        (t
         (!{} connection (sql-junction-args thing) stream :separator (sql-junction-op thing)))))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
