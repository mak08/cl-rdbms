;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   SQLite specific DDL statements 
;;; Author        Michael Kappert 2019
;;; Last Modified <michael 2019-12-16 20:31:35>

(in-package :sqlite-client)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CREATE DATABASE
;;;   SQLite does not have CREATE DATABASE. Just use with-open-connection.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CREATE SCHEMA
;;;
;;; SQLite does not have CREATE SCHEMA, but attached DBs look somewhat like schemas.
;;; Schemas are always create on disk and not work in-memory.
;;; We keep track of attached schemas in a special table __schema.
;;; ToDo: The process is not recursive. Distinguish between 'main db files' and 'schema db files'.
;;; For example, opening a schema db file directly would create a __schema table in it (see CONNECT).

(defmethod sql:sql-exec ((conn sqlite-connection) (statement schema-create-statement))
  (let* ((main-db (database conn))
         (db-name (pathname-name main-db))
         (schema-name (schema-create-statement-schema statement))
         (schema-db (merge-pathnames 
                     (make-pathname :name (format () "~a.~a" db-name schema-name) :type "sdb")
                     (make-pathname :device (pathname-device main-db)
                                    :directory (pathname-directory main-db)))))
    (cond ((probe-file schema-db)
           (error "Schema ~a exists" schema-db))
          (t
           (sql:sql-exec conn
                         (format nil "INSERT INTO __SCHEMA (NAME) VALUES ('~a')" schema-name))
           (sql:sql-exec conn
                         (with-output-to-string (s)
                           (format s "ATTACH '~a' AS ~a" schema-db schema-name)
                           (!{} conn (schema-create-statement-tables statement) s)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DROP TABLE

(defmethod sql:sql-exec ((conn sqlite-connection) (statement table-drop-statement))
  (ecase (table-drop-statement-if-not-empty statement)
    (:force
     (error "DROP TABLE ... CASCADE is not supported"))
    (:error
     ;; SQLite default behavour
     ))
  (sql:sql-exec conn
                (format nil "DROP TABLE ~:[IF EXISTS~;~] ~:[~;~:*~a.~]~a"
                        (ecase (table-drop-statement-if-does-not-exist statement)
                          (:ignore nil)
                          (:error t))
                        (tabdef-schema (table-drop-statement-tabdef statement))
                        (tabdef-name (table-drop-statement-tabdef statement)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transaction isolation level
(defmethod sql:sql-exec ((conn sqlite-connection) (statement transaction-command))
  ;; do nothing
  (unless (null (transaction-command-isolation-level statement))
    (log2:warning "Ignoring ISOLATION LEVEL ~a"
                  (transaction-command-isolation-level statement))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQL statement syntax

(defmethod sql:serialize-for-connection ((connection sqlite-connection) (thing primary-key) stream)
  (format stream "CONSTRAINT ~a PRIMARY KEY (~{~a~^, ~})"
          (primary-key-name thing)
          (primary-key-columns thing)))

(defmethod sql:serialize-for-connection ((connection sqlite-connection) (thing unique-key) stream)
  (format stream "CONSTRAINT ~a UNIQUE (~{~a~^, ~})"
          (unique-key-name thing)
          (unique-key-columns thing)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Type mapping for SQLite
;;; - probably unnecessary?

(defmethod sql:map-db-type ((connection sqlite-connection) type)
  (let  ((type (if (listp type) (car type) type))
         (length (when (listp type) (cadr type))))
    (when (and length
               (not (member type '(id string))))
      (error "Type ~a does not have a length parameter" type))
    (cond
      ((stringp type)
       (log2:debug "MAP-DB-TYPE: Not mapping type string ~a" type)
       type)
      (t
       (ecase type
         ;; character
         (id          (format () "char(~a)" (or length 8)))
         (uuid        "text")
         (identifier  "text")
         (string      (format () "varchar(~a)" (or length 1024)))
         (text        "text")
         ;; number
         (boolean     "boolean")
         (integer     "integer")
         (float       "float")
         (decimal     "float")
         (money       "float")
         (percent     "float")
         ;; byte strings
         (raw         "blob")
         ;; date & time
         (timestamp   "text")
         (duration    "text")
         (date        "text")
         ((nil)   "text"))))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
