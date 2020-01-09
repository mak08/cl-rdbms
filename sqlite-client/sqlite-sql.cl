;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   SQLite specific DDL statements 
;;; Author        Michael Kappert 2019
;;; Last Modified <michael 2019-12-24 01:59:05>

(in-package :sqlite-client)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CREATE DATABASE
;;;   SQLite does not have CREATE DATABASE. Just use with-open-connection.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CREATE SCHEMA
;;;
;;; SQLite does not have CREATE SCHEMA, but attached DBs look somewhat like schemas.
;;; Schemas are always create on disk.
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
           (when (eq (schema-create-statement-if-exists statement) :error)
             (error "Schema ~a exists" schema-db)))
          (t
           (sql:sql-exec conn
                         (format nil "INSERT INTO __SCHEMA (NAME) VALUES ('~a')" schema-name))
           (sql:sql-exec conn
                         (with-output-to-string (s)
                           (format s "ATTACH '~a' AS ~a" schema-db schema-name)))))))

(defmethod sql:sql-exec ((conn sqlite-connection) (statement schema-drop-statement))
  (let* ((main-db (database conn))
         (db-name (pathname-name main-db))
         (schema-name (schema-drop-statement-schema statement))
         (schema-db (merge-pathnames 
                     (make-pathname :name (format () "~a.~a" db-name schema-name) :type "sdb")
                     (make-pathname :device (pathname-device main-db)
                                    :directory (pathname-directory main-db))))
         (schema-file (probe-file schema-db)))
    (log2:info "Deleting schema file: ~a" schema-file)
    (case schema-file
      ((nil)
       (error "Schema ~a does not exist" schema-db))
      (otherwise
       (?delete '__SCHEMA :where (?= 'name schema-name))
       (delete-file schema-db)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Loading metadata
;;;
;;; SQLite stores CREATE TABLE statements in a special table SQLITE_MASTER.

(defmethod load-schema ((connection sqlite-connection) (schema string))
  (let ((schema-table
         (cond ((string= schema "")
                'sqlite_master)
               (t
                (make-symbol (format nil "~a.sqlite_master" schema))))))
    (with-connection (connection)
      (let ((result
             (?select 'sql
                      :from schema-table
                      :where (?and (?= 'type "table")
                                   (?not (?= 'name "__schema")))
                      :into '("sql"))))
        (make-schema :name schema
                     :tables (loop
                                :for row :across (tuples result)
                                :collect (parse-table-definition (sql row))))))))

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

(defmethod serialize-for-connection ((connection t) (thing foreign-key) stream)
  (when (foreign-key-referenced-table-schema thing)
    (log2:warning "Ignoring schema ~a in foreign key reference" (foreign-key-referenced-table-schema thing)))
  (format stream "CONSTRAINT ~a FOREIGN KEY (~{~a~^, ~}) REFERENCES ~:[~;~:*~a.~]~a(~{~a~^, ~}) ON DELETE ~a ON UPDATE ~a DEFERRABLE INITIALLY DEFERRED"
          (foreign-key-name thing)
          (foreign-key-columns thing)
          ;; SQLite does not allow cross-schema references
          nil
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
