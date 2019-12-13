;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael 2013
;;; Last Modified <michael 2019-12-12 22:20:31>

(in-package :sql)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQL Commands
;;;    http://www.postgresql.org/docs/current/static/sql-commands.html

(defstruct schema name owner roles tables)

(defstruct role name
           (superuser nil)
           (createdb nil)

           (createrole nil)
           (createuser nil)
           (inherit t)
           (login nil)
           (replication nil)
           (connection-limit 5)
           (encrypted-password "change-me")
           (valid-until "9999-12-31")
           (member-of nil)
           (members nil)
           (admin nil))

(defstruct (ddl-table-cmd (:include sql-statement)) schema name)

(defstruct (alter-table-cmd (:include ddl-table-cmd))
  ;; cf. tabmod
  )

(defstruct (sql-truncate (:include ddl-table-cmd))
  cascade)

(defstruct (sql-drop (:include ddl-table-cmd))
  cascade)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Table definition

(defstruct tabdef schema name columns constraints)

(defun create-tabdef (&key schema name columns constraints)
  (when (symbolp name)
    (setf name (symbol-name name)))
  (unless (< (length schema) *table_name_length*)
    (error "Schema name too long"))
  (unless (< (length name) *table_name_length*)
    (error "Table name too long"))
  (make-tabdef :schema schema
               :name name
               :columns columns
               :constraints constraints))

;;; Column definitions
(defstruct coldef name datatype default-value collation constraint)
(defstruct colcon label notnull check default unique references)
(defstruct colref table column matchtype on-delete on-update)

;;; Table Constraints
(defstruct tabcon
  schema
  name
  columns)
(defstruct (primary-key (:include tabcon)))
(defstruct (unique-key (:include tabcon)))
(defstruct (foreign-key (:include tabcon))
  referenced-table-schema
  referenced-table
  referenced-columns
  (on-delete :restrict)
  (on-update :restrict))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Table Modifications
;;; The default action is 'ADD'.
;;; Note that the ADD COLUMNS and ADD CONSTRAINT modification use the same
;;; syntax as in CREATE TABLE - you can pass a coldef or tabcon, respectively.

(defstruct tabmod
  schema
  table
  (action "ADD")
  modification)


(defmethod sql:sql-exec ((conn t) (tabmod tabmod))
  (sql-exec conn
            (let ((*print-circle* nil))
              (with-output-to-string (s)
                (format s "ALTER TABLE ~a.~a ~a "
                        (tabmod-schema tabmod)
                        (tabmod-table tabmod)
                        (tabmod-action tabmod))
                (serialize-for-connection conn (tabmod-modification tabmod) s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schemas

(defstruct schema-create-statement schema authorization tables)

(defun %create-schema (owner &key (name owner) (tabdefs)) 
  (sql:sql-exec
   *current-connection*
   (make-schema-create-statement :schema name :authorization owner :tables tabdefs)))

(defmethod sql:sql-exec ((conn t) (statement schema-create-statement))
  (sql:sql-exec conn
                (with-output-to-string (s)
                  (format s "CREATE SCHEMA ~a AUTHORIZATION ~a"
                          (schema-create-statement-schema statement)
                          (schema-create-statement-authorization statement))
                  (!{} conn (schema-create-statement-tables statement) s))))

(defstruct schema-drop-statement schema if-does-not-exist if-not-empty)

(defun %drop-schema (name &key (if-does-not-exist :error) (if-not-empty :error)) 
  (sql:sql-exec
   *current-connection*
   (make-schema-drop-statement :schema name
                               :if-does-not-exist if-does-not-exist
                               :if-not-empty if-not-empty)))

(defmethod sql:sql-exec ((conn t) (statement schema-drop-statement))
  (sql:sql-exec conn
                (format nil "DROP SCHEMA ~:[IF EXISTS~;~] ~a ~:[CASCADE~;RESTRICT~] ;"
                        (ecase (schema-drop-statement-if-does-not-exist statement)
                          (:ignore nil)
                          (:error t))
                        (schema-drop-statement-schema statement)
                        (ecase (schema-drop-statement-if-not-empty statement)
                          (:force nil)
                          (:error t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Databases

(defstruct database-create-statement name owner)

(defun %create-database (name owner)
  (sql:sql-exec *current-connection*
                (make-database-create-statement :name name :owner owner)))

(defmethod sql:sql-exec ((conn t) (statement database-create-statement))
  (sql:sql-exec conn
                (format nil "CREATE DATABASE ~a WITH OWNER = ~a;"
                        (database-create-statement-name statement)
                        (database-create-statement-owner statement))))

(defstruct database-drop-statement name)

(defun %drop-database (name)
  (sql:sql-exec *current-connection*
                (make-database-drop-statement :name name)))

(defmethod sql:sql-exec ((conn t) (statement database-drop-statement))
  (sql:sql-exec conn
                (format nil "DROP DATABASE ~a;"
                        (database-drop-statement-name statement))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Users & Roles


(defun enable-user (name)
  (%change-role name :login t))

(defun disable-user (name)
  (%change-role name :nologin t))

(defun %create-role (name &key (createdb nil)
                          (createrole nil)
                          (login t)
                          (connlimit (if createdb -1 5))
                          (password "")
                          (valid-until "9999-12-31")
                          in-role role admin)
  ;; SUPERUSER, CREATEUSER - not supported
  ;; INHERIT - not supported  (always inherit)
  ;; ENCRYPTED - not supported (always encrypt passwords) 
  ;; IN GROUPS - not supported (obsolete)
  ;; USER role_name - not supported (obsolete)
  ;; SYSID - not supported (obsolete)
  (sql:sql-exec
   *current-connection*
   (format nil "CREATE ROLE ~a ~:[NO~;~]CREATEDB ~:[NO~;~]CREATEROLE INHERIT ~:[NO~;~]LOGIN CONNECTION LIMIT ~a ENCRYPTED PASSWORD '~a' VALID UNTIL '~a'~:[~; IN ROLE ~]~:*~{~a~^, ~}~:[~; ROLE ~]~:*~{~a~^, ~}~:[~; ADMIN ~]~:*~{~a~^, ~};"
           name createdb createrole login connlimit password valid-until in-role role admin)))



(defun %alter-role (user &key (password nil) (login nil) (nologin nil) (valid-until nil))
  (sql:sql-exec *current-connection*
                (format nil "ALTER ROLE ~a
~:[~; WITH PASSWORD ~:*'~a'~]
~:[~; VALID UNTIL ~:*'~a'~]
~:[~; LOGIN~]~:[~; NOLOGIN~];"
                        user password valid-until login nologin)))

(defun %drop-role (name &key (force nil) (cascade nil))
  (when force
    (%drop-owned name :cascade cascade))
  (sql:sql-exec *current-connection* (format nil "DROP ROLE ~a;" name)))

(defun %drop-owned (name &key (cascade nil))
  (sql:sql-exec *current-connection* (format nil "DROP OWNED BY ~a ~:[RESTRICT~;CASCADE~];" name cascade)))

(defun %grant (privilege &key database schema table columns tablespace role)
  (sql:sql-exec *current-connection*
                (format nil "GRANT ~a ~:[~;~:*(~{~a~^, ~})~] ON ~:[~;DATABASE ~:*~a~]~:[~;SCHEMA ~:*~a~]~:[~;TABLE ~:*~a~]~:[~;TABLESPACE ~:*~a~] TO ~a;"
                        privilege
                        columns
                        database
                        schema
                        table
                        tablespace
                        role)))

(defun %revoke (privilege &key database schema table columns tablespace role)
  (sql:sql-exec *current-connection*
                (format nil "REVOKE ~a ~:[~;~:*(~{~a~^, ~})~] ON ~:[~;DATABASE ~:*~a~]~:[~;SCHEMA ~:*~a~]~:[~;TABLE ~:*~a~]~:[~;TABLESPACE ~:*~a~] FROM ~a;"
                        privilege
                        columns
                        database
                        schema
                        table
                        tablespace
                        role)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tables

(defstruct (table-create-statement (:include sql-statement)) tabdef)

(defun %create-table (tabdef)
  (sql:sql-exec *current-connection*
                (make-table-create-statement :tabdef tabdef)))

(defmethod sql:sql-exec ((connection t) (statement table-create-statement))
  (sql:sql-exec connection
                (let ((*print-circle* nil))
                  (with-output-to-string (s)
                    (format s "CREATE TABLE ")
                    (serialize-for-connection connection (table-create-statement-tabdef statement) s)))))

(defun %copy-table (tabdef &key (to nil) (from nil) (format "csv") (headers t))
  (when (and to from)
    (error "Cannot copy TO and FROM at the same time"))
  (let ((directory (or to from))
        (direction (if to "TO" "FROM")))
    (sql:sql-exec
     *current-connection*
     (format nil "COPY ~a.~a ~a '~a/~a.~a' (FORMAT ~a~:[~;, HEADER~]);"
             (tabdef-schema tabdef)  
             (tabdef-name tabdef)
             direction
             directory
             (tabdef-schema tabdef)
             (tabdef-name tabdef)
             format
             headers))))

(defstruct (table-drop-statement (:include sql-statement)) tabdef &key if-does-not-exist if-not-empty)

(defun %drop-table (tabdef &key (if-does-not-exist :error) (if-not-empty :error))
  (sql:sql-exec
   *current-connection*
   (make-table-drop-statement :tabdef tabdef
                              :if-does-not-exist if-does-not-exist
                              :if-not-empty if-not-empty)))

(defmethod sql:sql-exec ((connection t) (statement table-drop-statement))
  (sql:sql-exec connection
                (format nil "DROP TABLE ~:[IF EXISTS~;~] ~:[~;~:*~a.~]~a ~:[CASCADE~;RESTRICT~]"
                        (ecase (table-drop-statement-if-does-not-exist statement)
                          (:ignore nil)
                          (:error t))
                        (tabdef-schema (table-drop-statement-tabdef statement))
                        (tabdef-name (table-drop-statement-tabdef statement))
                        (ecase (table-drop-statement-if-not-empty statement)
                          (:force nil)
                          (:error t)))))

(defun %truncate-table (tabdef &key (cascade t))
  (sql:sql-exec
   *current-connection*
   (make-sql-truncate :schema (tabdef-schema tabdef)
                      :name (tabdef-name tabdef)
                      :cascade cascade)))

(defun %add-column (tabdef coldef)
  (sql:sql-exec
   *current-connection*
   (format nil "ALTER TABLE ~a.~a ADD COLUMN ~a ~a;"
           (tabdef-schema tabdef)
           (tabdef-name tabdef)
           (coldef-name coldef)
           (map-db-type *current-connection* (coldef-datatype coldef)))))

(defun %drop-column (tabdef coldef)
  (sql:sql-exec
   *current-connection*
   (format nil "ALTER TABLE ~a.~a DROP COLUMN ~a CASCADE;"
           (tabdef-schema tabdef)
           (tabdef-name tabdef)
           (coldef-name coldef))))

(defun %add-primary-key (tabdef tabcon)
  (sql:sql-exec
   *current-connection*
   (make-tabmod :schema (tabdef-schema tabdef)
                :table (tabdef-name tabdef)
                :modification tabcon)))

(defun %add-unique-key (tabdef tabcon)
  (sql:sql-exec
   *current-connection*
   (make-tabmod :schema (tabdef-schema tabdef)
                :table (tabdef-name tabdef)
                :modification tabcon)))

(defun %add-foreign-key (tabdef fk)
  (sql:sql-exec
   *current-connection*
   (make-tabmod :schema (tabdef-schema tabdef)
                :table (tabdef-name tabdef)
                :modification fk)))

(defun %drop-key (tabdef key)
  (sql:sql-exec
   *current-connection*
   (format nil "ALTER TABLE ~a.~a DROP CONSTRAINT ~a"
           (tabdef-schema tabdef)
           (tabdef-name tabdef)
           (tabcon-name key))))
  
(defun %add-table-constraint (table constraint)
  (declare (ignore table constraint))
  (error "nyi"))

(defun %drop-table-constraint (table constraint)
  (declare (ignore table constraint))
  (error "nyi"))

(defun %change-column (table coldef)
  (declare (ignore table coldef))
  (error "nyi"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
