;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description  Low-level interface to the backend 
;;; Author         Michael 2014
;;; Last Modified <michael 2020-01-02 20:41:44>

(in-package :sql)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; All SQL commands are called through SQL-EXEC or SQL-QUERY, where the
;;; connection is a subclass of SQL-CONNECTION and the sql statement is a subtype
;;; of SQL-STATEMENT.
;;; The backends provide methods on SQL-EXEC, SQL-QUERY and SERIALIZE-FOR-CONNECTION
;;; to serialize the statement to appropriate SQL syntax and take additional actions
;;; if required.

;;; There are currently three backends:
;;; - sqlite-client: SQLite3 via libsqlite3
;;; - pg-client: PostgreSQL via libpq, Linux
;;; - pg-socket: PostgreSQL via socket, OS independent
;;; - hdb-odbc: HANA DB via libodbcHDB, Windows10
 
(defstruct sql-statement)

(defclass sql-connection () ())

(defgeneric sql-exec (connection statement)
  (:documentation "Execute an SQL statement other than a SELECT"))

(defgeneric sql-query (connection statement)
  (:documentation "Execute an SQL SELECT statement"))

(defgeneric serialize-for-connection (connection thing stream)
  (:documentation "This function is called to serialize SQL objects into SQL fragments"))

(defgeneric fetch (transient-table result &key field-mapper)
  ;; #ToDo: Think about how to integrate this into ?select/sql-query.
  ;; -> for example, require sql-query to return am object of type sql-result
  ;;    (instead of (values columns rows)) and dispatch on it.
  (:documentation "This function is called by ?select to fetch data rows from the DB"))

(defgeneric output-container-for-spec (spec &key mode))

(defmethod sql-exec ((conn t) (sql-statement sql-statement))
  ;; The default SQL-EXEC method simply serializes the command into a string
  ;; using SERIALIZE-FOR-CONNECTION.
  ;; Backends should specialize SERIALIZE-FOR-CONNECTION and specialize SQL-EXEC
  ;; only if more than one SQL command needs to be issued.
  (sql:sql-exec conn
                (with-output-to-string (s)
                  (serialize-for-connection conn sql-statement s))))

(defgeneric load-schema (connection name)
  (:documentation "Load a schema definition from the DB"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
