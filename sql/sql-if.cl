;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael 2014
;;; Last Modified <michael 2019-12-14 13:28:59>

(in-package :sql)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; All SQL commands are called through SQL-EXEC and SQL-QUERY, where the
;;; sql statement is a subtype of SQL-STATEMENT. The backends provide methods on
;;; SQL-EXEC and SQL-QUERY to serialize the statement to appropriate SQL
;;; syntax and send it over the wire.

;;; There are currently three backends:
;;; - pg-socket (PostgreSQL/socket, OS independent)
;;; - pg-client (PostgreSQL/libpq, linux only, slightly faster)
;;; - hdb-odbc (HANA DB, Woe32 and Woe64, libodbcHDB)
 
(defstruct sql-statement)

(defclass sql-connection () ())

(defgeneric sql-exec (connection statement)
  (:documentation "Execute an SQL statement other than a SELECT"))

(defgeneric sql-query (connection statement)
  (:documentation "Execute an SQL SELECT statement"))

(defgeneric serialize-for-connection (connection thing stream)
  (:documentation "This function is called to serialize SQL objects into SQL fractions"))

(defgeneric fetch (transient-table result &key field-mapper)
  ;; Currently not used !
  ;; #ToDo: Think about how to integrate this into ?select/sql-query.
  ;; -> for example, require sql-query to return am object of type sql-result
  ;;    (instead of (values columns rows)) and dispatch on it.
  (:documentation "This function is called by ?select to fetch data rows from the DB"))

(defmethod sql:sql-exec ((conn t) (sql-statement sql-statement))
  ;; The default SQL-EXEC method simply serializes the command into a string.
  ;; Each connection type C provides a method
  ;;   SQL-EXEC (conn C) (statement string)
  ;; and, if necessary, specialized methods for SERIALIZE-FOR_CONNECTION.
  (sql:sql-exec conn
                (with-output-to-string (s)
                  (serialize-for-connection conn sql-statement s))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
