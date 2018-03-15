;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael 2014
;;; Last Modified <michael 2018-03-15 21:58:34>

(defpackage "SQL"
  (:use "COMMON-LISP"
        #+:clisp "CLOS"
        #+:sbcl "SB-MOP"
        #+:ccl "CCL")
  (:shadow "UNION")
  (:export

   ;; Selecting the current connection
   sql-connection
   with-connection

   ;; Functions for creating and terminating connections
   ;;     with-open-connection
   ;;     %connect
   ;;     %disconnect
   ;;  are provided by the backend packages
   ;;  pg-socket, pg-client and hdb-odbc


   ;; SQL - basic interaction
   sql-exec
   sql-query
   fetch
   serialize-for-connection
      
   ;; where does this belong?
   dbinfo
   make-dbinfo
   dbinfo-name
   dbinfo-owner
           
   userinfo
   make-userinfo
   userinfo-name
   userinfo-password

   ;; SQL Statements
   sql-statement
   
   ;; Embedded SQL - DDL Entities 
   schema
   schema-create-statement
   schema-drop-statement
   make-schema
   schema-name
   schema-tables

   ;; Managing schemas - not really SQL, but used in the information_schema interface
   defschema
   *schema-lib*
   use-schema
   get-schema-by-name
   load-db-schema
   find-db-schema

   tabdef
   make-tabdef
   tabdef-p
   copy-tabdef
   tabdef-schema
   tabdef-name
   tabdef-columns
   tabdef-constraints
   create-tabdef
   table-create-statement
   table-drop-statement
   
   make-tabmod

   make-coldef
   coldef-p
   copy-coldef
   coldef-name
   coldef-datatype
   coldef-constraint
           
   make-tabcon
   tabcon-p
   copy-tabcon
   tabcon-schema
   tabcon-name
   tabcon-columns
   tabcon-primary-key
   
   make-primary-key
   primary-key-p
   primary-key-name
   primary-key-columns
           
   make-unique-key
   unique-key-p
   unique-key-name
   unique-key-columns
           
   make-foreign-key
   foreign-key-p
   foreign-key-name
   foreign-key-columns
   foreign-key-referenced-table
   foreign-key-referenced-columns
   foreign-key-on-delete
   foreign-key-on-update

   ;; Embedded SQL - DML
   sql-query
   make-sql-query
   sql-query-sellist
   sql-query-table-expression

   sql-table-expression
   make-sql-table-expression
   sql-table-expression-from
   sql-table-expression-where
   
   join
   make-join
   join-kind
   join-left
   join-right
   join-on
   join-using

   ;; Embedded SQL - expression
   sql-tuple
   make-sql-tuple
   sql-tuple-elements

   sql-junction
   make-sql-junction
   sql-junction-op
   sql-junction-args

   sql-comparison
   make-sql-comparison
   sql-comparison-op
   sql-comparison-column
   sql-comparison-value
   
   ;; Embedded SQL - DDL
   *sql-readtable*

   %grant
   %revoke
   
   %create-database
   %drop-database
   
   %create-schema
   make-schema-create-statement
   schema-create-statement-schema
   schema-create-statement-authorization
   schema-create-statement-tables

   %drop-schema
   make-schema-drop-statement
   schema-drop-statement-schema
   schema-drop-statement-if-does-not-exist
   schema-drop-statement-if-not-empty

   %create-role
   %alter-role
   %drop-role
   %drop-owned
           
   %create-table
   %alter-table
   %drop-table
   
   %add-column
   %drop-column
   %change-column

   %add-primary-key
   %add-unique-key
   %add-foreign-key
   %drop-key

   ;; Embedded SQL - Transaction Control
   with-transaction
   with-nested-transaction
   mode<=

   %begin
   %commit
   %rollback
   %savepoint
   %release-savepoint
   %rollback-to-savepoint
   %set-transaction-isolation

   ;; Embedded SQL - Data Manipulation
   %copy-table
   %truncate-table

   ?update
   ?insert-into
   ?insert
   ?delete
   ?select
   ?inner-join
   ?left-join
   ?alias
   ?not
   ?and
   ?or
   ?unnest
   ?=
   ?<
   ?<=
   ?>
   ?>=
   ?<>
   ?like
   ?in
   ?between
   ?max
   ?min
   ?some
   ?count
   ?t
   ?+
   ?-
   ?*
   ?/

   ;; Tuples (representing rows, not to be confused with SQL QL tuples representing columns)
   ensure-tuple-class
   ensure-tuple-class%
   tuples
   tuple-table
   tuple-columns
   tuple-values
   db-tuple
   transient-table
   create-transient-table
   
   ;; DB types
   map-db-type
   
   id
   uuid
   identifier
   string
   text
   boolean
   integer
   decimal
   money
   percent
   raw
   timestamp
   duration
   date

   ;; UUIDs
   create-uuid
   
   ;;
   make-keyword))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
