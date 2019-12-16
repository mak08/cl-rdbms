;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael 2014
;;; Last Modified <michael 2019-12-16 20:37:11>

(defpackage "SQL"
  (:use "COMMON-LISP"
        "RDPARSE"
        #+:clisp "CLOS"
        #+:sbcl "SB-MOP"
        #+:ccl "CCL")
  (:shadow "UNION")
  (:export

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Database connection

   ;; The following two macros are implemented and exported from backend packages:
   ;; with-open-connection ((conn &rest args &key &allow-other-keys) &body forms)
   ;; with-current-connection ((conn &rest args &key &allow-other-keys) &body forms)

   ;; The backend packages define their own connection classes as subclasses of
   sql-connection

   ;; Select current connection
   with-connection

   ;; SQL - basic interaction
   sql-exec
   sql-query
   fetch
   serialize-for-connection

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Embedded SQL - DDL Entities 

   ;; SQL Statement
   sql-statement

   
   ;; Database
   database-create-statement ; paradoxically, this exists because of SQLite
   database-create-statement-name

   database-drop-statement
   database-drop-statement-name
   
   ;; Schema
   defschema
   %create-schema
   get-schema-by-name
   load-db-schema
   find-db-schema

   schema
   schema-create-statement
   schema-create-statement-name
   schema-create-statement-owner
   schema-drop-statement
   make-schema
   schema-name
   schema-tables
   
   ;; Table
   deftable
   %create-table

   create-tabdef
   make-tabdef
   tabdef
   tabdef-p
   copy-tabdef
   tabdef-schema
   tabdef-name
   tabdef-columns
   tabdef-constraints

   table-create-statement
   table-drop-statement
   table-drop-statement-tabdef
   table-drop-statement-if-does-not-exist
   table-drop-statement-if-not-empty

   
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
   primary-key
   primary-key-p
   primary-key-name
   primary-key-columns
           
   make-unique-key
   unique-key
   unique-key-p
   unique-key-name
   unique-key-columns
           
   make-foreign-key
   foreign-key
   foreign-key-p
   foreign-key-name
   foreign-key-columns
   foreign-key-referenced-table-schema
   foreign-key-referenced-table
   foreign-key-referenced-columns
   foreign-key-on-delete
   foreign-key-on-update

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

   transaction-command
   transaction-command-isolation-level
   
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
   
   ;; Internal use
   !{}
   @[]
   ?[]
   make-keyword))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
