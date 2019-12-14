;;; -*- lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert
;;; Created        2011-10-31 14:43:51 14:43:51

(defsystem "cl-rdbms"
  :description "RDBMS interface"
  :depends-on ("log2" "cl-utilities" "local-time" "rdparse")
  :default-component-class cl-source-file.cl
  :components ((:module "sql"
                        :pathname "sql"
                        :serial t
                        :components ((:file "sql-package")
                                     (:file "macros")
                                     (:file "sql-if")
                                     (:file "sql-basic")
                                     (:file "sql-ddl")
                                     (:file "sql-dml")
                                     (:file "sql-tcl")
                                     (:file "sql-tuples")
                                     (:file "sql-serializer")))
               
               (:module "edm"
                        :serial t
                        :depends-on ("sql")
                        :components ((:file "edm-package")
                                     (:file "entity")   
                                     (:file "entity-api")
                                     (:file "entity-syntax")
                                     (:file "datamodel")
                                     (:file "eeql")))
               (:module "pg-sql"
                        :pathname "pg-sql"
                        :depends-on ("sql")
                        :serial t
                        :components ((:file "pg-sql-package")
                                     (:file "pg-sql")))
               (:module "pg-socket"
                        :pathname "pg-socket"
                        :depends-on ("sql" "pg-sql")
                        :serial t
                        :components ((:file "pg-socket-package")
                                     (:file "binary-types")
                                     (:file "pg-socket")))
               #+:unix
               (:module "pg-client"
                        :pathname "pg-client"
                        :depends-on ("sql" "pg-sql")
                        :serial t
                        :components ((:file "pg-client-package")
                                     (:file "pg-client")))
               (:module "sqlite-client"
                        :pathname "sqlite-client"
                        :depends-on ("sql")
                        :serial t
                        :components ((:file "sqlite-client-package")
                                     (:file "sqlite-client")
                                     (:file "sqlite-sql")))
               #+:windows
               (:module "hdb-odbc"
                        :pathname "hdb-odbc"
                        :depends-on ("sql")
                        :serial t
                        :components ((:file "hdb-odbc-package")
                                     (:file "hdb-odbc")
                                     (:file "sql-serialization")))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
