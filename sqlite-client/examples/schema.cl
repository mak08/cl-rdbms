;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2019-12-19 18:57:18>

(defpackage :cl-rdbms-example
    (:use "COMMON-LISP" "SQLITE-CLIENT" "SQL"))

(in-package :cl-rdbms-example)

(defparameter *examples-directory*
  (make-pathname :directory (pathname-directory #.*load-truename*)))

(defparameter *db*
  (namestring (merge-pathnames "script.sdb" *examples-directory*)))


(unless (boundp '*db*)
  (defparameter *db* "/home/michael/Repository/cl-rdbms/sqlite-client/examples/script.sdb"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQL datatype mapping
;;;
;;; ToDo: datatype definition should be provided by the SQL module...

(defparameter +serial+  "serial")
(defparameter +int+  "int")
(defparameter +smallid+  "char(10)")
(defparameter +largeid+  "char(40)")
(defparameter +smallname+  "varchar(20)")
(defparameter +largename+  "varchar(40)")
(defparameter +text+  "varchar")
(defparameter +point+ "point")
(defparameter +date+  "date")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data model naming conventions
;;; 1 - Primary key fields: If possible use a single primary field named 'id'.
;;; 2 - Attribute fields don't include the table name
;;; 3 - Reference fields are named <table>_<id>
;;; 4 - Primary keys are named pk_<table>
;;; 5 - Foreign keys are named fk_<table>_<field> (because of (3) <field> includes
;;;     the foreign table name)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defschema "books"
  (:table "book"
          :columns (("id" :datatype +serial+)
                    ("title" :datatype +text+)
                    ("author_id"  :datatype +int+)
                    ("status" :datatype +int+))
          :constraints ((:primary-key "pk_book" :columns ("id"))
                        (:unique-key "pk_book_title" :columns ("title"))))
  
  (:table "author"
          :columns (("id" :datatype +serial+)
                    ("firstname" :datatype +smallname+)
                    ("lastname" :datatype +smallname+))
          :constraints ((:primary-key "pk_author" :columns ("id")))))

(with-current-connection (c "/home/michael/Repository/cl-rdbms/sqlite-client/examples/schema.sdb" :if-does-not-exist :create)
  (create-schema (get-schema-by-name "books")))

(with-current-connection (c "/home/michael/Repository/cl-rdbms/sqlite-client/examples/schema.sdb" :if-does-not-exist :create)
  (with-transaction ()
      (?insert-into 'books.author :values '("2" "Isaac" "Newton"))
      (?insert-into 'books.book :values '("Principia Mathematicae" "2") :columns '(title author_id))))


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
