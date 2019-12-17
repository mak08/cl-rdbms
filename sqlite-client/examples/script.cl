;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   Examples using SQLite3
;;; Author        Michael Kappert 2019
;;; Last Modified <michael 2019-12-18 00:15:20>

(in-package sqlite-client)

(defparameter *examples-directory*
  (make-pathname :directory (pathname-directory #.*load-truename*)))

(defparameter *db*
  (namestring (merge-pathnames "script.sdb" *examples-directory*)))

;;; logging to an Emacs buffer is too slow to log 100.000 SQL inserts.
;;; Even logging to file more than doubles the run time.
(setf (log2:log-level "sqlite-client") log2:+warning+)

(unless (boundp '*db*)
  (defparameter *db* "/home/michael/Repository/cl-rdbms/sqlite-client/examples/script.sdb"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Connect to test.sdb (create if does not exist), and create table BOOKS in the 'main' schema.
;;; The WITH-CURRENT-CONNECTION macro opens a DB connection, and binds *CURRENT-CONNECTION* to it.
;;; It also binds the provided variable (C in this case) to the connection.
;;; All the 'embedded SQL' functions (names beginning with % and ?) use *current-connection*.

(with-current-connection (c *db* :if-does-not-exist :create) 
  (%create-table (make-tabdef :name "books" :columns '(title author))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Insert 100.000 books in a transaction.
;;; The WITH-TRANSACTION macro accepts an :ISOLATION parameter, but the SQLite implementation ignores it.

(with-current-connection (c *db*) 
  (with-transaction ()
    (dotimes (k 100000)
      (?insert (list (format nil "Harry Potter ~a" k)
                     "J K Rowling")
               :into 'books))
    (?select (?alias (?count '*) 'total) :from 'books)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create schema SCHEMA_01 and table SCHEMA_01.BOOKS 

(with-current-connection (c *db*) 
  (%create-schema "schema_01")
  (%create-table (make-tabdef :name "books" :schema "schema_01" :columns '(title author))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Insert 100.000 books into SCHEMA_01.BOOKS

(with-current-connection (c *db*) 
  (with-transaction ()
    (dotimes (k 100000)
      (?insert (list (format nil "Harry Potter ~a" k)
                     "J K Rowling")
               :into 'schema_01.books))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Select from SCHEMA_01.BOOKS

(with-current-connection (c *db*) 
  (with-transaction ()
    (?select '*
             :from  'schema_01.books
             :where (?like 'title "%555"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Select from SCHEMA_01.BOOKS, store result in anonymous tuple class
;;; The method TITLE is created on-the-fly from the tuple class definition in the :INTO clause

(with-current-connection (c *db*) 
  (with-transaction ()
    (title (aref (tuples
                  (?select '*
                           :from 'schema_01.books
                           :where (?like 'title "%555")
                           :into '("title" "author")))
                 10))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Select from BOOKS, store result in anonymous tuple class
;;; The same tuple class that was created on-the-fly in the previous statement is re-used here.
;;; Anonymous tuple classes are identified by the ordered set of fields.

(with-current-connection (c *db*) 
  (with-transaction ()
    (title (aref (tuples
                  (?select '*
                           :from 'books
                           :where (?like 'title "%555")
                           :into '("title" "author")))
                 10))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Update some rows

(with-current-connection (c *db*) 
  (with-transaction ()
    (?update 'schema_01.books
             :set 'author :to "John Doe"
             :where (?like 'title "%555"))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
