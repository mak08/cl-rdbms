;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   Create, Alter, Drop table
;;; Author        Michael Kappert 2019
;;; Last Modified <michael 2019-12-16 20:18:59>

(in-package sqlite-client)

(defparameter *examples-directory*
  (make-pathname :directory (pathname-directory #.*load-truename*)))

(defparameter *db*
  (namestring (merge-pathnames "tables.sdb" *examples-directory*)))

(setf (log2:log-level "sqlite-client") log2:+info+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Connect to tables.sdb (create if does not exist), and create table BOOKS
;;; The WITH-CURRENT-CONNECTION macro opens a DB connection, and binds *CURRENT-CONNECTION* to it.
;;; It also binds the provided variable (C in this case) to the connection.
;;; All the 'embedded SQL' functions (names beginning with % and ?) use *current-connection*.

(with-current-connection (c *db*
                            :if-does-not-exist :create) 
  (%create-table (make-tabdef :name "books" :columns '(title author))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Connect to tables.sdb, fail (do not create) if it doesn't exist.
;;; %drop-table requires a tabdef, but the :columns need not be provided.

(with-current-connection (c *db*
                            :if-does-not-exist :fail) 
  (%drop-table (make-tabdef :name "books")))

(with-current-connection (c *db*)
  
  (%drop-table (make-tabdef :name "root") :if-does-not-exist :ignore)
  (%drop-table (make-tabdef :name "child") :if-does-not-exist :ignore)
  
  (%create-table
   (make-tabdef :name "root"
                :columns (list (make-coldef :name "k"
                                            :datatype 'text)
                               (make-coldef :name "u"
                                            :datatype 'text)
                               (make-coldef :name "c" :datatype 'text))
                
                :constraints (list
                              (make-primary-key :name "test_pk_k" :columns '(k))
                              (make-unique-key :name "test_unique_u" :columns '(u)))))
  (%create-table
   (make-tabdef :name "child"
                :columns (list (make-coldef :name "k"
                                            :datatype 'text)
                               (make-coldef :name "u"
                                            :datatype 'text)
                               (make-coldef :name "c" :datatype 'text))
                
                :constraints (list
                              (make-foreign-key :name "test_pk_k" :columns '(k) :referenced-table "root" :referenced-columns '(k))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PRIMARY and UNIQUE keys

(with-current-connection (c *db*)
  
  (%drop-table (make-tabdef :name "test") :if-does-not-exist :ignore)
  
  (%create-table
   (make-tabdef :name "test"
                :columns (list (make-coldef :name "k"
                                            :datatype 'text)
                               (make-coldef :name "u"
                                            :datatype 'text)
                               (make-coldef :name "c" :datatype 'text))
                
                :constraints (list
                              (make-primary-key :name "test_pk_k" :columns '(k))
                              (make-unique-key :name "test_unique_u" :columns '(u)))))

  (ignore-errors
    (?insert-into 'test :values '("k-11" "u-11" "I succeed")))
  (ignore-errors
    (?insert-into 'test :values '("k-11" "u-12" "I fail")))
  (ignore-errors
    (?insert-into 'test :values '("k-12" "u-13" "I succeed")))
  (ignore-errors
    (?insert-into 'test :values '("k-13" "u-13" "I fail")))
  ;; Only the 'I succeed' rows should be inserted
  (log2:info "Result: ~a"
             (multiple-value-list
              (?select '* :from 'test))))


(with-current-connection (c *db*)
  
  (%drop-table (make-tabdef :name "test") :if-does-not-exist :ignore)
  
  (%create-table
   (deftable "test"
                :columns (("k" :datatype 'text)
                          ("u" :datatype 'text)
                          ("c" :datatype 'text))
                :constraints (
                              (:primary-key "test_pk_k" :columns (k))
                              (:unique-key "test_unique_u" :columns (u)))))


  (with-transaction ()
    (?insert-into 'test :values '("k-21" "u-21" "TX I succeed"))
    (?insert-into 'test :values '("k-21" "u-22" "TX I fail"))
    (?insert-into 'test :values '("k-22" "u-23" "TX I succeed"))
    (?insert-into 'test :values '("k-23" "u-23" "TX I fail"))))

(with-current-connection (c *db*)
  ;; No rows should be inserted
  (log2:info "Result: ~a"
             (multiple-value-list
              (?select '* :from 'test))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
