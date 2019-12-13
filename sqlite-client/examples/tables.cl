;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   Create, Alter, Drop table
;;; Author        Michael Kappert 2019
;;; Last Modified <michael 2019-12-13 00:03:30>

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
    (?insert-into 'test :values '("k-1" "u-1" "I succeed")))
  (ignore-errors
    (?insert-into 'test :values '("k-1" "u-2" "I fail")))
  (ignore-errors
    (?insert-into 'test :values '("k-2" "u-3" "I succeed")))
  (ignore-errors
    (?insert-into 'test :values '("k-3" "u-3" "I fail")))
  ;; Only the 'I succeed' rows should be inserted
  (log2:info "Result: ~a"
             (multiple-value-list
              (?select '* :from 'test)))

  (with-transaction ()
    (ignore-errors
      (?insert-into 'test :values '("k-1" "u-1" "TX I succeed")))
    (ignore-errors
      (?insert-into 'test :values '("k-1" "u-2" "TX I fail")))
    (ignore-errors
      (?insert-into 'test :values '("k-2" "u-3" "TX I succeed")))
    (ignore-errors
      (?insert-into 'test :values '("k-3" "u-3" "TX I fail"))))
  ;; No rows should be inserted
  (log2:info "Result: ~a"
             (multiple-value-list
              (?select '* :from 'test))))



;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
