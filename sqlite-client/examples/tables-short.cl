;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   Create, Alter, Drop table
;;; Author        Michael Kappert 2019
;;; Last Modified <michael 2019-12-14 21:15:40>

(in-package sqlite-client)

(defparameter *examples-directory*
  (make-pathname :directory (pathname-directory #.*load-truename*)))

(defparameter *db*
  (namestring (merge-pathnames "tables.sdb" *examples-directory*)))

(setf (log2:log-level "sqlite-client") log2:+info+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Connect to tables.sdb (create if does not exist), and create table TEST
;;; The WITH-CURRENT-CONNECTION macro opens a DB connection, and binds *CURRENT-CONNECTION* to it.
;;; It also binds the provided variable (C in this case) to the connection.
;;; All the 'embedded SQL' functions (names beginning with % and ?) use *current-connection*.

(with-current-connection (c *db* :if-does-not-exist :create)
  
  (%drop-table (make-tabdef :name "test") :if-does-not-exist :ignore)
  (%create-table
   (deftable "test"
                :columns (("k" :datatype 'text)
                          ("u" :datatype 'text)
                          ("c" :datatype 'text))
                :constraints (
                              (:primary-key "test_pk_k" :columns (k))
                              (:unique-key "test_unique_u" :columns (u)))))  

  (?insert-into 'test :values '("k-11" "u-11" "I succeed"))
  (ignore-errors
    (?insert-into 'test :values '("k-11" "u-12" "I fail")))

  (?insert-into 'test :values '("k-12" "u-13" "I succeed"))
  (ignore-errors
    (?insert-into 'test :values '("k-13" "u-13" "I fail")))

  ;; Only the 'I succeed' rows should be inserted
  (log2:info "Result: ~a"
             (multiple-value-list
              (?select '* :from 'test)))

  (with-transaction ()
    (?insert-into 'test :values '("k-21" "u-21" "TX I succeed"))
    (?insert-into 'test :values '("k-21" "u-22" "TX I fail"))
    (?insert-into 'test :values '("k-22" "u-23" "TX I succeed"))
    (?insert-into 'test :values '("k-23" "u-23" "TX I fail"))))

(with-current-connection (c *db* :if-does-not-exist :create)
  
  ;; Transaction rolled back - no rows inserted.
  (log2:info "Result: ~a"
             (multiple-value-list
              (?select '* :from 'test))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
