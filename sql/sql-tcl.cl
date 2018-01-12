;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael 2013
;;; Last Modified <michael 2018-01-12 00:27:09>

(in-package :sql)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The with-transaction abstraction requires to hide the db connection:
;; All commands must be executed on the same db !

(defmacro with-transaction ((&key (isolation :read-committed)) &body statements)
  ;; todo: design an abstraction layer that hides the connection
  `(let ((success nil))
     (unwind-protect
          (let ((results ())) 
            (%begin)
            (%set-transaction-isolation ,isolation)
            (setf results
                  (multiple-value-list
                   (progn ,@statements)))
            (%commit)
            (setf success t)
            (values-list results))
       (when (not success)
         (%rollback)))))

(defvar *sp-level* 0)
(defmacro with-nested-transaction (&body statements)
  ;; ToDo: design an abstraction layer that hides the connection
  `(let* ((*sp-level* (1+ *sp-level*))
          (savepoint (format nil "SP_~d" *sp-level*))
          (success nil))
       (unwind-protect
            (let ((results ()))
              ;; (%savepoint savepoint)
              (setf results
                    (multiple-value-list
                     (progn ,@statements)))
              ;; (%release-savepoint savepoint)
              (setf success t)
              (values-list results))
         (when (not success)
           ;; (%rollback-to-savepoint savepoint)
           ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transactions + Locking

(defparameter +isolation-modes+ '(:read-committed :repeatable-read :serializable))
(defparameter +lock-modes+ '(:none :share :update))

(defun mode<= (m1 m2)
  (<= (position m1 +lock-modes+) (position m2 +lock-modes+)))

(defstruct (begin-command (:include sql-statement)))
(defun %begin ()
  (sql:sql-exec *current-connection* (make-begin-command)))
(defmethod sql:sql-exec ((conn t) (statement begin-command))
  (sql:sql-exec conn (format nil "BEGIN;")))

(defstruct (commit-command (:include sql-statement)))
(defun %commit ()
  (sql:sql-exec *current-connection* (make-commit-command)))
(defmethod sql:sql-exec ((conn t) (statement commit-command))
  (sql:sql-exec conn (format nil "COMMIT;")))

(defstruct (rollback-command (:include sql-statement)))
(defun %rollback ()
  (sql:sql-exec *current-connection* (make-rollback-command)))
(defmethod sql:sql-exec ((conn t) (statement rollback-command))
  (sql:sql-exec conn (format nil "ROLLBACK;")))

(defstruct (transaction-command (:include sql-statement)) isolation-level)
(defun %set-transaction-isolation (mode)
  (sql:sql-exec *current-connection* (make-transaction-command :isolation-level mode)))
(defmethod sql:sql-exec ((conn t) (statement transaction-command))
  (sql:sql-exec conn 
                (format nil "SET TRANSACTION ISOLATION LEVEL ~a"
                        (ecase (transaction-command-isolation-level statement)
                          (:read-committed "READ COMMITTED")
                          (:repeatable-read "REPEATABLE READ")
                          (:serializable "SERIALIZABLE")))))

(defun %savepoint (name)
  (sql:sql-exec *current-connection* (format nil "SAVEPOINT ~a;" name)))
(defun %rollback-to-savepoint (name)
  (sql:sql-exec *current-connection* (format nil "ROLLBACK TO SAVEPOINT ~a;" name)))
(defun %release-savepoint (name)
  (sql:sql-exec *current-connection* (format nil "RELEASE SAVEPOINT ~a;" name)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
