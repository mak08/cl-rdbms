;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael 2013
;;; Last Modified <michael 2018-01-12 01:49:10>

(in-package "PG-CLIENT")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enum ExecStatusType
(defconstant PGRES_EMPTY_QUERY  0)    #| empty query string was executed |#
(defconstant PGRES_COMMAND_OK   1)    #| a query command that doesn't return
                                       * anything was executed properly by the
                                       * backend |#
(defconstant PGRES_TUPLES_OK 2)       #| a query command that returns tuples was
                                       * executed properly by the backend, PGresult
                                       * contains the result tuples |#
(defconstant PGRES_COPY_OUT 3)        #| Copy Out data transfer in progress |#
(defconstant PGRES_COPY_IN 4)         #| Copy In data transfer in progress |#
(defconstant PGRES_BAD_RESPONSE 5)    #| an unexpected response was recv'd from the
                                       * backend |#
(defconstant PGRES_NONFATAL_ERROR 6)  #| notice or warning message |#
(defconstant PGRES_FATAL_ERROR 7)     #| query failed |#
(defconstant PGRES_COPY_BOTH 8)       #| Copy In/Out data transfer in progress |#
(defconstant PGRES_SINGLE_TUPLE 9)    #| single tuple from larger resultset |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API functions

(defclass pg-client-connection (postgres-connection)
  ((host :accessor host :initarg :host)
   (port :accessor port :initarg :port)
   (user :accessor user :initarg :user)
   (database :accessor database :initarg :database)
   (conn :accessor conn :initarg :conn)))

(defmacro with-open-connection ((connection database &key (user "crmadmin") (password user)) &body forms)
  "Provides a new connection"
  `(progn 
     (let ((,connection
             (%connect% ,database
                       :user ,user
                       :password ,password)))
       (unwind-protect
         (progn ,@forms)
         (%disconnect% ,connection)))))

(defmethod sql:sql-exec ((conn pg-client-connection) (sql-statement string))
  (sql-exec% conn sql-statement))

(defmethod sql:sql-query ((conn pg-client-connection) (sql-statement string))
  (sql-exec% conn sql-statement))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation

(defvar *active-conn-ht* (make-hash-table))
(defun active-p (conn)
  (gethash conn *active-conn-ht*))
(defun set-active-p (conn value)
  (setf (gethash conn *active-conn-ht*) value))
(defsetf active-p set-active-p)

(defun %connect% (database &key (user "dbuser") (password user) (port 5432))
  ;; many missing parameters: host, port, auth params...
  (let ((connect-params
          (format nil "user=~a~:[~; port=~:*~a~]~:[~; password=~:*~a~]~:[~; dbname=~:*~a~]" user port password database)))
    (let* ((conn (PQconnectdb connect-params))
           (err (PQerrorMessage conn)))
      (cond ((string= err "")
             (setf (active-p conn) t)
             (let ((connection
                    (make-instance 'pg-client-connection :conn conn
                                :user user
                                :host (or (PQhost conn) "localhost")
                                :port (PQport conn)
                                :database (PQdb conn))))
               (log2:info "CONNECT: User=~a, Database=~a, Address=~a:~a"
                             user
                             (database connection)
                             (host connection)
                             (port connection))
               (values connection)))
            (t
             (error err))))))

(defun %disconnect% (connection)
  (cond
    ((active-p (conn connection))
     (log2:info "DISCONNECT: User=~a, Database=~a, Address=~a:~a"
                   (user connection)
                   (database connection)
                   (host connection)
                   (port connection))
     (PQfinish (conn connection))
     (setf (active-p (conn connection)) nil))
    (t
     (log2:warning "DISCONNECT: connection to ~a:~a (User=~a, Database=~a) already closed"
                      (host connection)
                      (port connection)
                      (user connection)
                      (database connection))
     (cerror "Continue" "Connection already closed"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Executing SQL strings

(defvar *simulate* nil)

(defun sql-exec% (conn sql-statement)
  ;; log2 is awfully slow even if nothing is logged.
  ;; (log2:info "~a" sql-statement)
  (when (not *simulate*)
    (let* ((result (PQexec (conn conn) sql-statement))
           (status (PQresultStatus result)))
      (unwind-protect
           (cond
             ((eql status PGRES_FATAL_ERROR)
              (error (PQerrorMessage (conn conn))))
             ((eql status PGRES_COMMAND_OK)
              t)
             ((eql status PGRES_TUPLES_OK)
              (fetch% nil result))
             (t
              (error (PQresultErrorMessage result))))
        (PQclear result)))))

(defun fetch% (into result &key field-mapper)
  (let* ((num-cols (PQnfields result))
         (num-rows (PQntuples result)))
    (values
     (loop :for k :below num-cols
        :collect (PQfname result k))
     (loop :for rownum :below num-rows
        :collect (loop :for colnum :below num-cols
                    :collect (unless (eql (PQgetisnull result rownum colnum) 1)
                               (PQgetvalue result rownum colnum))))
     num-rows)))

(defmethod sql:fetch ((transient-table transient-table) (result t) &key (field-mapper #'default-field-mapper))
  (let* ((package (symbol-package (class-name (tuple-class transient-table))))
         (num-cols (PQnfields result))
         (num-rows (PQntuples result))
         (slots (mapcar #'slot-definition-name (class-direct-slots (tuple-class transient-table))))
         (columns
           (loop
              :for k :below num-cols
              :for column =  (PQfname result k)
              :collect (let ((colsym (find-symbol (string-upcase column) package)))
                         (or (find (funcall field-mapper colsym) slots)
                             (warn "Table ~a has no column ~a, valid columns are ~a. Values will be discarded."
                                   transient-table column slots))))))
    (dotimes (rownum num-rows)
      (let ((tuple (append-tuple transient-table nil)))
        (loop
           :for column :in columns
           :for colnum :below num-cols
           :when column
           :do (setf (slot-value tuple  column)
                       (PQgetvalue result rownum colnum)))))
    (values transient-table
            (loop :for k :below num-cols :collect (PQfname result k))
            num-rows)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PGClient interface

(eval-when (:load-toplevel :compile-toplevel :execute)
  (define-foreign-library libpq
    (:unix
     "libpq.so.5")
    (t
     (:default "libpq")))
  (define-foreign-library libuuid
    (:unix
     "libuuid.so.1.3.0")
    (t
     (:default "libuuid"))))

(use-foreign-library libpq)
(use-foreign-library libuuid)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PQ Types

(defctype PGconn :pointer)
(defctype PGresult :pointer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 31.1. Database Connection Control Functions

(defcfun ("PQconnectdb" :library libpq)
    PGconn
  (connect-string :string))

(defcfun ("PQfinish" :library libpq)
    :void
  (connection PGconn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 31.2. Connection Status Functions

(defcfun ("PQhost" :library libpq)
    :string
  (connection PGconn))

(defcfun ("PQport" :library libpq)
    :string
  (connection PGconn))

(defcfun ("PQdb" :library libpq)
    :string
  (connection PGconn))

(defcfun ("PQerrorMessage" :library libpq)
    :string
  (connection PGconn))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 31.3 Command Execution Functions

;;; 31.3.1 Main Functions

(defcfun ("PQexec" :library libpq)
    PGResult
  (connection PGConn)
  (command :string))

(defcfun ("PQclear" :library libpq)
    :void
  (result PGResult))

(defcfun ("PQresultStatus" :library libpq)
    :int32
  (result PGResult))

;;; 31.3.2 Retrieving Query Result Information

(defcfun ("PQresultErrorMessage" :library libpq)
    :string
  (connection PGresult))

(defcfun ("PQnfields" :library libpq)
    :int32
  (result PGResult))

(defcfun ("PQntuples" :library libpq)
    :int32
  (result PGResult))

(defcfun ("PQfname" :library libpq)
    :string
  (result PGResult)
  (colnum :int32))

(defcfun ("PQcmdTuples" :library libpq)
    :string
  (result PGresult))

(defcfun ("PQgetvalue" :library libpq)
    :string
  (result PGResult)
  (row-num :int32)
  (col-num :int32))

(defcfun ("PQgetisnull" :library libpq)
    :int32
  (result PGResult)
  (row-num :int32)
  (col-num :int32))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UUIDS

(defun uuid-generate ()
  (with-foreign-object (array :uint8 16)
    (uuid_generate array)
    (let ((uuid (make-array 16)))
      (loop
         :for k :below 16
         :do (setf (aref uuid k) (mem-aref array :uint8 k)))
      uuid)))

(defcfun ("uuid_generate" uuid_generate :library libuuid)
    :void
  (result :pointer))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
