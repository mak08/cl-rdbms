;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Copyright      (c)  2013
;;; Last Modified  <michael 2021-03-20 20:59:38>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :hdb-odbc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API functions

(defclass odbc-connection (sql-connection)
  ((connection-string :accessor connection-string :initarg :connection-string)
   (env-handle :accessor env-handle :initarg :env-handle)
   (connection-handle :accessor connection-handle :initarg :connection-handle)))

(defmacro with-open-connection ((conn &key (user "d-user") (password  "d-user") server (port "30015")) &body forms)
  (let ((conn-var (gensym)))
    `(let* ((,conn (odbc-connect :user ,user :password ,password :server ,server :port ,port))
            (,conn-var ,conn))
       (unwind-protect 
         (progn ,@forms)
         (when ,conn-var
           (odbc-disconnect ,conn-var))))))

(defmethod sql:sql-exec ((conn odbc-connection) (sql-statement string))
  (log2:info "~a" sql-statement)
  (odbc-exec conn sql-statement))

(defmethod sql:sql-query ((conn odbc-connection) (sql-statement string))
  (log2:info "~a" sql-statement)
  (odbc-query conn sql-statement))

(defmethod map-db-type ((connection odbc-connection) type)
  (let  ((type (if (listp type) (car type) type))
         (length (when (listp type) (cadr type))))
    (when (and length
               (not (member type '(id string))))
      (error "Type ~a does not have a length parameter" type))
    (cond
      ((stringp type)
       (log2:debug "MAP-DB-TYPE: Not mapping type string ~a" type)
       type)
      (t
       (ecase type
         ;; character
         (id          (format () "nvarchar(~a)" (or length 8)))
         (uuid        "nvarchar(36)")
         (identifier  "text")
         (string      (format () "nvarchar(~a)" (or length 1024)))
         (text        "text")
         ;; number
         (boolean     "boolean")
         (integer     "integer")
         (decimal     "decimal")
         (money       "decimal")
         (percent     "decimal")
         ;; byte strings
         (raw         "bytea")
         ;; date & time
         (timestamp   "timestamp with time zone")
         (duration    "interval")
         (date        "date"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQL DDL

(defmethod sql:sql-exec ((conn t) (statement schema-create-statement))
  (sql:sql-exec conn
                (format nil "CREATE SCHEMA ~a OWNED BY ~a"
                        (schema-create-statement-schema statement)
                        (schema-create-statement-authorization statement)))
  (dolist (tabdef (schema-create-statement-tables statement))
    (sql:sql-exec conn (make-table-create-statement :tabdef tabdef))))

(defmethod sql:sql-exec ((conn t) (statement schema-drop-statement))
  ;; HANA does not support the IF EXISTS clause and will raise an error when trying to
  ;; drop a schema that does not exist. We issue the DROP without IF EXISTS and
  ;; simply ignore the error.
  (handler-case 
    (sql:sql-exec conn
                  (format nil "DROP SCHEMA ~a ~:[CASCADE~;RESTRICT~]"
                          (schema-drop-statement-schema statement)
                          (ecase (schema-drop-statement-if-not-empty statement)
                            (:force nil)
                            (:error t))))
    (error (e)
      (ecase (schema-drop-statement-if-does-not-exist statement)
        (:ignore t)
        (:error (error e))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation

(define-foreign-library libodbcHD32
  (:windows "C:\\Programs\\SAP\\HanaClientW32\\libodbcHDB32.dll"))

(define-foreign-library libodbcHD64
  (:windows "C:\\Programs\\SAP\\HanaClientW64\\libodbcHDB.dll"))

#+cffi-features:x86-64
(use-foreign-library libodbcHD64)
#-cffi-features:x86-64
(use-foreign-library libodbcHD32)

(defvar *db-encoding* :utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programmer Interface

(defstruct (handle (:conc-name nil))
  handle-type
  handle)

(defstruct (column-description (:conc-name nil))
  column-number
  column-name
  column-sql-type
  column-size
  column-digits
  column-nullable)

(defconstant +log-level-none+ 0)
(defconstant +log-level-warning+ 1)
(defconstant +log-level-info+ 2)
(defconstant +log-level-debug+ 3)

(eval-when (:load-toplevel :compile-toplevel :execute)
(defconstant SQL_ERROR                 -1)
(defconstant SQL_SUCCESS                0)
(defconstant SQL_SUCCESS_WITH_INFO      1)
(defconstant SQL_HANDLE_ENV             1)
(defconstant SQL_HANDLE_DBC             2)
(defconstant SQL_HANDLE_STMT            3)
(defconstant SQL_HANDLE_DESC            4)
(defconstant SQL_DEFAULT               99)
(defconstant SQL_CHAR                   1)  ;; SQL Type
(defconstant SQL_VARCHAR               12)  ;; SQL Type

;; SQL C Types - Constants from sqlext.h.
;; Most constants refer to constants from sql.h
(defconstant SQL_C_CHAR                 1)
(defconstant SQL_C_NUMERIC              2)
(defconstant SQL_C_LONG                 4)
(defconstant SQL_C_SHORT                5)
(defconstant SQL_C_FLOAT                7) ;; SQL_C_FLOAT = SQL_REAL but SQL_FLOAT also exists !?
(defconstant SQL_C_DOUBLE               8)
(defconstant SQL_C_WCHAR               -8)
(defconstant SQL_C_TIMESTAMP           11)

(defconstant SQL_C_DEFAULT             99)

(defconstant SQL_NULL_DATA -1)
(defconstant SQL_NTS -3)

(defconstant SQL_CONNECT_OPT_DRVR_START 1000)


(defconstant SQL_ATTR_AUTOCOMMIT      102)
(defconstant SQL_AUTOCOMMIT_OFF              0)
(defconstant SQL_AUTOCOMMIT_ON               1)
(defconstant SQL_AUTOCOMMIT_DEFAULT          SQL_AUTOCOMMIT_ON)

)

(defvar *current-statement-handle* ()
  "contains the last executed statement (via odbc-exec or odbc-query).
Note that statement handles are freed upon successful execution of the statement.")

(defvar *log-level* +log-level-debug+)
(defvar *log-stream* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Connections

(defun odbc-connect (&key
                     (driver "HDBODBC32")
                     (server "veHXSws088.dhcp.wdf.sap.corp")
                     (port "30015")
                     (user "SYSTEM")
                     (password ""))
  (let*  ((env-handle
            (sql-alloc-handle SQL_HANDLE_ENV nil))
          (dbc-handle
            (when env-handle (sql-alloc-handle SQL_HANDLE_DBC env-handle))))
    (sql-set-connect-attr dbc-handle SQL_ATTR_AUTOCOMMIT SQL_AUTOCOMMIT_ON)
    (cond
      ((null dbc-handle)
        (error "Driver error: Could not allocate handle"))
      (t
        (multiple-value-bind (success-connect connection-string)
            (sql-driver-connect dbc-handle
                                (format ()
                                        "DRIVER=~a;UID=~a;PWD=~a;SERVERNODE=~a:~a"
                                        driver user password server port))
          (cond
            ((not (eql success-connect 0))
              (multiple-value-bind (code text)
                  (sql-get-diag-rec dbc-handle 1)
                (error "Failed to connect to ~a:~a : ~a ~a" server port code text)))
            (t
              (make-instance 'odbc-connection
               :connection-string connection-string
               :env-handle env-handle
               :connection-handle dbc-handle))))))))

(defun odbc-disconnect (odbc-conn)
  (values
   (sql-disconnect (connection-handle odbc-conn))
   (sql-free-handle (connection-handle odbc-conn)) 
   (sql-free-handle (env-handle odbc-conn))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SELECT

(defun odbc-query (odbc-conn sql-string)
  (let ((stmt-handle
          (sql-alloc-handle SQL_HANDLE_STMT (connection-handle odbc-conn))))
    (setf *current-statement-handle* stmt-handle)
    (sql-exec-direct stmt-handle sql-string)
    (let* ((columns
             (loop
                :for c :below (sql-num-result-cols stmt-handle)
                :collect (sql-describe-col stmt-handle (1+ c)))))
      ;; Fetch results
      (multiple-value-prog1 
        (values (mapcar #'column-name columns)
                (loop
                   :while (sql-fetch stmt-handle)
                   :collect (loop
                               :for column :in columns
                               :collect (sql-get-data stmt-handle column))))
        (sql-free-handle stmt-handle)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INSERT, UPDATE, DELETE \\ other SQL statements

(defun odbc-exec (odbc-conn sql-string)
  (let ((stmt-handle
          (sql-alloc-handle SQL_HANDLE_STMT (connection-handle odbc-conn))))
    (setf *current-statement-handle* stmt-handle)
    (prog1
      (sql-exec-direct stmt-handle sql-string)
      (sql-free-handle stmt-handle))))

(defmacro odbc-exec-parm (odbc-conn sql-string)
  (let ((stmt-handle
          (sql-alloc-handle SQL_HANDLE_STMT (connection-handle odbc-conn))))
    (setf *current-statement-handle* stmt-handle)
    (prog1
      (sql-exec-direct stmt-handle sql-string)
      (sql-free-handle stmt-handle))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Low-level functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQLAllocHandle

(defcfun (sql-alloc-handle% "SQLAllocHandle")
    :int16
  (handle_type :int16)
  (input_handle :pointer)
  (output_handle :pointer))

(defun sql-alloc-handle (handle-type &optional (input-handle nil))
  (let ((input_handle (if input-handle (handle input-handle) (null-pointer)))
        (output_handle (foreign-alloc :pointer)))
    (let ((success 
            (sql-alloc-handle% handle-type input_handle output_handle)))
      (when (eql success 0)
        (make-handle  :handle-type handle-type
                      :handle (mem-ref output_handle :pointer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQLFreeHandle

(defcfun (sql-free-handle% "SQLFreeHandle")
    :int16
  (handle-type :int16)
  (handle :pointer))

(defun sql-free-handle (handle)
  (sql-free-handle% (handle-type handle) (handle handle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQLDriverConnect

(defcfun (sql-driver-connect% "SQLDriverConnect")
    :int16
  (connection-handle :pointer)
  (window-handle :pointer)
  (in-connection-string :string)
  (string-length-1 :int16)
  (out-connection-string :pointer) ; (c-ptr (FFI:C-ARRAY-MAX ffi:char 256)) :out :alloca)
  (buffer-length :int16)
  (string-length-2-ptr :pointer) ; (c-ptr :int16) :out)
  (driver-completion :uint16))

(defun sql-driver-connect (handle connection-string)
  (let*  ((buffer-length 256)
          (out-connection-string (foreign-alloc :char :count buffer-length))
          (out-length (foreign-alloc :int16)))
    (let ((success
            (sql-driver-connect% (handle handle)
                                 (null-pointer)
                                 connection-string
                                 (length connection-string)
                                 out-connection-string
                                 buffer-length
                                 out-length
                                 0)))
      (values success (foreign-string-to-lisp out-connection-string :count (mem-ref out-length :int16) :encoding :ascii)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQLDisconnect

(defcfun (sql-disconnect% "SQLDisconnect")
    :int16
  (connection_handle :pointer))

(defun sql-disconnect (handle)
  (sql-disconnect% (handle handle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQLSetConnectAttr

;; SQLRETURN SQLSetConnectAttr(
;;      SQLHDBC       ConnectionHandle,
;;      SQLINTEGER    Attribute,
;;      SQLPOINTER    ValuePtr,
;;      SQLINTEGER    StringLength);

(defcfun (sql-set-connect-attr% "SQLSetConnectAttr")
    :int8
  (connection_handle :pointer)
  (attribute :int)
  (value_ptr :int)
  (string_length :int))

(defun sql-set-connect-attr (handle attribute value)
  (let ((c-value
          (etypecase value
            (string (foreign-string-alloc value))
            (integer (foreign-alloc :uint :initial-element value)))))
    (sql-set-connect-attr% (handle handle)
                           attribute
                           value
                           (etypecase value
                             (string SQL_NTS)
                             (integer -1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQLExecDirect

(defcfun (sql-exec-direct% "SQLExecDirect")
    :int16
  (statement-handle :pointer)
  (statement-text :string)
  (text-length :int))

(defun sql-exec-direct (statement-handle statement-text)
  ;; (i statement-text)
  (let ((result
          (sql-exec-direct% (handle statement-handle) statement-text (length statement-text))))
    (ecase result
      (#.SQL_SUCCESS
        t)
      (#.SQL_SUCCESS_WITH_INFO
        (multiple-value-bind (result-code result-text)
            (sql-get-diag-rec statement-handle 1)
          (format t "~a:~a" result-code result-text)))
      (#.SQL_ERROR
        (multiple-value-bind (error-code error-text)
            (sql-get-diag-rec statement-handle 1)
          (error "~a:~a" error-code error-text))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQLNumResultCols

(defcfun (sql-num-result-cols% "SQLNumResultCols")
    :int16
  (statement-handle :pointer)
  (column-count-ptr :pointer)) ; (c-ptr :int16) :out))

(defun sql-num-result-cols (statement-handle)
  (let ((num-cols (foreign-alloc :int16)))
    (let ((success
            (sql-num-result-cols% (handle statement-handle) num-cols)))
      (values (mem-ref num-cols :int16) success))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQLDescribeCol

(defcfun (sql-describe-col% "SQLDescribeCol")
    :int16
  (statement-handle :pointer)
  (column-number :uint16)
  (column-name :pointer) ; (c-ptr (ffi:c-array-max ffi:char 256)) :in-out :alloca)
  (buffer-length :int16)
  (name-length-ptr :pointer) ; (c-ptr :int16) :out)
  (data-type-ptr :pointer) ; (c-ptr :int16) :out)
  (column-size-ptr :pointer) ; (c-ptr uint64) :out)
  (decimal-digits-ptr :pointer) ; (c-ptr :int16) :out)
  (nullable-ptr :pointer)) ; (c-ptr :int16) :out))

(defun sql-describe-col (statement-handle column-number)
  (let* ((buffer-length 256)
         (column-name (foreign-alloc :char :count buffer-length))
         (name-length (foreign-alloc :int16))
         (data-type
           ;; Sometimes bogus data-type values are retrieved - allocation problem?
           (foreign-alloc :int16))
         (column-size (foreign-alloc :int))
         (digits (foreign-alloc :int16))
         (nullable (foreign-alloc :int16)))
    (let* ((success
             (sql-describe-col% (handle statement-handle)
                                column-number
                                column-name
                                buffer-length
                                name-length
                                data-type
                                column-size
                                digits
                                nullable))
           (coldesc
             (make-column-description
              :column-number column-number
              :column-name (foreign-string-to-lisp column-name :count (mem-ref name-length :int16) :encoding :ascii)
              :column-sql-type (mem-ref data-type :int)
              :column-size (mem-ref column-size :int)
              :column-digits (mem-ref digits :int16) 
              :column-nullable (mem-ref nullable :int16))))
      (unless (eql success 0)
        (warn "Describe column ~a ~a failed" statement-handle column-number))
      (values coldesc success))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQLGetData

(defcfun (sql-get-data% "SQLGetData")
    :int16 ;; SQLLEN *
  (StatementHandle :pointer)
  (ColumnNumber :uint16) 
  (TargetType :int16)
  (TargetValuePtr :pointer) ;; SQLPOINTER
  (BufferLength :int) ;; SQLLEN
  (StrLen_or_Ind :pointer))


(defun convert-array-to-lisp (foreign-array element-type length &key (lisp-type :character))
  (let ((result
          (ecase lisp-type
            (:character (make-string length))
            (:integer (make-array length))
            (:byte (make-array length)))))
    (unless (null-pointer-p foreign-array)
      (loop
         :for k :below length
         :for c = (mem-aref foreign-array element-type k)
         :do (ecase lisp-type
               (:character
                 (setf (aref (the simple-string result) k) (code-char c)))
               ((:byte :integer)
                 (setf (aref result k) c))))
      (values result))))
  
(defun append-data (data target-value indicator column)
  (cond
    ((> indicator 0)
      (case (column-sql-type column)
        ((65532)
          (convert-array-to-lisp target-value :uint8 indicator :lisp-type :byte))
        (otherwise
          (map 'string #'code-char
               (convert-array-to-lisp target-value :uint8 indicator :lisp-type :integer)))))
    (t
      data)))

(defun sql-get-data (statement-handle column)
  (let* ((buffer-length 2048)
         ;; Convert all values to string for now.
         ;; TODO: Use proper types for target-type, target-value and convert-array-to-lisp 
         (target-value (foreign-alloc :uchar :count buffer-length))
         (target-type (cond
                        ((< (column-sql-type column) 32768)
                          SQL_C_CHAR)
                        ((= (column-sql-type column) 65532)
                          SQL_DEFAULT)
                        (t
                          SQL_C_CHAR)))
         (encoding *db-encoding*)
         (str-len (foreign-alloc :int)))
    (loop
       :with data = nil
       :for result = (sql-get-data% (handle statement-handle)
                                    (column-number column)
                                    target-type
                                    target-value
                                    buffer-length
                                    str-len)
       :for indicator = (mem-ref str-len :int)
       :while (eql result 1)
       :do (progn
             (warn "Reading partial data...")
             (setf data (append-data data target-value indicator column)))
       :finally (case result
                  (0
                    (return (append-data data target-value indicator column)))
                  (100
                    (return (append-data data target-value indicator column)))
                  (t (return
                       (error 
                        (format () "~{~a~^, ~}"
                                (multiple-value-list (sql-get-diag-rec statement-handle 1))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQLFetch

(defcfun (sql-fetch% "SQLFetch")
    :int16
  (StatementHandle :pointer))

(defun sql-fetch (statement-handle)
  (let ((result (sql-fetch% (handle statement-handle))))
    (ecase result
      (0
        t)
      (1
        (multiple-value-bind (code text)
            (sql-get-diag-rec statement-handle 1)
          (log2:warning "~a:~a" code text))
        t)
      (100
        nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQLGetDiagRec

(defcfun (sql-get-diag-rec% "SQLGetDiagRec")
    :int16 
  (handle-type :int16)
  (handle :pointer)
  (rec-number :int16)
  (sql-state :pointer :in)
  (native-error :pointer :in) 
  (message-text :pointer :in)
  (buffer-length :int16 :in)
  (text-length :pointer :in))

(defun sql-get-diag-rec (handle rec-number)
  (let* ((handle-type (handle-type handle))
         (handle (handle handle))
         (sql-state (foreign-alloc :char :count 6))
         (native-error (foreign-alloc :int))
         (buffer-length 1024)
         (message-text (foreign-alloc :char :count buffer-length))
         (text-length (foreign-alloc :int16)))
    (let ((result
            (sql-get-diag-rec% handle-type
                               handle
                               rec-number
                               sql-state
                               native-error
                               message-text
                               buffer-length
                               text-length)))
      (case result
        ((or 0 100)
          (values
           (foreign-string-to-lisp sql-state :count 6 :encoding :ascii)
           (foreign-string-to-lisp message-text :count (mem-ref text-length :int16) :encoding :ascii)
           (mem-ref native-error :int)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +string-buffer-max+ (1- (expt 2 14)))

(defun allocate-string-buffer (size)
  (let ((count (min size +string-buffer-max+)))
    (when (< count size)
      (foreign-alloc :char :count count))))

      
;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
