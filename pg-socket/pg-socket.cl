;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Copyright      (c)  2014
;;; Last Modified  <michael 2018-01-12 01:00:48>

(in-package :pg-socket)

(declaim (optimize (debug 3)))
;; (declaim (optimize (speed 3) (safety 1) (space 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API functions

;;; Performance observations
;;; - SLEEPing before LISTENING brings down CPU usage but does not speed up running time
;;; - write/read-unsigned-byte, write/read-utf-8-string are not slow

(defclass pg-socket-connection (postgres-connection)
  ((connection :reader connection :initarg :socket)))

(defmacro with-open-connection ((conn-var database &key (user "postgres") (password user)) &body forms)
  `(let ((,conn-var (make-instance 'pg-socket-connection
                                   :socket (usocket:socket-connect "localhost" 5432
                                                                   :element-type '(unsigned-byte 8)
                                                                   :nodelay t))))
     (log2:info "Opened ~a" ,conn-var)
     (unwind-protect
       (progn
         (logon (connection ,conn-var) :user ,user :password ,password :database ,database)
         (log2:info "Logged on to ~a as ~a" ,database ,user)
         ,@forms)
       (log2:info "Closing ~a" ,conn-var)
       (socket-close (connection ,conn-var)))))

(defmethod sql:sql-query ((conn pg-socket-connection) (sql-statement string))
  (exec-query (connection conn) sql-statement))

(defmethod sql:sql-exec ((conn pg-socket-connection) (sql-statement string))
  (exec-sql (connection conn) sql-statement))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Queries (SELECT statements)

(defun exec-query (conn sql-statement &key (row-parser #'string-row-parser))
  (log2:info "~a" sql-statement)
  (let ((message (create-query-message sql-statement)))
    (write-query-message (socket-stream conn) message)
    (force-output (socket-stream conn))
    ;; (wait-for-input (socket-stream conn) :timeout 1000)
    (loop :until (listen (socket-stream conn)) :do (sleep 0.000001))
    (let* ((status nil)
           (row-description nil)
           (rows (cons nil nil))
           (lastrow  rows)
           (error nil)
           (ready nil))

      (loop
         :with stream = (socket-stream conn)
         :while (listen stream)
         :do (let ((type (read-unsigned-byte stream :length 1))
                   (length (read-unsigned-byte stream :length 4)))
               (case (code-char type)
                 (#\C
                  (assert (null status))
                  (setf status
                        (read-command-complete stream)))
                 (#\D
                  (rplacd lastrow
                          (list
                           (read-data-row% stream)))
                  (setf lastrow (cdr lastrow)))
                 (#\E
                  (assert (null error))
                  (setf error
                        (read-error-message stream length)))
                 (#\T
                  (assert (null row-description))
                  (setf row-description
                        (read-row-description stream length)))
                 (#\Z
                  (setf ready
                        (read-ready-for-query stream length))))))
      (cond
        (error
          (error (format-error-message error)))
        (t
          (values
           (map 'list #'field-description-name (row-description-fields row-description))
           (cdr rows)
           status
           ready))))))

(defun read-data-row% (stream)
  (let ((num-fields
         (read-ub2 stream :byte-order :be)))
    (loop
       :for k :below num-fields
       :for length = (the (unsigned-byte 32) (read-ub4 stream))
       :collect (unless
                    (= length 4294967295)
                  (read-utf-8-array stream :byte-length length)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other SQL Commands

(defun exec-sql (conn sql-statement)
  (log2:info "~a" sql-statement)
  (let ((message (create-query-message sql-statement)))
    (write-query-message (socket-stream conn) message)
    (force-output (socket-stream conn))
    ;; (wait-for-input (socket-stream conn) :timeout 1000)
    (sleep 0.000001)
    (loop :until (listen (socket-stream conn)) :do (sleep 0.000001))
    (let* ((status nil)
           (error nil)
           (ready nil))
      (loop
         :while (listen (socket-stream conn))
         :for message = (read-backend-message (socket-stream conn))
         :do (etypecase message
               (command-complete
                 (push message status))
               (error-message
                 (assert (null error))
                 (setf error message))
               (ready-for-query
                 (setf ready message))
               (notice-message
                 (log2:warning "~a" (format-notice-message message)))))
      (cond
        (error
          (error (format-error-message error)))
        (t
          (values status ready))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Row Parsers

(defun string-row-parser (row-description row)
  (declare (ignore row-description))
  (loop
     :for fields :on (data-row-fields row)
     :do (rplaca fields (field-value (car fields))))
  (data-row-fields row))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Error messages
;;;   S  Severity: the field contents are ERROR, FATAL, or PANIC (in an error message), or WARNING, NOTICE, DEBUG, INFO, or LOG (in a notice message), or a localized translation of one of these. Always present.
;;;   C  Code: the SQLSTATE code for the error (see Appendix A). Not localizable. Always present.
;;;   M  Message: the primary human-readable error message. This should be accurate but terse (typically one line). Always present.
;;;   D  Detail: an optional secondary error message carrying more detail about the problem. Might run to multiple lines.
;;;   H  Hint: an optional suggestion what to do about the problem. This is intended to differ from Detail in that it offers advice (potentially inappropriate) rather than hard facts. Might run to multiple lines.
;;;   P  Position: the field value is a decimal ASCII integer, indicating an error cursor position as an index into the original query string. The first character has index 1, and positions are measured in characters not bytes.
;;;   p  Internal position: this is defined the same as the P field, but it is used when the cursor position refers to an internally generated command rather than the one submitted by the client. The q field will always appear when this field appears.
;;;   q  Internal query: the text of a failed internally-generated command. This could be, for example, a SQL query issued by a PL/pgSQL function.
;;;   W  Where: an indication of the context in which the error occurred. Presently this includes a call stack traceback of active procedural language functions and internally-generated queries. The trace is one entry per line, most recent first.
;;;   s  Schema name: if the error was associated with a specific database object, the name of the schema containing that object, if any.
;;;   t  Table name: if the error was associated with a specific table, the name of the table. (Refer to the schema name field for the name of the table's schema.)
;;;   c  Column name: if the error was associated with a specific table column, the name of the column. (Refer to the schema and table name fields to identify the table.)
;;;   d  Data type name: if the error was associated with a specific data type, the name of the data type. (Refer to the schema name field for the name of the data type's schema.)
;;;   n  Constraint name: if the error was associated with a specific constraint, the name of the constraint. Refer to fields listed above for the associated table or domain. (For this purpose, indexes are treated as constraints, even if they weren't created with constraint syntax.)
;;;   F  File: the file name of the source-code location where the error was reported.
;;;   L  Line: the line number of the source-code location where the error was reported.
;;;   R  Routine: the name of the source-code routine reporting the error.
(defun format-error-message (e)
  (flet ((find-tag (tag)
           (let ((f (find (char-code tag) (error-message-content e) :key #'tagged-field-tag)))
             (if f
               (tagged-field-value f)
               ""))))
  (let ((severity (find-tag #\S))
        (code (find-tag #\C))
        (message (find-tag #\M)))
    (format () "~a: ~a (~a)" severity code message))))
(defun format-notice-message (e)
  (flet ((find-tag (tag)
           (let ((f (find (char-code tag) (notice-message-content e) :key #'tagged-field-tag)))
             (if f
               (tagged-field-value f)
               ""))))
  (let ((severity (find-tag #\S))
        (code (find-tag #\C))
        (message (find-tag #\M)))
    (format () "~a: ~a (~a)" severity code message))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers

(defun logon (socket &key user password database)
  (let ((m (create-startup-message (list "user" user "database" database)))
        (p (create-password-message password)))

    (write-startup-message (socket-stream socket) m)
    (force-output (socket-stream socket))

    (let  ((reply (read-backend-message (socket-stream socket))))
      (typecase reply
        (authentication-request

         (case (authentication-request-auth-type reply)
           (3
            (log2:info "Using plaintext password authentication")
            (write-password-message (socket-stream socket) p)
            (force-output (socket-stream socket))
            (let  ((reply (read-backend-message (socket-stream socket))))
              (typecase reply
                (authentication-request
                 )
                (error-message
                 (error
                  (format-error-message reply)))
                (t
                 (error "Unexpected server message ~a" reply))))
            
            (loop :until (listen (socket-stream socket)))
            (loop
               :while (listen (socket-stream socket))
               :collect (read-backend-message (socket-stream socket))))
           (5
            (error "MD5 passwords not supported"))))
        (t
         (error "Unexpected server message ~a" reply))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PostgreSQL Frontend/Backend Protocol implementation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Frontend Messages

(define-out-message startup-message (parameters)
  (length (:unsigned-byte :length 4)
          (+ (reduce #'+ (mapcar #'utf-8-string-byte-length parameters))
             8 ; 4-byte version and length
             (length parameters) ; string terminat NUL bytes 
             1 ; sequence terminating NUL byte
             ))
  (version (:unsigned-byte :length 4)
           #x00030000)
  (parameters (:sequence (:utf-8-string))
              parameters))

(define-out-message password-message (password)
  (type (:unsigned-byte :length 1)
        (char-code #\p))
  (length (:unsigned-byte :length 4)
          (+ (utf-8-string-byte-length password) 4 1))
  (content (:utf-8-string)
           password))

(define-out-message query-message (content)
  (type (:unsigned-byte :length 1)
        (char-code #\Q))
  (length (:unsigned-byte :length 4)
          (+ (utf-8-string-byte-length content) 4 1))
  (content (:utf-8-string)
           content))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Backend Messages

;;; The following definition could be compressed using
;;; an additional macro DEFINE-VARIANT-TYPE. The macro should create two functions,
;;; READ-BACKEND-MESSAGE and WRITE-BACKEND-MESSAGE, but no DEFSTRUCT.

(defun read-backend-message (stream)
  (let ((type (read-unsigned-byte stream :length 1))
        (length (read-unsigned-byte stream :length 4)))
    (case (code-char type)
      (#\C
        (read-command-complete stream))
      (#\D
        (read-data-row stream))
      (#\E
        (read-error-message stream length))
      (#\K
        (read-backend-key-data stream length))
      (#\N
        (read-notice-message stream length))
      (#\R
        (read-authentication-request stream length))
      (#\S
        (read-parameter-status stream length))
      (#\T
        (read-row-description stream length))
      (#\Z
        (read-ready-for-query stream length))
      (t
        (log2:warning "READ-BACKEND-MESSAGE: Unknown tag ~c" (code-char type))
        (read-byte-array stream :length (- length 4))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The actual backend messages

(define-binary-type command-complete ()
  (content (:utf-8-string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-binary-type error-message (length)
  (content (:sequence tagged-field :byte-length (- length 8)))
  (terminator (:unsigned-byte :length 1)))

(define-binary-type notice-message (length)
  (content (:sequence tagged-field :byte-length (- length 8)))
  (terminator (:unsigned-byte :length 1)))

(define-binary-type tagged-field ()
  (tag (:unsigned-byte :length 1))
  (value (:utf-8-string)))

(define-binary-type backend-key-data (length)
  (process-id (:unsigned-byte :length 4 :byte-order :be))
  (secret-key (:unsigned-byte :length 4 :byte-order :be)))
  
(define-binary-type authentication-request (length)
  (auth-type (:unsigned-byte :length 4 :byte-order :be))
  (salt (:case auth-type
          (5 (:sequence (:unsigned-byte :length 1) :length 4)))))

(define-binary-type row-description (length)
  (num-fields (:unsigned-byte :length 2 :byte-order :be))
  (fields (:sequence field-description :length num-fields)))

(define-binary-type field-description ()
  (name (:utf-8-string))
  (table-oid (:unsigned-byte :length 4 :byte-order :be))
  (attnum (:unsigned-byte :length 2 :byte-order :be))
  (datatype-oid (:unsigned-byte :length 4 :byte-order :be))
  (datatype-size (:unsigned-byte :length 2 :byte-order :be))
  (datatype-modifier (:unsigned-byte :length 4 :byte-order :be))
  (format-code (:unsigned-byte :length 2 :byte-order :be)))
  
(define-binary-type parameter-status (length)
  (parameter (:utf-8-string))
  (value (:utf-8-string)))

(define-binary-type ready-for-query (length)
  (status (:unsigned-byte :length 1) (setf status (code-char status))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
