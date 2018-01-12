;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael 2014
;;; Last Modified <d037165 2014-07-18 14:10:29>

;;; Performace observations
;;; - SLEEPing before LISTENING brings down CPU usage but does not speed up running time
;;; - write/read-unsigned-byte, write/read-utf-8-string are not slow
    
(defmacro with-connection ((conn-var &key user password database) &body forms)
  `(let ((,conn-var (usocket:socket-connect "localhost" 5432 :element-type '(unsigned-byte 8)  :nodelay t)))
     (logging:log-info "Opened ~a" ,conn-var)
     (unwind-protect
       (progn
         (logon ,conn-var :user ,user :password ,password :database ,database)
         ,@forms)
       (logging:log-info "Closing ~a" ,conn-var)
       (socket-close ,conn-var))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Queries (SELECT statements)

(defun exec-query (conn sql-statement &key (row-parser #'string-row-parser))
  (let ((message (make-query-message :type (char-code #\Q)
                                     :length (+ (utf-8-string-byte-length sql-statement) 4 1)
                                     :content sql-statement)))
    (write-query-message (socket-stream conn) message)
    (force-output (socket-stream conn))
    (wait-for-input conn :timeout 1 :ready-only t)
    (let* ((status nil)
           (row-description nil)
           (rows (cons nil nil))
           (lastrow  rows)
           (error nil)
           (ready nil))
      (loop
         :while (listen (socket-stream conn))
         :for message = (read-backend-message (socket-stream conn))
         :do (etypecase message
               (data-row
                 (rplacd lastrow (list
                                  (funcall row-parser row-description message)))
                 (setf lastrow (cdr lastrow)))
               (row-description
                 (assert (null row-description))
                 (setf row-description message))
               (command-complete
                 (assert (null status))
                 (setf status message))
               (error-message
                 (assert (null error))
                 (setf error message))
               (ready-for-query
                 (setf ready message))))
      (cond
        (error
          (error (format-error-message error)))
        (t
          (values
           (map 'list #'field-description-name (row-description-fields row-description))
           (cdr rows)
           status
           ready))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other SQL Commands

(defun exec-sql (conn sql-statement)
  (let ((message (make-query-message :type (char-code #\Q)
                                     :length (+ (utf-8-string-byte-length sql-statement) 4 1)
                                     :content sql-statement)))
    (write-query-message (socket-stream conn) message)
    (force-output (socket-stream conn))
    (loop :until (listen (socket-stream conn)))
    (loop
       :while (listen (socket-stream conn))
       :collect (read-backend-message (socket-stream conn)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Row Parsers

(defun string-row-parser (row-description row)
  (declare (ignore row-description))
  (loop
     :for fields :on (data-row-fields row)
     :do (rplaca fields 
                 (when (>= (field-length (car fields)) 0)
                   (map 'string #'code-char (field-value (car fields))))))
  (data-row-fields row))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun logon (socket &key user password database)
  (let ((m (create-startup-message (list "user" user "database" database)))
        (p (create-password-message password)))

    (write-startup-message (socket-stream socket) m)
    (force-output (socket-stream socket))
    (read-backend-message (socket-stream socket))
    
    (write-password-message (socket-stream socket) p)
    (force-output (socket-stream socket))
    (read-backend-message (socket-stream socket))

    (loop :until (listen (socket-stream socket)))
    (loop
       :while (listen (socket-stream socket))
       :collect (read-backend-message (socket-stream socket)))))


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

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
