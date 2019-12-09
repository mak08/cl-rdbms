;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   sqlite3 API and CFFI bindings for libsqlite3
;;; Author        Michael Kappert 2019
;;; Last Modified <michael 2019-12-09 23:28:52>

(in-package "SQLITE-CLIENT")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic error/return codes
(defconstant +SQLITE_OK+           0  ) ;; Successful result */
(defconstant +SQLITE_ERROR+        1  ) ;; SQL error or missing database */
(defconstant +SQLITE_INTERNAL+     2  ) ;; Internal logic error in SQLite */
(defconstant +SQLITE_PERM+         3  ) ;; Access permission denied */
(defconstant +SQLITE_ABORT+        4  ) ;; Callback routine requested an abort */
(defconstant +SQLITE_BUSY+         5  ) ;; The database file is locked */
(defconstant +SQLITE_LOCKED+       6  ) ;; A table in the database is locked */
(defconstant +SQLITE_NOMEM+        7  ) ;; A malloc() failed */
(defconstant +SQLITE_READONLY+     8  ) ;; Attempt to write a readonly database */
(defconstant +SQLITE_INTERRUPT+    9  ) ;; Operation terminated by sqlite3_interrupt()*/
(defconstant +SQLITE_IOERR+       10  ) ;; Some kind of disk I/O error occurred */
(defconstant +SQLITE_CORRUPT+     11  ) ;; The database disk image is malformed */
(defconstant +SQLITE_NOTFOUND+    12  ) ;; Unknown opcode in sqlite3_file_control() */
(defconstant +SQLITE_FULL+        13  ) ;; Insertion failed because database is full */
(defconstant +SQLITE_CANTOPEN+    14  ) ;; Unable to open the database file */
(defconstant +SQLITE_PROTOCOL+    15  ) ;; Database lock protocol error */
(defconstant +SQLITE_EMPTY+       16  ) ;; Database is empty */
(defconstant +SQLITE_SCHEMA+      17  ) ;; The database schema changed */
(defconstant +SQLITE_TOOBIG+      18  ) ;; String or BLOB exceeds size limit */
(defconstant +SQLITE_CONSTRAINT+  19  ) ;; Abort due to constraint violation */
(defconstant +SQLITE_MISMATCH+    20  ) ;; Data type mismatch */
(defconstant +SQLITE_MISUSE+      21  ) ;; Library used incorrectly */
(defconstant +SQLITE_NOLFS+       22  ) ;; Uses OS features not supported on host */
(defconstant +SQLITE_AUTH+        23  ) ;; Authorization denied */
(defconstant +SQLITE_FORMAT+      24  ) ;; Auxiliary database format error */
(defconstant +SQLITE_RANGE+       25  ) ;; 2nd parameter to sqlite3_bind out of range */
(defconstant +SQLITE_NOTADB+      26  ) ;; File opened that is not a database file */
(defconstant +SQLITE_NOTICE+      27  ) ;; Notifications from sqlite3_log() */
(defconstant +SQLITE_WARNING+     28  ) ;; Warnings from sqlite3_log() */
(defconstant +SQLITE_ROW+         100 ) ;; sqlite3_step() has another row ready */
(defconstant +SQLITE_DONE+        101 ) ;; sqlite3_step() has finished executing */

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extended error codes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fundamental datatypes
(defconstant +SQLITE_INTEGER+  1)
(defconstant +SQLITE_FLOAT+    2)
(defconstant +SQLITE_BLOB+     4)
(defconstant +SQLITE_NULL+     5)
(defconstant +SQLITE_TEXT+     3)
(defconstant +SQLITE3_TEXT+    3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API functions

(defclass sqlite-connection (sql-connection)
  ((database :accessor database :initarg :database)
   (conn :accessor conn :initarg :conn)))

(defmacro with-open-connection ((connection database) &body forms)
  "Provides a new connection"
  `(let ((,connection
          (%connect% ,database))
         (values))
     (unwind-protect
          (setf values
                (multiple-value-list 
                 (progn ,@forms)))
       (%disconnect% ,connection))
     (values-list values)))

(defmethod sql:sql-exec ((conn sqlite-connection) (sql-statement string))
  (log2:info "~a" sql-statement)
  (sql-exec% conn sql-statement))

(defmethod sql:sql-query ((conn sqlite-connection) (sql-statement string))
  (log2:info "~a" sql-statement)
  (sql-exec% conn sql-statement))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation

(defun %connect% (database)
  (let ((db (sqlite3-open database)))
    (make-instance 'sqlite-connection
                   :database database
                   :conn db)))

(defun %disconnect% (connection) 
 (sqlite3-close (conn connection)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Executing SQL strings

(defun sql-exec% (conn sql-statement)
  (log2:trace "~a" sql-statement)
  (let* ((p-stmt (sqlite3-prepare-v2 (conn conn) sql-statement))
         (n-cols (sqlite3-column-count p-stmt))
         (col-names (loop
                       :for c :below n-cols
                       :collect (sqlite3-column-name p-stmt c))))
    (unwind-protect
         (values col-names
                 (loop
                    :for row = (sqlite3-step p-stmt)
                    :while (eql row +sqlite_row+)
                    :collect (loop
                                :for col :below n-cols
                                :collect (sqlite3-column-text p-stmt col))))
      (sqlite3-finalize p-stmt))))

(defun fetch% (into result &key field-mapper)
 )

(defmethod sql:fetch ((transient-table transient-table) (result t) &key (field-mapper #'default-field-mapper))
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQLite interface

(eval-when (:load-toplevel :compile-toplevel :execute)
  (define-foreign-library libsqlite3
    (:unix
     "libsqlite3.so.0")
    (t
     (:default "libsqlite3")))
  (define-foreign-library libuuid
    (:unix
     "libuuid.so.1.3.0")
    (t
     (:default "libuuid"))))

(use-foreign-library libsqlite3)
(use-foreign-library libuuid)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Performance limits

;;; Run-Time Limit Categories
(defconstant +SQLITE_LIMIT_LENGTH+                    0)
(defconstant +SQLITE_LIMIT_SQL_LENGTH+                1)
(defconstant +SQLITE_LIMIT_COLUMN+                    2)
(defconstant +SQLITE_LIMIT_EXPR_DEPTH+                3)
(defconstant +SQLITE_LIMIT_COMPOUND_SELECT+           4)
(defconstant +SQLITE_LIMIT_VDBE_OP+                   5)
(defconstant +SQLITE_LIMIT_FUNCTION_ARG+              6)
(defconstant +SQLITE_LIMIT_ATTACHED+                  7)
(defconstant +SQLITE_LIMIT_LIKE_PATTERN_LENGTH+       8)
(defconstant +SQLITE_LIMIT_VARIABLE_NUMBER+           9)
(defconstant +SQLITE_LIMIT_TRIGGER_DEPTH+            10)
(defconstant +SQLITE_LIMIT_WORKER_THREADS+           11)

;;; int sqlite3_limit(sqlite3*, int id, int newVal);
(defcfun ("sqlite3_limit" :library libsqlite3)
    :int
  (db :pointer)
  (id :int)
  (new-val :int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Database Connection

(defun sqlite3-open (filename)
  (let* ((db (foreign-alloc :pointer))
         (result (sqlite3_open filename db)))
    (case result
      (#.+SQLITE_OK+
       (mem-ref db :pointer))
      (otherwise
       (error "SQLite error ~a" (sqlite3-errmsg db))))))

(defcfun ("sqlite3_open" sqlite3_open :library libsqlite3)
    :int
  (filename :string)
  (database :pointer))

(defun sqlite3-close (db)
  (let ((result (sqlite3_close db)))
      (case result
        (#.+SQLITE_OK+
         t)
        (otherwise
         (error "SQLite error ~a" (sqlite3-errmsg db))))))
      
(defcfun ("sqlite3_close" sqlite3_close :library libsqlite3)
    :int
    (database :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Error handling and status

;;; int sqlite3_errcode(sqlite3 *db);
(defcfun ("sqlite3_errcode" :library libsqlite3)
    :int
  (db :pointer))
  
;;; int sqlite3_extended_errcode(sqlite3 *db);
(defcfun ("sqlite3_extended_errcode" :library libsqlite3)
    :int
  (db :pointer))

;;; const char *sqlite3_errmsg(sqlite3*);
(defcfun ("sqlite3_errmsg" :library libsqlite3)
    :string
  (db :pointer))

;;; const void *sqlite3_errmsg16(sqlite3*);
(defcfun ("sqlite3_errmsg16" :library libsqlite3)
    :void
  (db :pointer))

;;; const char *sqlite3_errstr(int);
(defcfun ("sqlite3_errstr" :library libsqlite3)
    :string
  (code :int))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prepared statements

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prepared Statements - create, advance, count rows

;;; There are many variants of sqlite3_prepare. sqlite3_prepare_v2 is the preferred one.
;;; int sqlite3_prepare_v2(
;;;   sqlite3 *db,            /* Database handle */
;;;   const char *zSql,       /* SQL statement, UTF-8 encoded */
;;;   int nByte,              /* Maximum length of zSql in bytes. */
;;;   sqlite3_stmt **ppStmt,  /* OUT: Statement handle */
;;;   const char **pzTail     /* OUT: Pointer to unused portion of zSql */
;;; );
(defcfun ("sqlite3_prepare_v2" sqlite3_prepare_v2 :library libsqlite3)
    :int
  (db :pointer)
  (z-sql :string)
  (n-byte :int)
  (p-stmt :pointer)
  (z-tail :pointer))

(defun sqlite3-prepare-v2 (db statement)
  (with-foreign-object
      (z-tail :pointer)
    (let* ((stmt (foreign-alloc :pointer))
           (result
            (sqlite3_prepare_v2 db statement (1+ (length statement)) stmt z-tail)))
      (case result
        (#.+SQLITE_OK+
         (mem-ref stmt :pointer))
        (otherwise
         (error "SQLite error ~a" (sqlite3-errmsg db)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reset prepared statement to start
;;; int sqlite3_reset(sqlite3_stmt *pStmt);
(defcfun ("sqlite3_reset" :library libsqlite3)
    :int
  (stmt :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clear statement bindings
;;; int sqlite3_clear_bindings(sqlite3_stmt*);
(defcfun ("sqlite3_clear_bindings" :library libsqlite3)
    :int
  (stmt :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dispose prepared statement. REQUIRED before db can be closed!
;;; int sqlite3_finalize(sqlite3_stmt *pStmt);
(defcfun ("sqlite3_finalize" :library libsqlite3)
    :int
  (stmt :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; int sqlite3_step(sqlite3_stmt*);
(defcfun ("sqlite3_step" :library libsqlite3)
    :int
  (stmt :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Number of columns in a result set. Only valid after sqlite3_step().
;;; int sqlite3_data_count(sqlite3_stmt *pStmt);
(defcfun ("sqlite3_data_count" :library libsqlite3)
    :int
  (stmt :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NUmber of columns in a result set.
;;; int sqlite3_column_count(sqlite3_stmt *pStmt);
(defcfun ("sqlite3_column_count" :library libsqlite3)
    :int
  (stmt :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prepared statement - column access

;;; const void *sqlite3_column_blob(sqlite3_stmt*, int iCol);
(defcfun ("sqlite3_column_blob" :library libsqlite3)
    :void
  (sqlite3_stmt :pointer)
  (i-col :int))

;;; double sqlite3_column_double(sqlite3_stmt*, int iCol);
(defcfun ("sqlite3_column_double" :library libsqlite3)
    :double
  (sqlite3_stmt :pointer)
  (i-col :int))

;;; int sqlite3_column_int(sqlite3_stmt*, int iCol);
(defcfun ("sqlite3_column_int" :library libsqlite3)
    :int
  (stmt :pointer)
  (i-col :int))

;;; sqlite3_int64 sqlite3_column_int64(sqlite3_stmt*, int iCol);
(defcfun ("sqlite3_column_int64" :library libsqlite3)
    :int64
  (stmt :pointer)
  (i-col :int))

;;; const unsigned char *sqlite3_column_text(sqlite3_stmt*, int iCol);
(defcfun ("sqlite3_column_text" :library libsqlite3)
    :string
  (stmt :pointer)
  (i-col :int))

;;; const void *sqlite3_column_text16(sqlite3_stmt*, int iCol);
(defcfun ("sqlite3_column_text16" :library libsqlite3)
    :pointer
  (stmt :pointer)
  (i-col :int))

;;; sqlite3_value *sqlite3_column_value(sqlite3_stmt*, int iCol);
(defcfun ("sqlite3_column_value" :library libsqlite3)
    :pointer
  (stmt :pointer)
  (i-col :int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Prepared statement - column type and size information

;;; Size of BLOB and UTF-8
;;; int sqlite3_column_bytes(sqlite3_stmt*, int iCol);
(defcfun ("sqlite3_column_bytes" :library libsqlite3)
    :int
  (stmt :pointer)
  (i-col :int))

;;; Size of UTF-16 test in bytes
;;; int sqlite3_column_bytes16(sqlite3_stmt*, int iCol);
(defcfun ("sqlite3_column_byte16" :library libsqlite3)
    :int
  (stmt :pointer)
  (i-col :int))

;;; int sqlite3_column_type(sqlite3_stmt*, int iCol);
(defcfun ("sqlite3_column_type" :library libsqlite3)
    :int
  (stmt :pointer)
  (i-col :int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prepared statement - Source of data in a query result

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Return the AS alias name for a column in the result.
;;; const char *sqlite3_column_name(sqlite3_stmt*, int N);
(defcfun ("sqlite3_column_name" :library libsqlite3)
    :string
  (stmt :pointer)
  (i-col :int))

;;; const void *sqlite3_column_name16(sqlite3_stmt*, int N);


;;; const char *sqlite3_column_database_name(sqlite3_stmt*,int);
(defcfun ("sqlite3_column_database_name" :library libsqlite3)
    :string
  (stmt :pointer)
  (i-col :int))

;;; const void *sqlite3_column_database_name16(sqlite3_stmt*,int);
(defcfun ("sqlite3_column_database_name16" :library libsqlite3)
    :pointer
  (stmt :pointer)
  (i-col :int))

;;; const char *sqlite3_column_table_name(sqlite3_stmt*,int);
(defcfun ("sqlite3_column_table_name" :library libsqlite3)
    :string
  (stmt :pointer)
  (i-col :int))

;;; const void *sqlite3_column_table_name16(sqlite3_stmt*,int);
(defcfun ("sqlite3_column_table_name16" :library libsqlite3)
    :pointer
  (stmt :pointer)
  (i-col :int))

;;; const char *sqlite3_column_origin_name(sqlite3_stmt*,int);
(defcfun ("sqlite3_column_origin_name" :library libsqlite3)
    :string
  (stmt :pointer)
  (i-col :int))

;;; const void *sqlite3_column_origin_name16(sqlite3_stmt*,int);
(defcfun ("sqlite3_column_origin_name16" :library libsqlite3)
    :pointer
  (stmt :pointer)
  (i-col :int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UUIDs

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