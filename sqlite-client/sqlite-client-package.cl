;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2021-12-28 18:29:14>

(defpackage :sqlite-client
  (:use "COMMON-LISP" "CFFI" "SQL")
  (:export "WITH-OPEN-CONNECTION")
  (:export "WITH-CURRENT-CONNECTION"

           "%CONNECT%"

           "SQLITE3-LIBVERSION"

           "+SQLITE_LIMIT_LENGTH+"
           "+SQLITE_LIMIT_SQL_LENGTH+"
           "+SQLITE_LIMIT_COLUMN+"
           "+SQLITE_LIMIT_EXPR_DEPTH+"
           "+SQLITE_LIMIT_COMPOUND_SELECT+"
           "+SQLITE_LIMIT_VDBE_OP+"
           "+SQLITE_LIMIT_FUNCTION_ARG+"
           "+SQLITE_LIMIT_ATTACHED+"
           "+SQLITE_LIMIT_LIKE_PATTERN_LENGTH+"
           "+SQLITE_LIMIT_VARIABLE_NUMBER+"
           "+SQLITE_LIMIT_TRIGGER_DEPTH+"
           "+SQLITE_LIMIT_WORKER_THREADS+"

           "SQLITE3-LIMIT"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
