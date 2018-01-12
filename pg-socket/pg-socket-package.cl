;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Copyright      (c)  2014
;;; Last Modified  <michael 2018-01-12 00:56:50>

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "USOCKET")
    (ql:quickload 'usocket)))

(defpackage "PG-SOCKET"
  (:use "COMMON-LISP" "USOCKET" "SQL" "PG-SQL")
  (:shadow "DATE")
  (:export "WITH-OPEN-CONNECTION"))

;; (pg-socket:with-open-connection (c "crm" :user "crmadmin" :password "crmadmin")
;;   c)

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

