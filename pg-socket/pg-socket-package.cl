;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Copyright      (c)  2014
;;; Last Modified  <michael 2018-01-15 21:11:14>

(defpackage "PG-SOCKET"
  (:use "COMMON-LISP" "USOCKET" "SQL" "PG-SQL")
  (:shadow "DATE")
  (:export "WITH-OPEN-CONNECTION"))

;; (pg-socket:with-open-connection (c "crm" :user "crmadmin" :password "crmadmin")
;;   c)

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

