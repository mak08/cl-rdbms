;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Copyright      (c)  2014
;;; Last Modified  <michael 2019-12-14 20:25:06>

(defpackage "PG-SQL"
  (:use "COMMON-LISP" "SQL" "EDM")
  (:shadowing-import-from "SQL" "DATE")
  (:export "LOAD-DB-SCHEMA"
           "POSTGRES-CONNECTION"

           dbinfo
           make-dbinfo
           dbinfo-name
           dbinfo-owner
           
           userinfo
           make-userinfo
           userinfo-name
           userinfo-password))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

