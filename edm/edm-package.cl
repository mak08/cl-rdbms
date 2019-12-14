;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author         Michael Kappert
;;; Copyright      (c) Michael Kappert 2011
;;; Created        2011-10-19 23:43:23 23:43:23
;;; Last Modified  <michael 2019-12-14 15:39:01>
;;; Description

(defpackage "EDM"
  (:use "COMMON-LISP"
        "SQL"
        "RDPARSE"
        ;; CLOS
        #+:clisp "CLOS"
        #+:sbcl "SB-MOP"
        #+:ccl "CCL")

  (:shadow "DATE")
  
  (:export

   ;; "Lifecycle"
   update-schema
   backup
   restore
   redeploy

   ;; "Relational Objects"
   create-transient-table
   append-tuple

   ;; Schemas
   use-schema

   ;; entity syntax
   *sql-readtable*

   ;; Entities
   defentity
   create-instance
   retrieve-instance
   delete-instance

   ;; Query
   retrieve

   ;; Entity mutators & readers
   set-element
   get-element
   reset-element
   add-element
   remove-element
   entity_id

   ;; Special Entity functions
   deleted

   ;; Helper functions
   find-entity
   get-entities
   ))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
