;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author         Michael Kappert
;;; Copyright      (c) Michael Kappert 2011
;;; Created        2011-10-19 23:43:23 23:43:23
;;; Last Modified  <michael 2018-01-14 20:34:45>
;;; Description

(defpackage "DATAMODEL"
  (:use "COMMON-LISP"
        "SQL"
        "RDPARSE"
        ;; CLOS
        #+:clisp "CLOS"
        #+:sbcl "SB-MOP"
        #+:ccl "CCL"
        ;; libpq 
        #+:unix "PG-CLIENT"
        ;; GTK UI 
        #+:gir "GIR"
        #+:gtk+ "GTK+")

  (:shadow "DATE")
  
  (:export

   ;; Lifecycle
   update-schema
   backup
   restore
   redeploy

   ;; ?
   with-open-connection
   with-connection

   ;; "Relational Objects"
   create-transient-table
   append-tuple

   ;; Schemas
   *schema*
   create-db-schema

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
