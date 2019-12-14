;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2019-12-14 11:00:14>

(in-package :datamodel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; DB Schema
;;
;; A schema is a collection of db table definitions, not a namespace.
;; The schema is used in two situations:
;; - When building the application, the types derived from the schema must
;;   be created/imported.
;; - When deploying the application, the corresponding DB tables must be
;;   created

(defvar *schema-lib* (make-hash-table :test #'equalp))

(defmacro defschema (name &rest definitions)
  (let ((owner)
        (tabdefs))
    (loop :for definition :in definitions
       :do (ecase (car definition)
             (:owner
               (setf owner (cadr definition)))
             (:table
               (push `(deftable ,@(cdr definition) :schema ,name)
                     tabdefs))))
    `(setf (gethash ,name *schema-lib*)
                    (make-schema :name ,name
                                 :owner ,owner
                                 :tables (reverse (list ,@tabdefs))))))

(defun get-schema-by-name (name)
  (gethash name *schema-lib*))

(defmethod use-schema ((name string))
  (use-schema (get-schema-by-name name)))
(defmethod use-schema ((schema schema))
  (loop
     :for tabdef :in (schema-tables schema)
     :do (ensure-tuple-class tabdef))
  (values schema))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper for defschema

(defmacro deftable (name &key schema columns constraints)
  (let ((coldefs
          (loop :for coldef :in columns
             :collect `(make-coldef :name ,@coldef)))
        (constraints
          (loop :for constraint :in constraints :collect
             (destructuring-bind (&key primary-key foreign-key unique-key
                                       columns referenced-table referenced-columns)
                 constraint
               (cond (primary-key
                      `(make-primary-key :name ,primary-key
                                         :columns ',columns))
                     (foreign-key
                      `(make-foreign-key :name ,foreign-key
                                         :schema ,schema
                                         :columns ',columns
                                         :referenced-table-schema ,schema
                                         :referenced-table ',referenced-table
                                         :referenced-columns ',referenced-columns))
                     (unique-key
                      `(make-unique-key :name ,unique-key
                                        :columns ',columns)))))))
    `(create-tabdef :name ,name
                    :schema ,schema
                    :columns (list ,@coldefs)
                    :constraints (list ,@constraints))))


;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
