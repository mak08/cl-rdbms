;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    High-level SQL API
;;; Author         Michael Kappert 2013
;;; Last Modified  <michael 2020-01-16 16:25:58>

(in-package :sql)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The SQL API provides functions for defining and managing databases, roles,
;;; schemas and tables.
;;; Embedded SQL functions are defined in sql-ddl.cl, sql-dml.cl and sql-tcl.cl.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Table

(defmacro deftable (name &key schema columns constraints)
  (let ((coldefs
          (loop :for coldef :in columns
             :collect `(make-coldef :name ,@coldef)))
        (constraints
          (loop :for constraint :in constraints :collect
             (destructuring-bind (&key primary-key foreign-key unique-key
                                       columns referenced-table referenced-columns
                                       (on-update :restrict) (on-delete :cascade))
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
                                         :referenced-columns ',referenced-columns
                                         :on-update ,on-update
                                         :on-delete ,on-delete))
                     (unique-key
                      `(make-unique-key :name ,unique-key
                                        :columns ',columns)))))))
    `(create-tabdef :name ,name
                    :schema ,schema
                    :columns (list ,@coldefs)
                    :constraints (list ,@constraints))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Schema

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

(defun schema-table (schema-name table-name)
  (find table-name
        (schema-tables (get-schema-by-name schema-name))
        :key #'tabdef-name
        :test #'string=))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; create schema

(defun create-schema (schema)
    ;; At least try to clean up before attempting to create the schema
    
    ;; not implemented in SQLite
    ;; (%drop-schema (schema-name schema) :if-does-not-exist :ignore :if-not-empty :force)
    
    ;; Create schema initially empty
    (%create-schema (schema-name schema) :if-exists :ignore)
    
    ;; Create tables
    (with-transaction ()

      (dolist (tabdef (schema-tables schema))
        (%create-table tabdef))))


(defun wipe-schema (schema)
  (dolist (tabdef (schema-tables schema))
    (%drop-table tabdef :if-not-empty :force)))

(defun clear-schema (schema)
  (with-transaction ()
    (dolist (tabdef (schema-tables schema))
      (?delete (tabdef-name tabdef)))))
  

(defun update-schema (schema user-name &key (redeploy nil))
  (with-transaction ()
    (let* ((old-schema
            (load-schema *current-connection* (schema-name schema))))
      
      (cond
        ((null old-schema)
         ;; Create schema initially empty
         (%create-schema user-name :name (schema-name schema))
         (setf old-schema (make-schema)))
        ((and old-schema redeploy)
         (log2:info "Dropping all tables because REDEPLOY was requested")
         (dolist (tabdef (reverse (schema-tables old-schema)))
           (%drop-table tabdef :if-does-not-exist :ignore :if-not-empty :force))
         (setf old-schema (make-schema))))
        
      ;; Drop tables present in the old schema but not in the new
      (dolist (tabdef (schema-tables old-schema))
        (unless (find-tabdef-in-schema tabdef schema)
          (log2:info "Dropping table ~a because it was not found in the new schema" (tabdef-name tabdef))
          (%drop-table tabdef :if-not-empty :force)))

      ;; Drop foreign keys
      (log2:info "Dropping foreign keys...")
      (dolist (tabdef (schema-tables old-schema))
        (dolist (constraint (tabdef-constraints tabdef))
          (when (foreign-key-p constraint)
            (%drop-key tabdef constraint))))
    
      ;; Create present in the new schema but not in the old.
      ;; Create tables initially without constraints.
      (dolist (tabdef (schema-tables schema))
        (let ((old-tabdef (find-tabdef-in-schema tabdef old-schema)))
          (if old-tabdef
              (progn
                (dolist (coldef (tabdef-columns tabdef))
                  (unless (find-coldef-in-table coldef old-tabdef)
                    (log2:info "Adding new column ~a in table ~a" (coldef-name coldef) (tabdef-name tabdef))
                    (%add-column tabdef coldef)))
                (dolist (coldef (tabdef-columns old-tabdef))
                  (unless (find-coldef-in-table coldef tabdef)
                    (log2:info "Dropping old column ~a in table ~a" (coldef-name coldef) (tabdef-name tabdef))
                    (%drop-column tabdef coldef ))))
              (let ((new-tabdef (copy-tabdef tabdef)))
                (setf (tabdef-constraints new-tabdef) nil)
                (%create-table new-tabdef)))))

      ;; Constraints cannot be altered. Create constraints only for new tables.
      ;; Create all primary keys
      (dolist (tabdef (schema-tables schema))
        (unless (find-tabdef-in-schema tabdef old-schema)
          (dolist (constraint (tabdef-constraints tabdef))
            (when (primary-key-p constraint)
              (%add-primary-key tabdef constraint)))))
        
      ;; Create UNIQUE contraints
      (dolist (tabdef (schema-tables schema))
        (let ((old-tabdef (find-tabdef-in-schema tabdef old-schema)))
          (when old-tabdef
            (dolist (constraint (tabdef-constraints old-tabdef))
              (when (unique-key-p constraint)
                (log2:info "Dropping existing UNIQUE key ~a" (tabcon-name constraint))
                (%drop-key tabdef constraint))))
          (dolist (constraint (tabdef-constraints tabdef))
            (when (unique-key-p constraint)
              (%add-unique-key tabdef constraint)))))
    
      ;; Create all foreign keys (refer to tables and their primary keys)
      (log2:info "Adding foreign keys...")
      (dolist (tabdef (schema-tables schema))
        (dolist (constraint (tabdef-constraints tabdef))
          (when (foreign-key-p constraint)
            (%add-foreign-key tabdef constraint)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Database 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions



(defun find-tabdef-in-schema (tabdef schema)
  (find (tabdef-name tabdef) (schema-tables schema) :key #'tabdef-name :test #'string-equal))

(defun find-coldef-in-table (coldef tabdef)
  (find (coldef-name coldef) (tabdef-columns tabdef) :key #'coldef-name :test #'string-equal))

(defun find-constraint-in-table (constraint tabdef)
  (find (tabcon-name constraint) (tabdef-constraints tabdef) :key #'tabcon-name :test #'string-equal))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
