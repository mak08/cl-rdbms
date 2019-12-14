;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    Lifeycle functions
;;; Author         Michael Kappert 2013
;;; Last Modified  <michael 2019-12-10 20:25:15>


(in-package :datamodel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create database

(defun create-db (dbinfo superuser su-password)
  (%create-role (dbinfo-user dbinfo)
                :createrole t
                :password (dbinfo-password dbinfo)
                :role '("dbadmin"))
  (%create-database (dbinfo-dbname dbinfo)
                    (dbinfo-user dbinfo)))

(defmethod backup ((database symbol)
                   (userinfo userinfo)
                   (schema schema)
                   (directory string))
  (with-db (database userinfo) 
    (with-transaction (:isolation :serializable)
      (dolist (table (schema-tables schema))
        (%copy-table table :to directory)))))

(defmethod restore ((database symbol)
                    (userinfo userinfo)
                    (schema schema)
                    (directory string))
  (with-db (database userinfo) 
    (with-transaction (:isolation :serializable)
      (dolist (table (schema-tables schema))
        (%copy-table table :from directory)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Perform deployment actions
;;  - Delete old definitions
;;  - Create roles, tables and users
;;  - Create initial table contents

(defun redeploy (schema database user-name password &key (redeploy nil))
  (with-open-connection (c database :user user-name :password password)
    (with-connection (c)
      (with-transaction ()
        (update-schema% (get-schema-by-name schema) user-name :redeploy redeploy)))))

(defmethod update-schema ((database string)
                          (userinfo userinfo)
                          (schema schema)
                          &key (redeploy nil))
  (with-open-connection  (db database :user (userinfo-name userinfo) :password (userinfo-password userinfo))
    (with-connection (db)
      (with-transaction (:isolation :serializable)
        (update-schema% schema (userinfo-name userinfo) :redeploy redeploy)))))

(defun update-schema% (schema user-name &key (redeploy nil))
  (let* ((old-schema
          (load-db-schema (schema-name schema))))

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
          (%add-foreign-key tabdef constraint))))))

(defun create-schema (schema user-name)
  ;; At least try to clean up before attempting to create the schema
  (%drop-schema (schema-name schema) :if-does-not-exist :ignore :if-not-empty :force)

  ;; Create schema initially empty
  (%create-schema user-name :name (schema-name schema))

  ;; Create tables initially without constraints.
  (dolist (tabdef (schema-tables schema))
    (let ((new-tabdef (copy-tabdef tabdef)))
      (setf (tabdef-constraints new-tabdef) nil)
      (%create-table new-tabdef)))

  ;; Constraints cannot be altered. Create constraints only for new tables.
  ;; Create all primary keys
  (dolist (tabdef (schema-tables schema))
    (dolist (constraint (tabdef-constraints tabdef))
      (when (primary-key-p constraint)
        (%add-primary-key tabdef constraint))))
        
  ;; Create UNIQUE contraints
  (dolist (tabdef (schema-tables schema))
    (dolist (constraint (tabdef-constraints tabdef))
      (when (unique-key-p constraint)
        (%add-unique-key tabdef constraint))))

  ;; Create all foreign keys (refer to tables and their primary keys)
  (dolist (tabdef (schema-tables schema))
    (dolist (constraint (tabdef-constraints tabdef))
      (when (foreign-key-p constraint)
        (%add-foreign-key tabdef constraint)))))

(defun populate-schema (schema user-name)
  ;; Create tables initially without constraints.
  (dolist (tabdef (schema-tables schema))
    (let ((new-tabdef (copy-tabdef tabdef)))
      (setf (tabdef-constraints new-tabdef) nil)
      (%create-table new-tabdef)))

  ;; Constraints cannot be altered. Create constraints only for new tables.
  ;; Create all primary keys
  (dolist (tabdef (schema-tables schema))
    (dolist (constraint (tabdef-constraints tabdef))
      (when (primary-key-p constraint)
        (%add-primary-key tabdef constraint))))
        
  ;; Create UNIQUE contraints
  (dolist (tabdef (schema-tables schema))
    (dolist (constraint (tabdef-constraints tabdef))
      (when (unique-key-p constraint)
        (%add-unique-key tabdef constraint))))

  ;; Create all foreign keys (refer to tables and their primary keys)
  (dolist (tabdef (schema-tables schema))
    (dolist (constraint (tabdef-constraints tabdef))
      (when (foreign-key-p constraint)
        (%add-foreign-key tabdef constraint)))))

(defun wipe-schema (schema)
  (dolist (tabdef (schema-tables schema))
    (%drop-table tabdef :if-not-empty :force)))

(defun clear-schema% (schema)
  (dolist (tabdef (schema-tables schema))
    (?delete (tabdef-name tabdef))))

(defun find-tabdef-in-schema (tabdef schema)
  (find (tabdef-name tabdef) (schema-tables schema) :key #'tabdef-name :test #'string-equal))

(defun find-coldef-in-table (coldef tabdef)
  (find (coldef-name coldef) (tabdef-columns tabdef) :key #'coldef-name :test #'string-equal))

(defun find-constraint-in-table (constraint tabdef)
  (find (tabcon-name constraint) (tabdef-constraints tabdef) :key #'tabcon-name :test #'string-equal))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
