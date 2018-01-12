;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Copyright      (c) michael 2012
;;; License
;;; Last Modified  <michael 2018-01-12 00:56:11>

(in-package :pg-sql)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PostgreSQL specific definitions
 
(defvar *table_name_length* 63)
 
(defclass postgres-connection (sql-connection)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The information_schema tables are actually ANSI SQL.
;; Move this from postgres.cl to dbl.cl ?
 
(defschema "information_schema"
  (:table "schemata"
          :schema "information_schema"
          :columns (("schema_name" :datatype "sql_identifier")
                    ("schema_owner" :datatype "sql_identifier")))
  (:table "tables"
          :schema "information_schema"
          :columns (("table_schema" :datatype "sql_identifier")
                    ("table_name" :datatype "sql_identifier")
                    ("table_type" :datatype "sql_identifier")))
  (:table "columns"
          :schema "information_schema"
          :columns (("table_schema" :datatype "sql_identifier")
                    ("table_name" :datatype "sql_identifier")
                    ("column_name" :datatype "sql_identifier")
                    ("data_type" :datatype "character_data"))))
 
(defschema "pg"
  (:table "constraints"
          :schema "pg"
          :columns (("key_name")
                    ("key_type")
                    ("key_updtype")
                    ("key_deltype")
                    ("key_tableid")
                    ("key_columns")
                    ("key_reftable")
                    ("key_reftableid")
                    ("key_refcolumns"))))
 
(use-schema "information_schema")
(use-schema "pg")
 
(defun load-db-schema (schema-name)
  (setf schema-name (string-downcase schema-name))
  (when (find-db-schema schema-name)
    (make-schema
     :name schema-name
     :tables (map 'list
                  (lambda (table)
                    (make-tabdef :schema schema-name :name (table_name table)
                                 :columns (map 'list
                                               (lambda (column)
                                                 (make-coldef :name (column_name column)
                                                              :datatype (data_type column)))
                                               (tuples
                                                (retrieve-table-columns schema-name
                                                                        (table_name table))))
                                 :constraints (retrieve-table-constraints schema-name
                                                                          (table_name table))))
                  (tuples (retrieve-tables schema-name))))))
 
(defun find-db-schema (schema-name)
  (multiple-value-bind
        (result columns)
      (?select '(schema_name schema_owner)
                :from 'information_schema.schemata
                :into (create-transient-table 'information_schema.schemata)
                :where (?= 'schema_name (string-downcase schema-name)))
    (declare (ignore columns))
    (and (= (length (tuples result)) 1)
         (aref (tuples result) 0))))
 
(defun retrieve-tables (schema-name)
  (?select '(table_schema table_name table_type)
           :from 'information_schema.tables
           :into 'information_schema.tables
           :where (?= 'table_schema schema-name)))
 
(defun retrieve-table-columns (schema-name table-name)
  (?select  '(table_schema table_name column_name data_type)
            :from 'information_schema.columns
            :into (create-transient-table 'information_schema.columns)
            :where (?and (?= 'table_schema schema-name)
                         (?= 'table_name table-name))))
 
(defun retrieve-table-constraints (schema-name table-name)
  (let ((constraints
         (?select (list (?alias 'con.conname 'key_name)
                        (?alias 'con.contype 'key_type)
                        (?alias 'con.confupdtype 'key_updtype)
                        (?alias 'con.confdeltype 'key_deltype)
                        (?alias 'con.conrelid 'key_tableid)
                        (?alias 'con.conkey 'key_columns)
                        (?alias 'con.confrelid 'key_reftableid)
                        (?alias 'con.confkey 'key_refcolumns))
                  :from (?inner-join (?alias 'pg_constraint 'con)
                                     (?inner-join (?alias 'pg_class 'cls)
                                                  (?alias 'pg_namespace 'nsp)
                                                  :on (?= 'cls.relnamespace 'nsp.oid))
                                     :on (?= 'con.conrelid 'cls.oid))
                  :where (?and (?= 'nsp.nspname schema-name)
                               (?= 'cls.relname table-name))
                  :into (create-transient-table 'pg.constraints))))
    (loop
       :for c :across (tuples constraints)
       :for columns = (mapcar #'car
                              (cadr (multiple-value-list
                                      (?select 'attname
                                               :from 'pg_attribute
                                               :where (?and (?= 'attrelid (key_tableid c))
                                                            (?in 'attnum (sql::make-sql-range-enum :values (parse-array (key_columns c)))))))))
       :for updtype = (map-cstr-action (key_updtype c))
       :for deltype = (map-cstr-action (key_deltype c))
       :collect (ecase (aref (key_type c) 0)
                  (#\f
                   (make-foreign-key :schema schema-name
                                     :name (key_name c)
                                     :columns columns
                                     :on-update updtype
                                     :on-delete deltype
                                     :referenced-table (caar
                                                        (cadr (multiple-value-list
                                                                (?select 'relname :from 'pg_class :where (?= 'oid (key_reftableid c))))))
                                     :referenced-columns (mapcar #'car
                                                                 (cadr (multiple-value-list
                                                                         (?select 'attname
                                                                                  :from 'pg_attribute
                                                                                  :where (?and (?= 'attrelid (key_reftableid c))
                                                                                               (?in 'attnum (sql::make-sql-range-enum :values (parse-array (key_refcolumns c)))))))))))
                  (#\p
                    (make-primary-key :schema schema-name
                                      :name (key_name c)
                                      :columns columns))
                  (#\u
                    (make-unique-key :schema schema-name
                                     :name (key_name c)
                                     :columns columns))))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some of the type mappings are PostgreSQL specific

(defmethod map-db-type ((connection postgres-connection) type)
  (let  ((type (if (listp type) (car type) type))
         (length (when (listp type) (cadr type))))
    (when (and length
               (not (member type '(id string))))
      (error "Type ~a does not have a length parameter" type))
    (cond
      ((stringp type)
       (log2:debug "MAP-DB-TYPE: Not mapping type string ~a" type)
       type)
      (t
       (ecase type
         ;; character
         (id          (format () "char(~a)" (or length 8)))
         (uuid        "uuid")
         (identifier  "text")
         (string      (format () "varchar(~a)" (or length 1024)))
         (text        "text")
         ;; number
         (boolean     "boolean")
         (integer     "integer")
         (decimal     "decimal")
         (money       "decimal")
         (percent     "decimal")
         ;; byte strings
         (raw         "bytea")
         ;; date & time
         (timestamp   "timestamp with time zone")
         (duration    "interval")
         (date        "date"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Aux functions
 
(defun parse-array (string)
  ;; Parse a string containing a postgres array into a list
  (cl-utilities:split-sequence #\, (subseq string 1 (1- (length string)))))
 
(defun map-cstr-action (c)
  (ecase (aref c 0)
    ((#\a #\space) nil)
    (#\r :restrict)
    (#\c :cascade)))
 
 
;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
