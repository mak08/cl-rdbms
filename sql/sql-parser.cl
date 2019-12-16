;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2019-12-17 00:46:33>

(in-package :sql)


(defun _table-definition (symbol tree level)
  (destructuring-bind (crt tmp tbl exist schema name po firstcol cols-and-constraints cp)
      tree
    (let ((coldefs)
          (constraints))
      (dolist (def cols-and-constraints)
        (typecase (cadr def)
          (coldef
           (push (cadr def) coldefs))
          (tabcon
           (push (cadr def) constraints))))
      (make-tabdef :schema (when schema (token-value schema))
                   :name (token-value name)
                   :columns (cons firstcol coldefs)
                   :constraints constraints))))

(defun _column-def (symbol tree level)
  (destructuring-bind (name type-name colcons)
      tree
    (make-coldef :name (token-value name)
                 :datatype type-name)))

(defun _table-constraint (symbol tree level)
  (destructuring-bind (name spec)
      tree
    (let ((ctype (token-value (car spec)))
          (name (when (cadr name) (token-value (cadr name)))))
      (cond
        ((string= ctype "PRIMARY")
         (make-primary-key :name name :columns (third spec)))
        ((string= ctype "UNIQUE")
         (make-unique-key :name name :columns (second spec)))
        ((string= ctype "FOREIGN")
         (make-foreign-key :name name :columns (third spec)))))))
    
(defun _primary (symbol tree level)
  (destructuring-bind (p k columns)
      tree
    (make-primary-key :columns columns)))

(defun _name-list (symbol tree level)
  (destructuring-bind (op firstname restnames cp)
      tree
    (cons (token-value firstname)
          (mapcar (lambda (name) (token-value (second name)))
                  restnames))))

(defun _type-name (symbol tree level)
  (destructuring-bind (names params)
      tree
    (token-value (first names))))

(defparser parse-table-definition
    :tokens ((__name (:seq :letter (:rep (:alt :letter :digit "_"))))
             (__signed-number (:seq (:opt (:alt "+" "-"))) :numeric))
    :rules ((_table-definition
             (:seq "CREATE" (:opt (:alt "TEMP" "TEMPORARY"))
                   "TABLE" (:opt (:seq "IF" "NOT" "EXISTS"))
                   (:opt (:seq __name ".")) __name
                   "("
                   _column-def
                   ;; This grammar allows mixing columns and constraints while
                   ;; in SQL the column definitions must come before the constraints.
                   ;; However, the 'natural' grammar gets stuck attempting to parse
                   ;; constraints as columns. It is unclear why it does not backtrack.
                   (:rep (:alt 
                          (:seq "," _table-constraint)
                          (:seq "," _column-def)))
                   ")"))
            (_column-def
             (:seq __name (:opt _type-name) (:rep _column-constraint)))
            (_type-name
             (:seq (:rep __name)
                   (:opt (:seq "(" __signed-number (:opt (:seq "," __signed-number)) ")"))))
            (_column-constraint
             (:seq (:opt (:seq "CONSTRAINT" __name))
                   (:alt _primary _not-null _unique _check _default _foreign-key-clause))) 
            (_primary
             (:seq "PRIMARY" "KEY" (:opt (:alt "ASC" "DESC")) _conflict-clause (:opt "AUTOINCREMENT")))
            (_conflict-clause
             (:opt (:seq "ON" "CONFLICT" (:alt "ROLLBACK" "ABORT" "FAIL" "IGNORE" "REPLACE"))))
            (_not-null
             (:seq "NOT" "NULL" _conflict-clause))
            (_unique
             (:seq "UNIQUE" _conflict-clause))
            (_check
             (:seq "CHECK" "(" _expr ")"))
            (_default
              (:seq "DEFAULT" (:alt __signed-number _literal (:seq "(" _expr ")"))))
            (_collate
             (:seq "COLLATE" __name))
            (_name-list
             (:seq  "(" __name (:rep (:seq "," __name)) ")"))
            (_foreign-key-clause
             (:seq "REFERENCES" __name
                   (:opt _name-list)
                   (:rep (:alt (:seq "ON"
                                     (:alt "DELETE" "UPDATE")
                                     (:alt (:seq "SET" "NULL")
                                           (:seq "SET" "DEFAULT")
                                           "CASCADE"
                                           "RESTRICT"
                                           (:seq "NO" "ACTION")))
                               (:seq "MATCH" __name)))
                   (:opt (:seq (:opt "NOT")
                               "DEFERRABLE"
                               (:opt (:seq "INITIALLY" (:opt "DEFERRED"
                                                             "IMMEDIATE")))))))
            (_table-constraint
             (:seq (:opt (:seq "CONSTRAINT" __name))
                   (:alt (:seq "PRIMARY" "KEY" _name-list _conflict-clause)
                         (:seq "UNIQUE" _name-list _conflict-clause)
                         _check
                         (:seq "FOREIGN" "KEY" _name-list _foreign-key-clause))))
            (_expr (:opt __name))
            (_literal (:alt :string :nuemric "TRUE" "FALSE" ))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
