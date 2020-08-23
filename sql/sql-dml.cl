;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael 2013
;;; Last Modified <michael 2020-01-28 15:58:33>
 
(in-package :sql)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQL statements

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query
 
(defstruct (sql-query (:include sql-statement))
  (sellist "*")
  (table-expression "")
  (lock nil))
 
(defmethod sql-query ((conn t) (sql-statement sql-query))
  (sql-query conn
             (let ((*print-circle* nil))
               (with-output-to-string (s)
                 (serialize-for-connection conn sql-statement s)))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Update, Insert, Delete
 
(defstruct (sql-table-statement (:include sql-statement))
  table)

(defstruct (sql-insert (:include sql-table-statement))
  columns
  values)

(defstruct (sql-update (:include sql-table-statement))
  expression
  condition)

(defstruct (sql-upsert (:include sql-insert))
  key-columns
  update)

(defstruct (sql-delete (:include sql-table-statement))
  condition)
 
(defmethod sql:sql-exec ((conn t) (sql-statement sql-table-statement))
  (sql:sql-exec conn
                (let ((*print-circle* nil))
                  (with-output-to-string (s)
                    (serialize-for-connection conn sql-statement s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SQL Expressions
 
(defstruct subquery
  sellist
  table-expression
  alias)
 
;; Joins
(defstruct derived-table left right)
(defstruct (join (:include derived-table)) (type :inner)  on using)
(defstruct (union (:include derived-table)))
(defstruct (difference (:include derived-table)))
 
(defstruct select-list items distinct-p)
 
(defstruct select-item table colspec alias)
 
;; Table Expression
(defstruct sql-table-expression from where groupby having (lock-mode :none) (nowait nil))
 
(defstruct sql-table-reference table-name alias columns)
 
(defstruct sql-junction op args)
(defstruct sql-negation argument)
(defstruct sql-comparison op column value)
(defstruct sql-range-predicate column lower upper)
(defstruct sql-alnum-expression op left right)
(defstruct sql-function name arguments)
(defstruct sql-postfix expr name)
(defstruct sql-tuple elements)
(defstruct sql-assignment colex valex)
(defstruct sql-range-enum values)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQL Expression
 
;; # Next steps
;;   # create tuple class for select-list on-the-fly
;;   # allow field-mapper in ?select ?
;;   # use qualified column names in mapper ?!
 
;;   # implement ?select
;;     - result tuple class can be specified or derived from query
;;     - derive tuple class: (anon tuple class)
;;       - if select-list of query is not empty, it determines the slots
;;       - othherwise the set of tables in the top-level join determines the slots (use union of table columns)
;;     - think about scalar queries
;;     - implement table & column aliases
;;     - respect aliases in result tuple class definition
 
;;   # Memory considerations
;;     - how many anon tuple classes will de generated?
;;     - memory consumption: ca. 1 KBytes per class
 
 
(defun ?insert-into (table  &key columns values)
  (sql:sql-exec
   *current-connection*
   (make-sql-insert :table table
                    :columns (if (and columns (symbolp columns))
                                 (list columns)
                                 columns)
                    :values values)))

(defgeneric ?insert (values &key into columns))
(defmethod ?insert ((values t) &key into columns)
  (sql:sql-exec
   *current-connection*
   (make-sql-insert :table into
                    :columns (if (and columns (symbolp columns))
                                 (list columns)
                                 columns)
                    :values values)))

(defgeneric ?update (table &key set to where)) 
(defmethod ?update (table &key set to where)
  (sql:sql-exec
   *current-connection*
   (make-sql-update :table table
                    :expression (make-sql-assignment :colex set :valex to)
                    :condition where)))
 
(defun ?delete (table &key where)
  (sql:sql-exec
   *current-connection*
   (make-sql-delete :table table
                    :condition where)))
 
(defmacro ?select (select-list &key (into nil) (rows :multi)  appending from where groupby having (lock-mode :none) (nowait nil))
  (declare (ignorable appending))
  `(macrolet ((?select (select-list &key from where groupby having as)
                `(let* ((select-list ,select-list)
                        (sellist
                         (cond
                           ((eq select-list :all)
                            (make-select-list :items (list '*)))
                           ((not (listp select-list))
                            (make-select-list :items (list select-list)))
                           (t
                            (make-select-list :items select-list)))))
                   (make-subquery :sellist sellist
                                  :table-expression (make-sql-table-expression :from ,from :where ,where :groupby ,groupby :having ,having)
                                  :alias ,as))))
     (let* ((select-list ,select-list)
            (sellist
             (cond
               ((eq ,select-list :all)
                (make-select-list :items (list '*)))
               ((not (listp select-list))
                (make-select-list :items (list select-list)))
               (t
                (make-select-list :items select-list)))))
       (multiple-value-bind (columns rows)
           (sql:sql-query
            *current-connection*
            (make-sql-query :sellist sellist
                            :table-expression (make-sql-table-expression
                                               :from ,from
                                               :where ,where
                                               :groupby ,groupby
                                               :having ,having
                                               :lock-mode ,lock-mode
                                               :nowait ,nowait)))
         (cond
           (,into
            (let ((container (output-container-for-spec ,into :mode ,rows)))
              (fetch container (list columns rows))))
           (t
            (values columns rows)))))))
 
(defmacro ?alias (column alias)
  `(make-select-item :colspec ,column :alias ,alias))
 
(defmacro ?inner-join (left right &key on using)
  `(make-join :left ,left
              :right ,right
              :type 'INNER
              :on ,on
              :using ,using))
 
(defmacro ?left-join (left right &key on using)
  `(make-join :left ,left
              :right ,right
              :type "LEFT OUTER"
              :on ,on
              :using ,using))
 
(defun ?not (argument)
  (make-sql-negation :argument argument))
(defun ?and (&rest arguments)
  (make-sql-junction :op "AND" :args arguments))
(defun ?or (&rest arguments)
  (make-sql-junction :op "OR" :args arguments))
 
(defun ?unnest (argument)
  (make-sql-function :name "unnest" :arguments (list argument)))
 
(defun ?= (column value)
  (make-sql-comparison :op "=" :column column :value value))
(defun ?< (column value)
  (make-sql-comparison :op "<" :column column :value value))
(defun ?<= (column value)
  (make-sql-comparison :op "<=" :column column :value value))
(defun ?> (column value)
  (make-sql-comparison :op ">" :column column :value value))
(defun ?>= (column value)
  (make-sql-comparison :op ">=" :column column :value value))
(defun ?<> (column value)
  (make-sql-comparison :op "<>" :column column :value value))
 
(defun ?like (column value)
  (make-sql-comparison :op "LIKE" :column column :value value))
(defun ?in (column value)
  (make-sql-comparison :op "IN"
                       :column column
                       :value value))
 
(defun ?max (argument)
  (make-sql-function :name "MAX" :arguments (list argument)))

(defun ?min (argument)
  (make-sql-function :name "MIN" :arguments (list argument)))

(defun ?count (argument)
  (make-sql-function :name "COUNT" :arguments (list argument)))
 
(defun ?some (argument)
  (make-sql-function :name "SOME" :arguments (list argument)))

(defun ?null (argument)
  (make-sql-postfix :expr argument :name "ISNULL"))

(defun ?not-null (argument)
  (make-sql-postfix :expr argument :name "NOTNULL"))

(defun ?between (column lower upper)
  (make-sql-range-predicate :column column :lower lower :upper upper))
 
(defun ?+ (arg1 arg2)
  (make-sql-alnum-expression :op '+ :left arg1 :right arg2))
 
(defun ?- (arg1 arg2)
  (make-sql-alnum-expression :op '- :left arg1 :right arg2))
 
(defun ?* (arg1 arg2)
  (make-sql-alnum-expression :op '* :left arg1 :right arg2))
 
(defun ?/ (arg1 arg2)
  (make-sql-alnum-expression :op '/ :left arg1 :right arg2))
 
(defun ?t (&rest elements)
  (make-sql-tuple :elements elements))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Aux functions
 
(defun normalize-table-reference (thing)
  (cond
    ((atom thing)
     (make-sql-table-reference :table-name thing))
    ((eq (car thing) 'quote)
     (make-sql-table-reference :table-name (cadr thing)))
    ((consp (car thing))
     ;; ('tabname ... )
     (make-sql-table-reference :table-name (eval (car thing))
                               :alias (eval (caddr thing))
                               :columns (cadddr thing)))
    (t
     (eval thing))))
 
;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
