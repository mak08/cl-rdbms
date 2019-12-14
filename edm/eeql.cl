;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    Extended Entity Query Language
;;; Copyright      (c) Michael Kappert 2014
;;; Last Modified  <michael 2019-12-14 15:40:37>

(in-package :edm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extended Entity Query
;;;
;;; An extended query consists of a query path and a result projection.
;;; The first two path elements designate a schema and entity. The following
;;; path elements are associations navigating from the current source entity to
;;; to a target entity. Paths elements are sperated by a . (dot).
;;; Each path element may be filtered by a condition. In the condition, paths
;;; may be used which may associate to other entities. 
;;; The projection is either a single field, separated by dot, or a set of
;;; "subqueries", enclosed in {} (curly braces).

(defun translate-query (ee-query)
  (build-select-statement ee-query))

(defun build-select-statement (ee-query)
  ;; We put all filter conditions in the join condition. This should be equivalent,
  ;; because all joins are inner joins, and more efficient.
  ;; Where clauses and other table expression clauses are currently not supported.
  (let* ((result-fields (get-result-fields nil ee-query))
         (path (ee-query-path ee-query))
         (root-name (get-qualified-name (car path) (cadr path)))
         (root (find-entity-for-class-name root-name))
         (condition (ee-path-element-filter (cadr path)))
         (join nil))
    (setf (ee-query-path ee-query) (cddr path))
    (multiple-value-setq (root join) (adjoin-filter root join condition))
    (multiple-value-setq (root join) (adjoin-query root join ee-query))
    (make-sql-query
     :sellist (mapcar #'make-qualified-field-name result-fields)
     :table-expression (make-sql-table-expression
                        :from (or join (entity-table-name root))))))

(defun adjoin-query (root join query)
  (multiple-value-setq (root join) (adjoin-path root join (ee-query-path query)))
  (dolist (path-elt (ee-query-projection query))
    (multiple-value-setq (_ join) (adjoin-query root join path-elt)))
  (values root join))

(defun adjoin-path (root join path)
  (dolist (path-elt path)
    (multiple-value-setq (root join) (adjoin-association root join path-elt)))
  (values root join))

#+()(defun adjoin-association (root join path-element)
  (let ((association (find-element-by-name root
                                           (get-element-name path-element))))
    (multiple-value-setq (root join)
      (adjoin-association% root join association))
    (multiple-value-setq (_ join)
      (adjoin-filter root join (ee-path-element-filter path-element))))
  (values root join))

(defun adjoin-association (root join path-element)
  (let* ((association (find-element-by-name root
                                            (get-element-name path-element)))
         (target (unless (typep association 'field)
                   (find-association-target-entity association))))
    (unless
       (or (null target)
            (member (entity-table-name target) (get-join-entities join)))
      (multiple-value-setq (root join)
        (adjoin-association% root join association)))
    (multiple-value-setq (_ join)
      (adjoin-filter root join (ee-path-element-filter path-element))))
  (values root join))
 
(defmethod get-join-entities ((join join))
  (append (get-join-entities (join-left join))
          (get-join-entities (join-right join))))
 
(defmethod get-join-entities ((join symbol))
  (list join))
 
;; There is one basic join condition per join. Some association types
;; require two joins(namely, if the association is stored in a sperate table).
;; A filter condition, if present, is added to the join condition later.

(defmethod adjoin-association% (root join (association field))
  ;; No association, but a field. Do nothing.
  (values root join))

(defmethod adjoin-association% (root join (association composition-1))
  (let* ((target (find-association-target-entity association))
         (join-condition (?= (make-qualified-field-name% root (name association))
                             (make-qualified-field-name% (find-association-target-entity association) 'entity_id))))
    (values
     target
     (sql:make-join :left (or join (entity-table-name root))
                     :right (entity-table-name target)
                     :on join-condition))))

(defmethod adjoin-association% (root join (association composition-n))
  (let* ((target (find-association-target-entity association))
         (join-condition (?= (make-qualified-field-name% root 'entity_id)
                             (make-qualified-field-name% (find-association-target-entity association) 'parent_id))))
    (values
     target
     (sql:make-join :left (or join (entity-table-name root))
                     :right (entity-table-name target)
                     :on join-condition))))

(defmethod adjoin-association% (root join (association reference))
  (let* ((target (find-association-target-entity association))
         (assoc-table (get-association-table-name root association))
         (source-join-cond (?= (make-qualified-field-name% root 'entity_id)
                               (make-qualified-field-name% assoc-table 'source_id)))
         (target-join-cond (?= (make-qualified-field-name% assoc-table 'target_id)
                               (make-qualified-field-name% (find-association-target-entity association) 'entity_id))))
    (values
     target
     (sql:make-join :left (sql:make-join :left (or join (entity-table-name root))
                                           :right assoc-table
                                           :on source-join-cond)
                     :right (entity-table-name target)
                     :on target-join-cond))))

(defun adjoin-filter (root join filter)
  (when filter
    (dolist (path (get-expression-paths filter))
      (multiple-value-setq (_ join) (adjoin-path root join path)))
    ;; translate-expression is destructive, it doesn't return anything meaningful!!
    (translate-expression root filter)
    (setf (sql:join-on join)
            (?and (sql:join-on join)
                  filter)))
  (values root join))

(defmethod get-expression-paths ((expression null))
  (error "empty expression"))
(defmethod get-expression-paths ((expression sql:sql-junction))
  (loop
     :for e :in (sql:sql-junction-args expression)
     :append (get-expression-paths e)))
(defmethod get-expression-paths ((expression sql:sql-comparison ))
  (list (sql:sql-comparison-column expression)))

(defmethod translate-expression (root (expression sql-junction))
  (dolist (e (sql:sql-junction-args expression))
    (translate-expression root e)))
(defmethod translate-expression (root (expression sql-comparison))
  (setf (sql-comparison-column expression)
          (find-association-path-target root (sql-comparison-column expression))))

(defun find-association-path-target (root path)
  (loop
     :while path
     :for elt = (find-element-by-name root
                                      (get-element-name (car path)))
     :while (typep elt 'association)
     :do (setf root (find-association-target-entity elt)
               path (cdr path))
     :finally (return
                (ecase (length path)
                  (1
                    (make-qualified-field-name% root (name elt)))))))

(defun get-result-fields (root ee-query)
  ;; The result fields of EE-QUERY are defined as follows:
  ;; - If there is a projection, the result fields of the ee-queries in the projection (recursively).
  ;; - If the query has no projection part,
  ;;   - if the last path element is a field, the field itself
  ;;   - otherwise, the fields of the last path element.
  ;; The query projection paths are relative to the query path.
  ;; If ROOT is not nil, the query path is relative to root.
  ;; If ROOT is nil, the first two query path elements determine the query root (schema and entity).
  (if (ee-query-projection ee-query)
    (let ((entity (find-entity-at-path root (ee-query-path ee-query))))
      (loop :for query :in (ee-query-projection ee-query) :append (get-result-fields entity query)))
    (find-fields-at-path root (ee-query-path ee-query))))

(defun find-entity-at-path (root path)
  ;; Assume that path designates an entity (directly or via associations),
  ;; but not a field.
  (when (null root)
    (let ((root-name (get-qualified-name (car path) (cadr path))))
      (setf root (find-entity-for-class-name root-name)
            path (cddr path))))
  (dolist (element path root)
    (setf root (find-association-target-entity
                (find-element-by-name root
                                      (get-element-name element))))))

(defun find-fields-at-path (root path)
  ;; Assume that pth designates a field (directly or via associations).
  ;; The last path element may also be an association, in which case
  ;; the path designates the set of fields of the target entity.
  (when (null root)
    (let ((root-name (get-qualified-name (car path) (cadr path))))
      (setf root (find-entity-for-class-name root-name)
            path (cddr path))))
  (loop
     :while path
     :for elt = (find-element-by-name root
                                      (get-element-name (car path)))
     :while (typep elt 'association)
     :do (setf root (find-association-target-entity elt)
               path (cdr path))
     :finally (return
                (case (length path)
                  (0
                    (loop :for field :in (entity-fields root) :collect (list root field)))
                  (1
                    (list (list root elt)))
                  (otherwise
                    (error "Excess path element ~a" (ee-path-element-element (cadr path))))))))

(defun make-qualified-field-name (result-field)
  (make-qualified-field-name% (car result-field)
                              (name (cadr result-field))))

(defmethod make-qualified-field-name% ((entity entity) column-name)
  (intern
   (format () "~a.~a"
           (entity-table-name entity)
           column-name)))
(defmethod make-qualified-field-name% ((entity symbol) column-name)
  (intern
   (format () "~a.~a" entity column-name)))

(defun get-qualified-name (schema-part entity)
  (intern
   (string-upcase
    (format () "~a.~a"
            (ee-path-element-element schema-part)
            (ee-path-element-element entity)))))

(defun get-element-name (element)
  (intern
   (string-upcase
    (ee-path-element-element element))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parser for infix syntax

(defparser parse-ee-query
    :rules (
  (ee-query
   (:seq ee-path (:opt ee-projection)))
  (ee-path
   (:seq ee-path-element
         (:rep ee-path-rest)))
  (ee-path-rest
   (:seq "." ee-path-element))
  (ee-path-element
   (:seq :identifier (:opt ee-filter)))
  (ee-filter
   (:seq "[" ee-condition "]"))
  (ee-condition
   (:alt ee-or ee-conjunction))
  (ee-or
   (:seq ee-conjunction "OR" ee-condition))
  (ee-conjunction
   (:alt ee-and ee-predicate))
  (ee-and
   (:seq ee-predicate "AND" ee-conjunction))
  (ee-predicate
   (:alt ee-comparison ee-nested-condition))
  (ee-comparison
   (:seq ee-path ee-predop :string))
  (ee-nested-condition
   (:seq "(" ee-condition ")"))
  (ee-predop
   (:alt "=" ">" "<" ">=" "<="))
  (ee-projection
   (:seq "{" ee-projection-elements "}"))
  (ee-projection-elements
   (:seq ee-query (:rep ee-projection-elements-rest)))
  (ee-projection-elements-rest
   (:seq "," ee-query))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;, Parse tree construction

(defstruct ee-query path projection)
(defstruct ee-path-element element filter)

(defun ee-query (tree level)
  (make-ee-query :path (car tree) :projection (cadr tree)))
(defun ee-path (tree level)
  (cons (car tree) (cadr tree)))
(defun ee-path-element (tree level)
  (make-ee-path-element :element (token-value (car tree))
                        :filter (cadr tree)))

(defun ee-path-rest (tree level) (cadr tree))
(defun ee-condition (tree level) tree)
(defun ee-or (tree level)
  (make-sql-junction :op :or :args (list (first tree) (third tree))))
(defun ee-conjunction (tree level) tree)
(defun ee-and (tree level)
  (make-sql-junction :op :and :args (list (first tree) (third tree))))
(defun ee-filter (tree level)
  (cadr tree))
(defun ee-predicate (tree level)
  tree)
(defun ee-comparison (tree level)
  (make-sql-comparison :op (token-value (second tree))
                       :column (first tree)
                       :value (string-trim "'" (token-value (third tree)))))
(defun ee-nested-condition (tree level)
  (cadr tree))
(defun ee-predop (tree level)
  tree)
(defun ee-projection (tree level)
  (cadr tree))
(defun ee-projection-elements (tree level)
  (cons (car tree) (cadr tree)))
(defun ee-projection-elements-rest (tree level)
  (cadr tree))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
