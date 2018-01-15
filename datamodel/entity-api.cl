;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   Entity Accessors that manipulate the database 
;;; Author        Michael Kappert 2014
;;; Last Modified <michael 2018-01-14 20:23:21>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Operations on entities
;;;
;;; * Update affected DB tables directly. Operations may or may not take place
;;;   within a transaction. If there is no open transaction, each update will
;;;   be implicitly committed.
;;;
;;; * The entity classes are generated upfront to allow method specialization
;;;
;;; - delete-instance
;;;   - mark instance deleted
;;;   - unset composition entities
;;;   - delete reference entries (source + target)
;;;
;;; - create-instance
;;;   - Create instance with fields only.
;;;   - Save fields: insert into entity table
;;;
;;; - set/reset/add/remove-element <intance> <element> <value>
;;;   - update instance
;;;   - update entity table
;;;     => if in open transaction, commit with transaction
;;;        otherwise implicit commit
;;;
;;; - set composition entity
;;;   - set instance field
;;;   - unset previous composition in old target
;;;   - set parent_id in new target
;;;   - update source and target entity tables
;;;
;;; - add/remove reference entity
;;;   - update source instance
;;;   - update source entity table
;;;   - update relation table
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ToDo
;;; - 'Automatic' SQL values like column default, NOW etc.
;;;    => disallow or fully support in create-instance etc.
;;;    => currently instances are created with missing (DEFAULT) or uninterpreted (NOW) values
;;;
;;; - Wrappers
;;;    => Generate entity-specific accessors
;;;    => pm.items <sales_order> on top of get-element <type> <element>
;;;    => Note that there is already pm.entrydate% etc (for fields) but these function don't
;;;       update the database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(in-package :datamodel)
 
(defvar *instances-ht*
  ;; A weak(value) hashtable for caching instances
  (make-hash-table :test #'equal
                   #+(or :clisp :ccl) :weak #+(or :sbcl) :weakness :value))
 
(defmethod unique-instance ((thing entity-class))
  ;; ToDo: If there is a buffered instance and its lock mode is lower than the
  ;; provided instance's lock-mode, update the buffered instance and return it.
  (let* ((id (entity_id thing))
         (instance (gethash id *instances-ht*)))
    (or instance
        (setf (gethash id *instances-ht*) thing))))

(defvar *instance-counter* -1)

(defmethod create-instance ((entity-type symbol)
                            &rest args
                            &key (uuid (create-uuid))
                            &allow-other-keys)
  ;; ToDo: handle default field values.
  ;; An elegant way is to generate create-<entity> methods with defaulted keyword arguments:
  ;; CREATE-PERSON (... &KEY FIRSTNAME LASTNAME (HOBBY "Sailing") ...)
  (remf args :uuid)
  (let* ((entity (find-entity-for-class-name entity-type))
         (elements (entity-elements entity))
         (fields)
         (compositions)
         (associations)
         (instance))
    (loop
       :for (key val) :on args :by #'cddr
       :for element = (find (read-from-string (symbol-name key) (entity-package entity)) elements :key #'name)
       :do (etypecase element
             (null
              (error "Entity type '~a' does not have an element called '~a'" entity-type key))
             (field
              (setf fields
                    (append (list key val) fields)))
             (composition-1
              (setf fields
                    (append (list key (entity_id val)) fields))
              (push (list element val) compositions))
             (association
              (push (list element val) associations))))
    (setf instance (apply #'make-instance entity-type :entity_id uuid fields))
    (with-nested-transaction
      (multiple-value-bind (columns values)
          (loop
             :for (col val) :on (list* :entity_id uuid fields) :by #'cddr
             :collect col :into c
             :collect val :into v
             :finally (return (values c v)))
        (?insert-into entity-type
                      :values (make-sql-tuple :elements values)
                      :columns (make-sql-tuple :elements columns)))
      (loop
         :for (element value) :in compositions
         :do (update-composition-1 entity element instance value))
      (loop
         :for (element value) :in associations
         :do (update-association entity element instance value))
      (values (unique-instance instance)))))
 
(defmethod retrieve-instance ((entity-type symbol) &rest args &key (lock-mode :none) &allow-other-keys)
  (let ((instances (apply #'retrieve entity-type :lock-mode lock-mode args)))
    (case (length instances)
      (1
       (car instances))
      (0
       (error "No instances found"))
      (t
       (error "Multiple instances found")))))
 
 
(defmethod retrieve ((entity-type symbol) &rest args &key (lock-mode :none) &allow-other-keys)
  ;; - Retrieve fields and singleton composition elements. These are stored in
  ;;   the entity table directly.
  ;; - Other elements are retrieved ONLY ON ACCESS (set/get/add/remove-element)
  ;;   * UNBOUND/DEFAULT = not retrieved, NIL = retrieved but empty
  ;; - If the entity_id is provided, try the instance buffer first. If we don't have
  ;;   a entity_id, we must read the entity table.
  ;;
  ;;   Options:
  ;;   - implement secondary keys first
  ;;   - use whatever is provided and signal an error only if it wasn't unique
  (apply #'retrieve (find-entity-for-class-name entity-type) :lock-mode lock-mode args))
 
(defmethod retrieve ((entity entity) &rest args &key (lock-mode :none) &allow-other-keys)
  ;; ToDo: return the buffered instance only if its lock-mode is greater than the requested mode.
  (loop :while (remf args :lock-mode))
  (let* ((entity-id (getf args :entity_id))
         (buffered (and entity-id
                        (gethash entity-id *instances-ht*))))
    (if buffered
        (list buffered)
        (multiple-value-bind (columns rows)
            ;; This may yield unexpected results if the DB isolation level is too low!!
            (?select :all
                     :from (entity-table-name entity)
                     :where (make-sql-junction :op "and"
                                               :args (loop :for (col val) :on args :by #'cddr :collect (?= col val)))
                     :lock-mode lock-mode
                     :nowait (mode<= :share lock-mode))
                                 (setf columns (mapcar #'make-keyword columns))
          (mapcar (lambda (row)
                    (create-instance% (entity-class-name entity) columns row))
                  rows)))))

(defmethod delete-instance ((instance entity-class))
  (let* ((class-name (type-of instance))
         (assocs (entity-associations  (find-entity-for-class-name class-name))))
    (with-nested-transaction
      (dolist (assoc assocs)
        (typecase assoc
          (composition-1
           (let ((c-entity
                  (gethash (funcall
                             (slot-reader-name (find-entity-for-class-name class-name)
                                               assoc)
                             instance)
                           *instances-ht*)))
             (when c-entity
               (setf (deleted c-entity) t))))))
      ;; (delete-instance (get-element instance assoc)))))
      (?delete class-name :where (?= 'entity_id (entity_id instance)))
      (setf (deleted instance) t))
    (values instance)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SET Element
;;;
;;; set-element needs to deal with concurrent modifications. Modification may fail,
;;; if the update lock cannot be obtained. In this case we raise an exception.
;;; (Modification will not fail if the instance was RETRIEVEd with lock mode UPDATE)
;;; Note that UPDATE will wait for concurrent locks - use SELECT ... FOR UPDATE NOWAIT to obtain the lock.
;;; Depending on the current lock mode, the instance needs to be re-read and may have changed.
;;; In this case, the instance buffer must be refreshed. 
 
(defmethod set-element ((instance entity-class) (element-name symbol) (value t))
  (let* ((entity (find-entity-for-class-name (type-of instance)))
         (element (find-element-by-name entity element-name)))
    (set-element instance element value)))
 
(defmethod set-element ((instance entity-class) (element field) (value t))
  (let* ((entity (find-entity-for-class-name (type-of instance))))
    (funcall (slot-writer-name entity element) value instance)
    (update-field entity element instance value)
    (values instance)))
 
(defmethod set-element ((instance entity-class) (element composition-1) (value t))
  ;; parent and child are linked by the parent's child field and the child's parent_id field.
  ;; The latter is used in the foreign key, allowing automatic deletion of the child.
  (let* ((entity (find-entity-for-class-name (type-of instance)))
         (value-entity (find-entity-for-class-name (type-of value)))
         (old-value (get-element% instance element)))
    (let ((parent
           (funcall (slot-reader-name value-entity 'parent_id) value)))
      (when parent
        (error "~a is already assigned to ~a; it cannot be assigned to another instance" value parent)))
    ;; Store only the target entity id to prevent endless loops
    (when old-value
      (funcall (slot-writer-name value-entity 'parent_id) () old-value))
    (funcall (slot-writer-name entity element) (entity_id value) instance)
    ;; The parent_id actually needs only be stored on the DB,
    ;; it is not really needed at the lisp instance.
    (funcall (slot-writer-name value-entity 'parent_id) (entity_id instance) value)
    (update-composition-1 entity element instance value old-value)
    (values instance)))
 
(defmethod set-element ((instance entity-class) (element association) (value t))
  (let* ((entity (find-entity-for-class-name (type-of instance))))
    (funcall (slot-writer-name entity element) value instance)
    (update-association entity element instance value)
    (values instance)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GET Element
 
(defun get-element (instance element-name)
  (get-element% instance
                (find-element-by-name
                 (find-entity-for-class-name (type-of instance))
                 element-name)))
 
(defmethod get-element% ((instance entity-class) (element field))
  ;; Fields are stored in the entity table.
  (let ((entity (find-entity-for-class-name (type-of instance))))
    (funcall (slot-reader-name entity element) instance)))
 
(defmethod get-element% ((instance entity-class) (element composition-1))
  ;; We store the target entity id in field <element>_id of the parent entity.
  (let* ((entity (find-entity-for-class-name (type-of instance)))
         (target-id (funcall (slot-reader-name entity element) instance)))
    (when target-id
      (retrieve-instance (entity-class-name
                          (find-association-target-entity element))
                         :entity_id target-id))))
 
(defmethod get-element% ((instance entity-class) (element composition-n))
  ;; composition-n is stored in the parent_id field of the child entity.
  ;; Target entity ids are not stored in a separate table.
  ;; => SELECT the target entity table WHERE parent_id = source.entity_id
  (let ((entity (find-entity-for-class-name (type-of instance)))
        (target-entity (find-association-target-entity element)))
    (unless (slot-boundp instance (name element))
      (let ((target-entities (retrieve target-entity 'parent_id (entity_id instance))))
        (funcall (slot-writer-name entity element)
                 target-entities
                 instance)))
    (funcall (slot-reader-name entity element) instance)))
 
(defmethod get-element% ((instance entity-class) (element singleton))
  (let ((entity (find-entity-for-class-name (type-of instance))))
    (unless (slot-boundp instance (name element))
      (let ((target-entities (load-association/join entity (entity_id instance) element)))
        (unless (<= (length target-entities) 1)
          (error "Invalid target multiplicity for ~a" element))
        (funcall (slot-writer-name entity element)
                 (car target-entities)
                 instance)))
    (funcall (slot-reader-name entity element) instance)))
 
(defmethod get-element% ((instance entity-class) (element polyton))
  (let ((entity (find-entity-for-class-name (type-of instance))))
    (unless (slot-boundp instance (name element))
      (let ((target-entities (load-association/join entity (entity_id instance) element)))
        (funcall (slot-writer-name entity element)
                 target-entities
                 instance)))
    (funcall (slot-reader-name entity element) instance)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mass Loading
 
(defun load-entity (entity-name)
  (let ((entity (find-entity-for-class-name entity-name)))
    (values (retrieve entity-name)
            (loop
               :for element :in (entity-associations entity)
               :collect (load-element entity element)))))

(defgeneric load-element (entity element &optional lock-mode))

(defmethod load-element (source-entity (element singleton) &optional (lock-mode :none))
  (let ((target-entity (find-association-target-entity element)))
    (retrieve target-entity :lock-mode lock-mode)))

(defmethod load-element (source-entity (element composition-n) &optional (lock-mode :none))
  (let ((target-entity (find-association-target-entity element)))
    (multiple-value-bind (columns rows)
        (?select :all :from  (entity-table-name target-entity) :where (?in 'parent_id (?select 'entity_id :from (entity-table-name source-entity)))  :lock-mode lock-mode)
      (setf columns (mapcar #'make-keyword columns))
      (loop
         :for row in rows
         :collect (create-instance% (entity-class-name target-entity) columns row)))))

(defmethod load-element (source-entity (element polyton) &optional (lock-mode :none))
  (let* ((target-entity (find-association-target-entity element)))
    (multiple-value-bind (columns rows)
        (?select (cons 'entity_id (mapcar #'name (entity-fields target-entity)))
                 :from (?inner-join (get-association-table-name source-entity element)
                                    (entity-table-name target-entity)
                                    :on (?= 'target_id 'entity_id))
                 :lock-mode lock-mode
                 :nowait (mode<= :share lock-mode))
      (setf columns (mapcar #'make-keyword columns))
      (loop
         :for row :in rows
         :collect (create-instance% (entity-class-name target-entity) columns row)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RBA
 
(defun load-association/join (entity source-id element &optional (lock-mode :none))
  (let ((target-entity (find-association-target-entity element)))
    (multiple-value-bind (columns rows)
        (?select (cons 'entity_id (mapcar #'name (entity-fields target-entity)))
                 :from (?inner-join (get-association-table-name entity element)
                                    (entity-table-name target-entity)
                                    :on (?= 'target_id 'entity_id))
                 :where (?= 'source_id source-id)
                 :lock-mode lock-mode
                 :nowait (mode<= :share lock-mode))
      (loop
         :for row :in rows
         :collect (unique-instance
                   (apply #'make-instance (entity-class-name target-entity)
                          (loop
                             :for value :in row
                             :for field :in columns
                             :append (list (make-keyword field)
                                           value))))))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ADD Element
 
(defmethod add-element ((instance entity-class) (element-name symbol) (value t))
  (let* ((entity (find-entity-for-class-name (type-of instance)))
         (element (find-element-by-name entity element-name)))
    (add-element instance element value)))
 
(defmethod add-element ((instance entity-class) (element composition-n) (value t))
  (let* ((entity (find-entity-for-class-name (type-of instance)))
         (value-entity (find-entity-for-class-name (type-of value))))
    (let ((parent
           (funcall (slot-reader-name value-entity 'parent_id) value)))
      (when parent
        (error "~a is already assigned to ~a; it cannot be assigned to another instance" value parent)))
    ;; The parent_id actually needs only be stored on the DB,
    ;; it is not really needed at the lisp instance.
    (funcall (slot-writer-name value-entity 'parent_id) (entity_id instance) value)
    (update-composition-n entity element instance value)
    (values instance)))

(defmethod add-element ((instance entity-class) (element polyton) (value t))
  (let* ((entity (find-entity-for-class-name (type-of instance)))
         (slot-writer (slot-writer-name entity element)))
    (let ((old-values (get-element% instance element)))
      (?insert-into (table-name element) :values (?t (entity_id instance) (entity_id value)))
      (funcall slot-writer (append old-values (list value)) instance))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REMOVE Element



(defmethod remove-element :before (instance element value)
  (warn "REMOVE-ELEMENT implementation is not up-to-date"))
(defmethod remove-element (instance element value)
  (warn "No method on REMOVE-ELEMENT for arguments of type ~a ~a ~a" (type-of instance) (type-of element) (type-of value)))

(defmethod remove-element ((instance entity-class) (element-name symbol) (value t))
  (let* ((entity-class (class-of instance))
         (entity (find-entity-for-class-name (class-name entity-class)))
         (element (find-element-by-name entity element-name)))
    (remove-element instance element value)))
 
(defmethod remove-element ((instance entity-class) (element polyton) (value t))
  (let* ((entity-class (class-of instance))
         (entity (find-entity-for-class-name (class-name entity-class)))
         (slot-writer (slot-writer-name entity element)))
    (let ((old-values (get-element% instance element)))
      (?delete (table-name element)
               :where (?and (?= 'source_id (entity_id instance))
                            (?= 'target_id (entity_id value))))
      (funcall slot-writer (delete value old-values) instance))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers
 
(defun create-instance% (class-name columns values &key (lock-mode :share))
  (let* ((id (nth (position :entity_id columns) values))
         (instance (gethash id *instances-ht*)))
    (or instance
        (setf (gethash id *instances-ht*)
              (apply #'make-instance class-name
                     :lock-mode lock-mode
                     (loop
                        :for value :in values
                        :for field :in columns
                        :append (list field value)))))))

(defun find-target-element-accessor (association element-name)
  (let* ((entity (find-association-target-entity association))
         (element (find element-name (entity-elements entity) :key #'name)))
    (slot-reader-name entity element)))
 
(defun find-element-by-name (entity element)
  (or (find element (entity-elements entity) :key #'name)
      (error "Element ~a not defined in entity ~a" element entity)))
 
(defun is-default (value)
  (null value))
 
(defun update-field (entity element instance value)
  (?update (entity-table-name entity)
           :set (name element) :to value
           :where (?= 'entity_id (entity_id instance))))
 
(defun update-composition-1 (entity element instance value &optional (old-value nil))
  (let ((target-table (entity-table-name (find-association-target-entity element))))
    (when old-value
      (?update target-table
               :set 'parent_id :to 'null
               :where (?= 'entity_id (entity_id old-value))))
    (?update (entity-table-name entity)
             :set (name element) :to (entity_id value)
             :where (?= 'entity_id (entity_id instance)))
    (?update target-table
             :set 'parent_id :to (entity_id instance)
             :where (?= 'entity_id (entity_id value)))))
 
(defun update-composition-n (entity element instance value)
  (let ((target-table (entity-table-name (find-association-target-entity element))))
    (?update target-table
             :set 'parent_id :to (entity_id instance)
             :where (?= 'entity_id (entity_id value)))))

(defmethod update-association ((entity entity) (element singleton) (instance entity-class) (value t))
  (?delete (get-association-table-name entity element) :where (?= 'source_id (entity_id instance)))
  (?insert (?t (entity_id instance) (entity_id value)) :into (get-association-table-name entity element)))
 
(defmethod update-association ((entity entity) (element polyton) (instance entity-class) (value t))
  (?delete (get-association-table-name entity element)
           :where (?= 'source_id (entity_id instance)))
  (?insert-into (get-association-table-name entity element)
                :values  (loop
                            :for target :in value
                            :collect (?t (entity_id instance)
                                         (entity_id target)))))
 
(defmethod update-association ((entity entity) (element composition-n) (instance entity-class) (value t))
  (let ((table-name (entity-table-name (find-association-target-entity element))))
    (?update table-name :set 'parent_id :to 'null :where (?= 'parent_id (entity_id instance)))
    (?update table-name :set 'parent_id :to (entity_id instance) :where (?in 'entity_id (sql::make-sql-range-enum :values (mapcar #'entity_id value))))))
 
(defun unset-field (entity element instance)
  (?update (entity-table-name entity) :set (?= (name element) 'null) :where (?= 'entity_id (entity_id instance))))
 
(defun get-association-table-name (entity element)
  (intern (format () "~a.~a"
                  (entity-schema entity)
                  (make-association-table-name entity element))))
 
;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
