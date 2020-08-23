;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author        Michael Kappert 2012
;;; Last Modified <michael 2020-01-29 16:56:27>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Design
;;;
;;; Entities provide an object-oriented and application-oriented
;;; programming interface to ER data models. Entities support various types of
;;; relationship (composition, reference, delegation). 
;;; - Object oriented: access to entities is based on classes and methods
;;; - Application oriented: 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; * Associations
;;;   - weak:   existance of target is not guaranteed
;;;   - referential: target cannot be deleted while associated
;;;   - hierarchical: target is deleted when source is deleted
;;;   - non-hierarchical: target can exist independant of source
;;;
;;;   Observation:
;;;   - target row of a referential hierarchical cannot be target of
;;;     another association
;;;   - referential hierarchical assoc must have source mult <= 1
;;;
;;;   We provide four types of associations:
;;;   * Composition 1:0..n, referential, hierarchical
;;;     - A composition target cannot be used in another association
;;;     - Target instances are automatically deleted when the parent is deleted.
;;;     - Compositions are implemented without a separate association table.
;;;   * Delegation 1..n:0..n, referential, hierarchical
;;;     - Entities may be used as the target of more than one delegation
;;;     - Target instances are automatically deleted WHEN THE LAST REFERENCE is deleted
;;;   * Reference 0..n:0..n, referential, non-hierarchical(default)
;;;   * Associations 0..n:0..n, NON-REFERENTIAL
;;;
;;; - Implementation of associations
;;;
;;;   * Multiplicity
;;;   - In the general case an association can be implemented by a separate table.
;;;     The target entity is given by the source entity's association definition.
;;;   - If the source cardinality is <= 1, the association can be implemented
;;;     by a sourceID (parentID) field in the target entity. (parent-child relationship)
;;;     * Because an extra column must be added to the target entity table,
;;;       this implementation should be used only for very few associations.
;;;   - If the target cardinality is <=1 , the association can be implemented
;;;     by a targetID field in the source entity. (Classification relationship)
;;;   * Hierarchical assocs
;;;     - Can be implemented by a cascading foreign key if the assoc is
;;;       is implemented as a field in the target table (composition)
;;;     * Automatic deletion is only possible if the target entity is not the target
;;;       of another association.
;;;     * If the children should not be automatically deleted, we call the association
;;;       a delegation. Delegations are implemented through separate tables.
;;;   
;;; * Hooks TBD
;;;   - before-create
;;;   - after-update
;;;
;;; * Table Constraints
;;;   - Table consistency is checked immediately (eg PRIMARY KEY and UNIQUE constraints)
;;;   - Cross-table consistency is checked on COMMIT (eg FOREIGN KEYS used to ensure association consistency)
;;;
;;; * Instance Identity
;;;   - We want a entity instance to be represented by a unique single object
;;;     even if it is retrieved via different associations.
;;;     => keep a entity_id to object hash
;;;   - The default PK, entity_id, is always used to determine instance uniqueness. 
;;; 
;;; * Lazy Loading
;;;   - References may be circular; lazy loading should prevent infinite loops
;;;     -> Load the association target only when it's accessed
;;;
;;; * Transactional behaviour is provided by a complementary interface. ?!
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating Entity Schemas
;;;
;;; - entity class hierarchies not supported
;;; - Circular dependencies should be supported
;;; - Adding a entity can entail 
;;;   + modifications of the existing entity tabdefs
;;;   + new methods on existing entity classes

;;; create-schema <entities>
;;; add-entity <schema> <entity>
;;; add-entities <schema> <entities>


(in-package :edm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entities, Fields and Associations

(defstruct entity
  package
  schema
  name
  options
  elements
  class-name
  table-name
  incoming-assocs)

(defparameter *reserved-field-names* '("parent_id" "entity_id"))

;; The *entities-ht* hashtable stores the defined entities (metadata)
;; It must not be deleted.
(defvar *entities-ht* (make-hash-table :test #'eq))

;; The name of an entity is unique within a schema.
;; The symbol <schema>.<entity-name> is used as the entity class name.
;; Entities are hashed by their class name.
(defun find-entity-for-class-name (name)
  (or (gethash name *entities-ht*)
      ;; Callers rely on find-entity-for-class-name returning non-null.
      (error "Unknown entity type ~a" name)))

(defun find-association-target-entity (assoc)
  (let ((entity-name
         (read-from-string (format () "~@:(~a.~a~)" (target-schema assoc) (target-entity assoc)))))
    (find-entity-for-class-name entity-name)))

(defun get-entities (&optional schema)
  (let ((entities ()))
    (maphash (lambda (k v)
               (declare (ignore k))
               (when (or (null schema)
                         (equal (entity-schema v) schema))
                 (push v entities)))
             *entities-ht*)
    (sort entities #'string< :key #'entity-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API 

(defmacro defentity (name-and-options &rest fields-and-assocs)
  (multiple-value-bind
        (name options)
      (if (symbolp name-and-options)
          (values name-and-options nil)
          (values (car name-and-options)
                  (cdr name-and-options)))
    (let* ((schema (cadr (assoc :schema options)))
           (class-name
            (make-entity-class-name (symbol-name name) schema))
           (entity
            (make-entity
             :package *package*
             :schema schema
             :name name
             :options options
             :elements (mapcar (lambda (e)
                                 (destructuring-bind (elt-type elt-name &rest options)
                                     e
                                   (make-element-descriptor elt-type elt-name options)))
                               fields-and-assocs)
             :class-name class-name
             :table-name class-name)))
      (loop
         :for (elem . rest) :on (entity-elements entity)
         :do (progn
               (when (member (name elem) rest :key #'name)
                 (error "Duplicate element ~a in ~a" (name elem) entity))
               (when (and (typep elem 'association)
                          (or (not (slot-boundp elem 'target-schema))
                              (null (target-schema elem))))
                 (setf (target-schema elem) schema))))
      `(progn
         (setf (gethash ',class-name *entities-ht*)
               ,entity)
         (ensure-tuple-class ,entity)))))

(defun entity-fields (entity)
  (remove-if-not (lambda (e) (typep e 'field)) (entity-elements entity)))
(defun entity-associations (entity)
  (remove-if (lambda (e) (typep e 'field)) (entity-elements entity)))

(defun entity-keys (entity)
  (loop
     :for option :in (entity-options entity)
     :when (eq (car option) :key)
     :collect (cdr option)))

(defun entity-key-field-p (entity field)
  ;; Checks if the fields appears in some key of the entity
  (let ((name (name field)))
    (loop
       :for option :in (entity-options entity)
       :when (and (eq (car option) :key)
                  (member name (caddr option)))
       :return (cadr option))))

;; A "singleton" is a :1 association. The left multiplicity is not restricted.
(defclass singleton () ())
;; A "polyton" is a :n association. The left multiplicity is not restricted.
(defclass polyton ()
  ((table-name :accessor table-name :initarg :table-name)))

(defclass element ()
  ((name :accessor name :initarg :name)
   (screen-name :accessor screen-name :initarg :screen-name :initform nil)))

(defmethod print-object ((thing element) (stream t))
  (format stream "[~a ~a]" (type-of thing) (name thing)))

(defclass field (element singleton)
  ((datatype :accessor datatype :initarg :type)
   (default-value :accessor default-value :initarg :default-value :initform nil)))

(defclass association (element)
  ;; Associations, like fields, are elements of a entity.
  ;; - The parent entity is the source of the association.
  ;; - The entity may have asssociations to many targets, and the target may be associated with many source entities
  ;; - The pair (entity entity, entity element name) uniquely identifies an association.
  ;; - The implementation of an association depends on
  ;;   - target cardinality
  ;;   - source cardinality
  ;;   - wether the target entity can be references from more than one source entity
  ((target-schema :accessor target-schema :initarg :target-schema)
   (target-entity :accessor target-entity :initarg :target-entity)))

(defclass composition (association) ())
(defclass composition-1 (composition singleton) ())
(defclass composition-n (composition polyton) ())

(defclass delegation (association) ())
(defclass delegation-1 (delegation singleton)
  ((table-name :accessor table-name :initarg :table-name)))
(defclass delegation-n (delegation polyton) ())

(defclass reference (association) ())
(defclass reference-1 (reference singleton)
  ((table-name :accessor table-name :initarg :table-name)))
(defclass reference-n (reference polyton) ())

(defclass secondarykey ()
  ((name :accessor name :initarg :name)
   (columns :accessor columns :initarg :columns)))

(defun make-element-descriptor (type name options)
  (check-identifier-syntax (symbol-name name))
  (let ((card (or (getf options :cardinality)
                  :one)))
    (remf options :cardinality)
    (ecase type
      (:field
       (ecase card
         (:one (apply #'make-instance 'field :name name options))))
      (:composition
       (ecase card
         (:one (apply #'make-instance 'composition-1 :name name options))
         (:many (apply #'make-instance 'composition-n :name name options))))
      (:delegation
       (ecase card
         (:one (apply #'make-instance 'delegation-1 :name name options))
         (:many (apply #'make-instance 'delegation-n :name name options))))
      (:reference
       (ecase card
         (:one (apply #'make-instance 'reference-1 :name name options))
         (:many (apply #'make-instance 'reference-n :name name options)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class model

(defclass entity-class (db-tuple)
  (
   ;; Slot name must conform underlying DB syntax rules! There is no mapping yet.
   (entity_id :reader entity_id :initarg :entity_id)
   ;; Housekeeping
   (lock-mode :accessor lock-mode :initarg :lock-mode :initform nil)
   (deleted :accessor deleted :initform nil)))

(defmethod make-entity-class-name ((name string) (schema string))
  (check-identifier-syntax name)
  (check-identifier-syntax schema)
  (read-from-string (concatenate 'string schema "."  name)))

(defmethod ensure-tuple-class ((entity entity))
  (let ((class-name (entity-class-name entity))
        (slotspecs (mapcar (lambda (f)
                             (typecase f
                               (field 
                                (list (name f)
                                      :initform nil
                                      :reader (slot-reader-name entity f)
                                      :writer (slot-writer-name entity f)))
                               (composition-1 
                                (list (name f)
                                      :initform nil
                                      :reader (slot-reader-name entity f)
                                      :writer (slot-writer-name entity f)))
                               (t 
                                (list (name f)
                                      :reader (slot-reader-name entity f)
                                      :writer (slot-writer-name entity f)))))
                           (entity-elements entity))))
    (ensure-tuple-class% 'entity-class nil class-name slotspecs)))

(defmethod slot-reader-name (entity (element element))
  (read-from-string (concatenate 'string (entity-schema entity) "." (symbol-name (name element)) "%")
          (entity-package entity)))
(defmethod slot-reader-name (entity (element symbol))
  (read-from-string (concatenate 'string (entity-schema entity) "." (symbol-name element) "%")
          (entity-package entity)))

(defmethod slot-writer-name (entity (element element))
  (read-from-string (concatenate 'string "set-" (entity-schema entity) "." (symbol-name (name element)) "%")
          (entity-package entity)))
(defmethod slot-writer-name (entity (element symbol))
  (read-from-string (concatenate 'string "set-" (entity-schema entity) "." (symbol-name element) "%")
          (entity-package entity)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Identifier syntax

(defvar *illegal-chars* ".-~ ")
(defun illegal-char-p (c)
  (position c *illegal-chars*))
(defun check-identifier-syntax (s)
  (when (some #'illegal-char-p s)
    (error "Name ~a must not contain any of the chars ~a" s *illegal-chars*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DB Schema Generation

(defmethod create-db-schema ((schema-name string))
  ;; Compiles the DB schema for a set of entities.
  ;;   Circular dependencies between the entities are allowed, but incremental
  ;;   schema generation is not possible. A schema can be updated though, without
  ;;   deleting existing instances.
  ;; Also creates a CLOS class for each entity.
  (let* ((entities (get-entities schema-name))
         (entity-tabdefs
          (mapcar (lambda (entity) (tabdef-raw-for-entity entity))
                  entities))
         (assoc-tabdefs ()))
    (dolist (entity entities)
      (dolist (assoc (entity-associations entity))
        (let ((target (find-association-target-entity assoc)))
          ;; Check if target usage is OK
          (check-association target assoc entity)
          ;; Record target usage
          (push assoc (entity-incoming-assocs target))
          (typecase assoc
            (composition
              ;; Remeber that the target entity is the target of a composition.
              ;; It cannot be used anywhere else.
              ;; Composition Entities:
              ;; - Add a back reference on the target 
              (add-parent-column assoc
                                 entity
                                 (find (entity-name target) entity-tabdefs :key #'tabdef-name :test #'string=)))
            (t
              (let ((table (make-association-table assoc entity)))
                (when table
                  (push table assoc-tabdefs))))))))
    (let ((schema
            (make-schema :name schema-name
                         :tables (append entity-tabdefs assoc-tabdefs))))
      (setf (gethash schema-name *schema-lib*)
              schema))))

(defmethod check-association :before (target assoc source)
  (format t "check: ~a as ~a of ~a~%" (entity-name target) (name assoc) (entity-name source)))
(defmethod check-association (target assoc source)
  (when (some (lambda (a) (typep a 'composition)) (entity-incoming-assocs target))
    (error "Inconsistent usage of target entity ~a in association ~a of entity ~a"
           (entity-name target)
           (name assoc)
           (entity-name source))))
(defmethod check-association (target (assoc composition) source)
  (unless (null (entity-incoming-assocs target))
    (error "Inconsistent usage of target entity ~a in association ~a of entity ~a"
           (entity-name target)
           (name assoc)
           (entity-name source))))
  
(defun tabdef-raw-for-entity (entity)
  ;; Create a preliminary table for storing instances of $entity.
  ;; The preliminary definition does not consider associations having $entity
  ;; as their target. The table definition may be updated to reflect usage of this
  ;; entity in associations from other entities.
  ;; Eg. if the entity is used in a composition, a parent_id column will be added later.
  (let* ((name
          (entity-name entity))
         (columns
          (mapcar (lambda (f) (make-field-column f entity))
                  (entity-fields entity)))
         (comp-cols
          (make-composition-columns entity))
         (foreign-keys
          (loop
             :for element :in (entity-associations entity)
             :for target = (find-association-target-entity element)
             :when (typep element 'composition-1)
             :collect (make-foreign-key :name (format () "fk_~a$~a$~a" name (name element) (entity-name target))
                                        :columns (list (name element))
                                        :referenced-table (format () "~a.~a" (entity-schema target) (entity-name target))
                                        :referenced-columns '("entity_id")
                                        :on-delete :restrict)))
         (constraints
          (list*
           (make-primary-key :name (format nil "pk_~a" name) :columns '("entity_id"))
           (loop
              :for key :in (entity-keys entity)
              :collect (make-unique-key :name (format nil "sk_~a$~a" name (first key))
                                        :columns (second key))))))
    (push (make-coldef :name "entity_id" :datatype 'uuid) columns)
    (create-tabdef :schema (entity-schema entity)
                   :name (entity-name entity)
                   :columns (append columns comp-cols)
                   :constraints (append constraints
                                        foreign-keys))))

(defun make-field-column (f entity)
  (let ((field-name (symbol-name (name f))))
    (when (member field-name *reserved-field-names* :test #'string-equal)
      (error "Entity ~a: Field name ~a is reserved. Field names are case-insensitive."
             (entity-name entity)
             field-name))
    (make-coldef :name field-name
                 :datatype (datatype f)
                 :default-value (default-value f))))

(defun make-composition-columns (entity)
  (loop
     :for assoc :in (entity-associations entity)
     :when (typep assoc 'composition-1)
     :collect (make-coldef :name (name assoc)
                           :datatype 'uuid)))

(defun add-parent-column (assoc entity target-tabdef &key (source-action :cascade))
  ;; The target entity is used in a composition in the source entity.
  ;; A entity type may be used in at most one composition.
  ;; Each child instance may be used in at most one parent instance. This is guaranteed by the
  ;; primary key of the target table (which is unique).
  ;; The association is implemented by a parent_id field in the target entity.
  ;; We use a foreign key to maintain consistency with the parent. The default on-delete action
  ;; is CASCADE, which  means the child is automatically delete when the parent is deleted.
  (let* ((schema (entity-schema entity))
         (source-name (symbol-name (entity-name entity)))
         (source-qname (concatenate 'string schema "." source-name))
         (target-entity (find-association-target-entity assoc)))
    (cond
      ((find "parent_id" (tabdef-columns target-tabdef) :key #'coldef-name :test #'string=)
       (error "Table ~a already has a PARENT_ID column" (target-entity assoc)))
      (t
       (push (make-instance 'field :name (read-from-string "parent_id") :type 'uuid)
             (entity-elements target-entity))
       (ensure-tuple-class target-entity)
       (push (make-coldef :name "parent_id" :datatype 'uuid)
             (tabdef-columns target-tabdef))
       (push (make-foreign-key :name (format nil "fk_~a$~a" source-name (name assoc))
                               :columns '("parent_id")
                               :referenced-table source-qname
                               :referenced-columns '("entity_id")
                               :on-delete source-action
                               :on-update :cascade)
             (tabdef-constraints target-tabdef))))))

;; Association table names are constructied by concatenating the source entity name
;; and the association name. This ensures unique table names.
;; Tables are currently only generated for reference associations.
;; Compositions are implemented without a separate table.
(defmethod make-association-table-name (entity (association polyton))
  (format nil "~a$mn$~a" (entity-name entity) (name association)))
(defmethod make-association-table-name (entity (association singleton))
  (format nil "~a$n1$~a" (entity-name entity) (name association)))

(defmethod make-association-table ((assoc composition) entity)
  ;; Compositions don't require an extra table
  (declare (ignorable entity))
  nil)

(defmethod make-association-table ((assoc delegation) entity)
  (make-association-table% assoc entity :cascade :restrict))

(defmethod make-association-table ((assoc reference) entity)
  (make-association-table% assoc entity :cascade :restrict))

(defun make-association-table% (assoc entity source-action target-action)
  (let* ((schema (entity-schema entity))
         (source-name (symbol-name (entity-name entity)))
         (target-name (symbol-name (target-entity assoc)))
         (source-qname (concatenate 'string schema "." source-name))
         (target-qname (concatenate 'string (target-schema assoc) "." target-name))
         (table-name (make-association-table-name entity assoc)))
    (setf (table-name assoc) (format () "~a.~a" schema table-name))
    (create-tabdef :schema schema
                   :name table-name
                   :columns (list (make-coldef :name "source_id" :datatype 'uuid)
                                  (make-coldef :name "target_id" :datatype 'uuid))
                   :constraints (list (make-primary-key :name (format nil "pk_~a$~a" source-name (name assoc))
                                                        :columns '("source_id" "target_id"))
                                      (make-foreign-key :name (format nil "fk_source$~a" table-name)
                                                        :columns '("source_id")
                                                        :referenced-table source-qname
                                                        :referenced-columns '("entity_id")
                                                        :on-delete source-action)
                                      (make-foreign-key :name (format nil "fk_target$~a" table-name)
                                                        :columns '("target_id")
                                                        :referenced-table target-qname
                                                        :referenced-columns '("entity_id")
                                                        :on-delete target-action)))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
