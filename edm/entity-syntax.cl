;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    "Embedded SQL"
;;; Copyright      (c) Michael Kappert 2011
;;; License
;;; Last Modified  <michael 2019-12-14 15:41:10>

(in-package :edm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *sql-readtable* (copy-readtable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entitiess

(defun entity-reader (stream char)
  (unless (eql char #\{)
    (error "Expected '{', not ~a" char))
  `(build-entity ,@(read-delimited-list #\} stream t)))

(defmacro build-entity (class &rest column-values)
  `(make-instance ',class ,@column-values))

(defun sql-reader-error (stream char)
  (declare (ignore stream))
  (error "Dangling ~a" char))

(set-macro-character #\{ 'entity-reader nil *sql-readtable*)
(set-macro-character #\} 'sql-reader-error nil *sql-readtable*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tuples

(defun sql-tuple-reader (stream char)
  (unless (eql char #\[)
    (error "Expected '[', not ~a" char))
  `(build-tuple ,@(read-delimited-list #\] stream t)))

(defmacro build-tuple (&rest elements)
  `(make-sql-tuple :elements (list ,@elements)))

(set-macro-character #\[ 'sql-tuple-reader nil *sql-readtable*)
(set-macro-character #\] 'sql-reader-error nil *sql-readtable*)

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
