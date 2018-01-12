;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   
;;; Copyright      (c) Michael Kappert 2011
;;; Last Modified  <michael 2018-01-12 00:26:34>

(in-package :sql)

(defparameter *table_name_length* 63)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data Types

(defstruct dbinfo name owner)
(defstruct userinfo name password)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DB Connections - dynamic scoping
;;    http://www.postgresql.org/docs/8.4/static/libpq-connect.html


(defvar *current-connection*)

(defmacro with-connection ((connection) &body forms)
  "Use this connection for SQL commands in the dynamic scope"
  `(progn
     (let ((*current-connection* ,connection))
       (declare (special *current-connection*))
       ,@forms)))
 

(defgeneric ensure-tuple-class (spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; String operations

(defun lcat (strings)
  (let ((s (make-array (+ -1
                          (length strings)
                          (loop for x in strings sum (length x)))
                       :element-type 'character
                       :initial-element #\-)))
    (loop
       for x in strings
       with i = 0
       with j = 0
       do (progn (shiftf i j (+ j (length x) 1))
                 (setf (subseq s i j) x)))
    s))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
