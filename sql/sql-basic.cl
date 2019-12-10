;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   
;;; Copyright      (c) Michael Kappert 2011
;;; Last Modified  <michael 2019-12-10 21:50:54>

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

(defgeneric map-db-type (connection type))

(defgeneric ensure-tuple-class (spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UUIDs

#+:unix
(defun create-uuid ()
  (with-open-file (f "/proc/sys/kernel/random/uuid")
    (values (read-line f nil nil))))

#+:windows
(defun create-uuid ()
  (let ((uuid (uuid-generate))
        (s (make-array 36 :element-type 'character :initial-element #\-)))
    (flet ((update (index offset)
             (let ((j (+ (* index 2) offset)))
               (setf (aref s j) (hex (ldb (byte 4 4) (aref uuid index))))
               (setf (aref s (1+ j)) (hex (ldb (byte 4 0) (aref uuid index)))))))
      (loop :for k :from 0 :to 3 :do (update k 0))
      (loop :for k :from 4 :to 5 :do (update k 1))
      (loop :for k :from 6 :to 7 :do (update k 2))
      (loop :for k :from 8 :to 9 :do (update k 3))
      (loop :for k :from 10 :to 15 :do (update k 4)))
    s))

#+:windows
(defun hex (n)
  (if (< n 10)
      (code-char (+ n 48))
      (code-char (+ n 87))))

#+:windows
(defun uuid-generate ()
  (let ((uuid (make-array 16)))
    (loop
       :for k :below 16
       :do (setf (aref uuid k) (random 256)))
    uuid))

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
