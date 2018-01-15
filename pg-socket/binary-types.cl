;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author         Michael Kappert
;;; Created        2010-10-12 22:16:25 22:16:25
;;; Last Modified  <michael 2018-01-14 17:57:21>
;;; Description    Binary Types inspired by Practical Common Lisp
;;;                Does not suppert strings with embedded NULs
  
(in-package :pg-socket)

(declaim (optimize (speed 3) (safety 1) (space 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defconstant +max-raw-array-size+ (1- (min (expt 2 21) (truncate array-total-size-limit 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Defining binary types
 
(defmacro define-binary-type (name params &rest field-defs)
  ;; field-defs have the form (field-name field-type statements)
  ;; statements may reference and params or field-names.
  `(progn
     (defstruct ,name ,@(mapcar #'car field-defs))
     (defun ,(reader name) (stream ,@params)
       (let ((bytes-read 0)
             ,@(mapcar #'car field-defs))
         ,@(mapcar (lambda (fd) (let-assignment-form fd))
                   field-defs)
         (values
          (,(constructor name)
            ,@(slot-assignment-forms field-defs))
          bytes-read)))
     (defun ,(writer name) (stream value)
       ,@(mapcar (lambda (fd)
                   (write-form (access-form name (car fd)) (cadr fd)))
                 field-defs))))


(defmacro define-out-message (name params &rest field-defs)
  ;; field-defs have the form (field-name field-type field-value)
  `(progn
     (defstruct ,name ,@(mapcar #'car field-defs))
     (defun ,(creator name) ,params
       (,(constructor name)
         ,@(slot-init-forms field-defs)))
     (defun ,(writer name) (stream value)
       ,@(mapcar (lambda (fd)
                   (write-form (access-form name (car fd)) (cadr fd)))
                 field-defs))))

(defmacro define-variant-type (name parames field-descriptions dispatcher)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation
 
(defun normalize-typedef (typedef)
  (if (atom typedef)
    (list typedef)
    typedef))

(defun creator (name)
  (intern (format nil "CREATE-~s" name)))

(defun constructor (name)
  (intern (format nil "MAKE-~s" name)))
  
(defun reader (name)
  (intern (format nil "READ-~s" name)))
 
(defun writer (name)
  (intern (format nil "WRITE-~s" name)))

(defun read-form (typedef)
  (typecase typedef
    (atom
      (case typedef
        (:nop
          '(values nil 0))
        (t
          `(,(intern (format nil "READ-~s" typedef)) stream))))
    (cons
      (case (car typedef)
        (:raw
          `(read-byte-array stream ,@(cdr typedef)))
        (:raw-string
          `(map 'vector #'code-char (read-byte-array stream ,@(cdr typedef))))
        (:utf-8-string
          `(read-utf-8-string stream ,@(cdr typedef)))
        (:utf-8-array
          `(read-utf-8-array stream ,@(cdr typedef)))
        (:iso-8859-1-string
          `(read-iso-8859-1-string stream ,@(cdr typedef)))
        (:unsigned-byte
         (destructuring-bind (type &key length byte-order)
             typedef
           (ecase length
             (1
               `(values (read-byte stream) 1))
             (2
              `(read-ub2 stream :byte-order ,byte-order))
             (4
              `(read-ub4 stream :byte-order ,byte-order))
             (t
              `(read-unsigned-byte stream  ,@(cdr typedef))))))
        (:case
            `(case ,(cadr typedef)
               ,@(mapcar #'case-read-form (cddr typedef))
               (otherwise
                (values nil 0))))
        (:ecase
            `(ecase ,(cadr typedef)
               ,@(mapcar #'case-read-form (cddr typedef))
               (otherwise
                (error "Invalid binary case ~a" ,(cadr typedef)))))
        (:sequence
         (destructuring-bind (type &key (byte-length nil) (length nil))
             (cdr typedef)
           (cond 
             (length
              `(loop
                  :with bytes-read = 0
                  :for k :below ,length
                  :for (_value _bytes-read) = (multiple-value-list ,(read-form type))
                  :do (incf bytes-read _bytes-read)
                  :collect _value :into result
                  :finally (return (values result bytes-read ))))
             (byte-length
              `(loop
                  :with bytes-read = 0
                  :while (< bytes-read ,byte-length)
                  :for (_value _bytes-read) = (multiple-value-list ,(read-form type))
                  :do (incf bytes-read _bytes-read)
                  :collect _value :into result
                  :finally (return (values result bytes-read ))))
             (t
              `(loop
                  :with bytes-read = 0
                  :for (_value _bytes-read) = (multiple-value-list ,(read-form type))
                  :while _value
                  :do (incf bytes-read _bytes-read)
                  :collect _value :into result
                  :finally (return (values result bytes-read )))))))
        (otherwise
         `(,(intern (format nil "READ-~s" (car typedef))) stream ,@(cdr typedef)))))))
 
(defun write-form (access-form typedef)
  (let ((typedef (normalize-typedef typedef)))
    (case (car typedef)
      (:raw
        `(write-byte-array stream ,access-form ,@(cdr typedef)))
      (:raw-string
        `(map 'vector #'code-char (write-byte-array stream ,access-form ,@(cdr typedef))))
      (:utf-8-string
        `(write-utf-8-string stream ,access-form ,@(cdr typedef)))
      (:iso-8859-1-string
        `(write-iso-8859-1-string stream ,access-form ,@(cdr typedef)))
      (:unsigned-byte
        `(write-unsigned-byte stream ,access-form ,@(cdr typedef)))
      (:case
          `(case ,(cadr typedef)
             ,@(mapcar (lambda (td) (case-write-form access-form td)) (cddr typedef))
             (otherwise
              (values nil 0))))
      (:ecase
          `(ecase ,(cadr typedef)
             ,@(mapcar (lambda (td) (case-write-form access-form td)) (cddr typedef))
             (otherwise
              (error "Invalid binary case ~a" ,(cadr typedef)))))
      (:sequence
       (destructuring-bind (type &key (length nil) (byte-length nil) (padbyte nil))
            (cdr typedef)
          `(progn (dolist (v ,access-form)
                    ,(write-form 'v type))
                  (write-byte 0 stream))))
      (otherwise
        `(,(intern (format nil "WRITE-~s" (car typedef))) stream ,access-form ,@(cdr typedef))))))
 
(defun case-read-form (casedef)
  `(,(car casedef) ,(read-form (cadr casedef))))
 
(defun case-write-form (access-form casedef)
  `(,(car casedef) ,(write-form access-form (cadr casedef))))
 
(defun access-form (name field &optional (value-var 'value))
  `(,(intern (format nil "~s-~s" name field)) ,value-var))
 
(defun let-assignment-form (field-def)
  `(multiple-value-bind (_value _bytes-read)
        ,(read-form (cadr field-def))
     (setf ,(car field-def) _value)
     (incf bytes-read _bytes-read)
     ,@(cddr field-def)))
 
(defun slot-assignment-forms (field-defs)
  (loop
     :for (name . rest) :in field-defs
     :append `(,(intern (symbol-name name) "KEYWORD") ,name)))

(defun slot-init-forms (field-defs)
  (loop
     :for (name type value) :in field-defs
     :append  `(,(intern (symbol-name name) "KEYWORD") ,value)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The atomic types
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; unsigned-byte (length, size, byte-order)

(defmacro read-8bb (stream &rest args)
  `(the (unsigned-byte 8)
     (read-byte ,stream ,@args)))

(defun read-ub4 (stream &key (byte-order :BE))
  (let ((ub 0))
    (declare ((unsigned-byte 32) ub))
    (ecase byte-order
      (:be
       (setf (ldb (byte 8 24) ub) (read-8bb stream t))
       (setf (ldb (byte 8 16) ub) (read-8bb stream t))
       (setf (ldb (byte 8 8) ub) (read-8bb stream t))
       (setf (ldb (byte 8 0) ub) (read-8bb stream t)))
      (:le
       (setf (ldb (byte 8 0) ub) (read-8bb stream t))
       (setf (ldb (byte 8 8) ub) (read-8bb stream t))
       (setf (ldb (byte 8 16) ub) (read-8bb stream t))
       (setf (ldb (byte 8 24) ub) (read-8bb stream t))))
    (values ub 4)))

(defun read-ub2 (stream &key (byte-order :BE))
  (let ((ub 0))
    (declare ((unsigned-byte 16) ub))
    (ecase byte-order
      (:be
       (setf (ldb (byte 8 8) ub) (read-8bb stream t))
       (setf (ldb (byte 8 0) ub) (read-8bb stream t)))
      (:le
       (setf (ldb (byte 8 0) ub) (read-8bb stream t))
       (setf (ldb (byte 8 8) ub) (read-8bb stream t))))
    (values ub 2)))

(defun read-unsigned-byte (stream &key (length 1) (size 8) (byte-order :BE))
  (declare ((integer 0 32) length)
           ((integer 0 8) size))
  (let ((ub 0))
    (declare ((unsigned-byte 32) ub))
    (ecase byte-order
      (:BE
        (loop
           :for k :below length
           :for position = (* (1- length) size) :then (- position size)
           :for b = (read-byte stream t)
           :while b
           :do (setf (ldb (byte 8 position) ub) b)))
      (:LE
        (loop
           :for k :below length
           :for position = 0 :then (+ position size)
           :for b = (read-byte stream t)
           :while b
           :do (setf (ldb (byte 8 position) ub) b))))
    (values ub
            length)))

(defun write-unsigned-byte (stream value &key (length 1) (byte-order :BE))
  (declare ((unsigned-byte 4) length)
           ((unsigned-byte 32) value))
  (ecase byte-order
    (:BE
      (loop
         :for k :below length
         :for p :downfrom (* (1- length) 8) :by 8
         :do (write-byte (ldb (byte 8 p) value) stream)))
    (:LE
      (loop
         :for k :below length
         :do (write-byte (ldb (byte 8 (* k 8)) value) stream)))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; byte-array (length)
 
(defun read-byte-array (stream &key (length nil))
  (assert (<= length +max-raw-array-size+))
  (let* ((byte-array
          ;; element-type T conses much less than (unsigned-byte 8) (at least in SBCL)
          (make-array length :adjustable nil :element-type t))
         (bytes-read
           (read-sequence byte-array stream)))
    (values byte-array bytes-read)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; utf-8 string (termination) &kex (nul-is-overlong nil)


(defun read-utf-8-array (stream &key byte-length)
  (declare ((unsigned-byte 32) byte-length))
  (multiple-value-bind (num-chars chars num-bytes)
      (loop
         :with bytes-read fixnum = 0 ; count the ZERO byte that terminates the string
         :for num-chars fixnum :from 0
         :while (<  bytes-read byte-length)
         :for byte = (the (unsigned-byte 8) (read-byte stream nil nil))
         :collect (let ((code 0))
                    (declare ((unsigned-byte 64) code))
                    (macrolet ((read-cont-byte ()
                                 `(let ((b (read-byte stream)))
                                    (declare ((unsigned-byte 8) b))
                                    (unless (= (ldb (byte 2 6) b) 2)
                                      (error "Invalid UTF-8 continuation byte ~d" b))
                                    b))
                               (store-cont-byte (pos)
                                 `(setf (ldb (byte 6 (the (unsigned-byte 5) pos)) code)
                                        (ldb (byte 6 0) (the (unsigned-byte 8) (read-cont-byte))))))
                      (cond
                        ((= (ldb (byte 1 7) byte) 0)
                         (incf bytes-read)
                         (setf code byte))
                        ((= (ldb (byte 3 5) byte) 6)
                         (incf bytes-read 2)
                         (setf (ldb (byte 5 6) code) (ldb (byte 5 0) byte))
                         (setf (ldb (byte 6 0) code) (ldb (byte 6 0) (read-cont-byte))))
                        ((= (ldb (byte 4 4) byte) 14)
                         (incf bytes-read 3)
                         (setf (ldb (byte 4 12) code) (ldb (byte 4 0) byte))
                         (store-cont-byte 6)
                         (store-cont-byte 0))
                        ((= (ldb (byte 5 3) byte) 30)
                         (incf bytes-read 4)
                         (setf (ldb (byte 3 18) code) (ldb (byte 3 0) byte))
                         (store-cont-byte 12)
                         (store-cont-byte 6)
                         (store-cont-byte 0))
                        ((= (ldb (byte 6 2) byte) 62)
                         (incf bytes-read 5)
                         (setf (ldb (byte 2 24) code) (ldb (byte 2 0) byte))
                         (store-cont-byte 18)
                         (store-cont-byte 12)
                         (store-cont-byte 6)
                         (store-cont-byte 0))
                        ((= (ldb (byte 7 1) byte) 126)
                         (incf bytes-read 6)
                         (setf (ldb (byte 1 32) code) (ldb (byte 1 0) byte))
                         (store-cont-byte 24)
                         (store-cont-byte 18)
                         (store-cont-byte 12)
                         (store-cont-byte 6)
                         (store-cont-byte 0))
                        (t
                         (error "Invalid UTF-8 start byte ~d" byte))))
                    (code-char code)) :into chars
         :finally (return (values num-chars
                                  chars
                                  (1+ bytes-read))))
    (let ((result (make-string num-chars)))
      (loop
         :for c :in chars
         :for i :from 0
         :do (setf (aref result i) c))
      (values result num-bytes))))

(defun read-utf-8-string (stream)
  (multiple-value-bind (chars num-chars num-bytes)
      (loop
         :with bytes-read of-type (unsigned-byte 32) = 1 ; count the ZERO byte that terminates the string
         :for num-chars :from 0
         :for byte = (the (unsigned-byte 8) (read-byte stream nil nil))
         :while (and (numberp byte) (> byte 0))
         :collect (let ((code 0))
                    (declare ((unsigned-byte 64) code))
                    (labels ((read-cont-byte (stream)
                               (let ((b (read-byte stream)))
                                 (declare ((unsigned-byte 8) b))
                                 (unless (= (ldb (byte 2 6) b) 2)
                                   (error "Invalid UTF-8 continuation byte ~d" b))
                                 b))
                             (store-cont-byte (pos)
                               (setf (ldb (byte 6 pos) code) (ldb (byte 6 0) (read-cont-byte stream)))))
                      (cond
                        ((= (ldb (byte 1 7) byte) 0)
                          (incf bytes-read)
                          (setf code byte))
                        ((= (ldb (byte 3 5) byte) 6)
                          (incf bytes-read 2)
                          (setf (ldb (byte 5 6) code) (ldb (byte 5 0) byte))
                          (store-cont-byte 0))
                        ((= (ldb (byte 4 4) byte) 14)
                          (incf bytes-read 3)
                          (setf (ldb (byte 4 12) code) (ldb (byte 4 0) byte))
                          (store-cont-byte 6)
                          (store-cont-byte 0))
                        ((= (ldb (byte 5 3) byte) 30)
                          (incf bytes-read 4)
                          (setf (ldb (byte 3 18) code) (ldb (byte 3 0) byte))
                          (store-cont-byte 12)
                          (store-cont-byte 6)
                          (store-cont-byte 0))
                        ((= (ldb (byte 6 2) byte) 62)
                          (incf bytes-read 5)
                          (setf (ldb (byte 2 24) code) (ldb (byte 2 0) byte))
                          (store-cont-byte 18)
                          (store-cont-byte 12)
                          (store-cont-byte 6)
                          (store-cont-byte 0))
                        ((= (ldb (byte 7 1) byte) 126)
                          (incf bytes-read 6)
                          (setf (ldb (byte 1 32) code) (ldb (byte 1 0) byte))
                          (store-cont-byte 24)
                          (store-cont-byte 18)
                          (store-cont-byte 12)
                          (store-cont-byte 6)
                          (store-cont-byte 0))
                        (t
                          (error "Invalid UTF-8 start byte ~d" byte))))
                    (code-char code)) :into chars
         :finally (return (values chars
                                  num-chars
                                  bytes-read)))
    (let* ((string (make-string num-chars)))
      (loop
         for c in chars
         for i from 0
         do (setf (aref string i) c))
      (values string num-bytes))))

(defun write-utf-8-string (stream string)
  (loop
     :for char :across string
     :for code = (char-code char)
     :do (cond
           ((< code #x80)
             (write-byte code stream))
           ((< code #x800)
             (write-byte (dpb (ldb (byte 5 6) code) (byte 5 0) #b11000000)
                         stream)
             (write-byte (dpb (ldb (byte 6 0) code) (byte 6 0) #b10000000)
                         stream))
           ((< code #x10000)
             (write-byte (dpb (ldb (byte 4 12) code) (byte 4 0) #b11100000)
                         stream)
             (write-byte (dpb (ldb (byte 6 6) code) (byte 6 0) #b10000000)
                         stream)
             (write-byte (dpb (ldb (byte 6 0) code) (byte 6 0) #b10000000)
                         stream))
           ((< code #x200000)
             (write-byte (dpb (ldb (byte 3 18) code) (byte 3 0) #b11110000)
                         stream)
             (write-byte (dpb (ldb (byte 6 12) code) (byte 6 0) #b1000000)
                         stream)
             (write-byte (dpb (ldb (byte 6 6) code) (byte 6 08) #b10000000)
                         stream)
             (write-byte (dpb (ldb (byte 6 0) code) (byte 6 0) #b10000000)
                         stream))
           ((< code #x4000000)
             (write-byte (dpb (ldb (byte 2 24) code) (byte 2 0) #b11111000)
                         stream)
             (write-byte (dpb (ldb (byte 6 18) code) (byte 6 0) #b10000000)
                         stream)
             (write-byte (dpb (ldb (byte 6 12) code) (byte 6 0) #b1000000)
                         stream)
             (write-byte (dpb (ldb (byte 6 6) code) (byte 6 0) #b10000000)
                         stream)
             (write-byte (dpb (ldb (byte 6 0) code) (byte 6 0) #b10000000)
                         stream))
           ((< code #x80000000)
             (write-byte (dpb (ldb (byte 1 32) code) (byte 1 0) #b11111100)
                         stream)
             (write-byte (dpb (ldb (byte 6 24) code) (byte 6 0) #b10000000)
                         stream)
             (write-byte (dpb (ldb (byte 6 18) code) (byte 6 0) #b10000000)
                         stream)
             (write-byte (dpb (ldb (byte 6 12) code) (byte 6 0) #b1000000)
                         stream)
             (write-byte (dpb (ldb (byte 6 6) code) (byte 6 0) #b10000000)
                         stream)
             (write-byte (dpb (ldb (byte 6 0) code) (byte 6 0) #b10000000)
                         stream))
           (t
            (error "Invalid UTF-8 code point ~d (~c)" code char))))
  (write-byte 0 stream))
 
(defun utf-8-string-byte-length (string)
  (loop
     :for char :across string
     :for code = (char-code char)
     :sum (cond
            ((< code #x80)
              1)
            ((< code #x800)
              2)
            ((< code #x10000)
              3)
            ((< code #x200000)
              4)
            ((< code #x4000000)
              5)
            ((< code #x80000000)
              6)
            (t
              (error "Invalid UTF-8 code point ~d (~c)" code char)))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; iso-8859-1-string (length)
 
(defun read-iso-8859-1-string (stream &key (length nil))
  (let ((string))
    (cond
      ((null length)
        (setf string (make-array 0 :adjustable t :element-type 'character :fill-pointer 0))
        (loop
           ;; ISO 8859-1 is an eight-bit character code
           :for b = (read-byte stream nil nil)
           :while (and b (not (eql b 0)))
           :do (vector-push-extend (code-char b) string)))
      (T
        (setf string (make-string length))
        (dotimes (i length)
          (setf (char string i)
                  (code-char (read-byte stream))))))
    (values string
            (length string))))
 
(defun write-iso-8859-1-string (stream value &key (length nil))
  (loop
     :for c :across value
     ;; ISO 8859-1 is an eight-bit character code
     :do (write-byte (char-code c) stream))
  (when (null length)
    (write-byte 0 stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
(defun write-byte% (byte stream)
  (log2:info "--> ~d" byte)
  (write-byte byte stream)) 

(defun read-byte% (stream &optional (eof-error-p t) (eof-value nil))
  (let ((byte (read-byte stream eof-error-p eof-value)))
    (log2:info "<-- ~d" byte)
    byte))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
