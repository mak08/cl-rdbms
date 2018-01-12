;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael 2014
;;; Last Modified <michael 2018-01-12 00:26:28>

(in-package :sql)

(defmacro !{} (connection expression stream &key (separator ", "))
  `(loop
      :for (head . rest) :on ,expression
      :do (serialize-for-connection ,connection head ,stream)
      :when rest :do (format ,stream "~a" ,separator)))

(defmacro @[] (connection expression stream &key (prefix "") (suffix ""))
  `(when ,expression
     (format stream "~a" ,prefix)
     (serialize-for-connection ,connection ,expression ,stream)
     (format stream "~a" ,suffix)))

(defmacro ?[] (connection expression true-value false-value stream &key (prefix "") (suffix ""))
  `(progn
     (format stream "~a" ,prefix)
     (if ,expression
         (serialize-for-connection ,connection ,true-value ,stream)
         (serialize-for-connection ,connection ,false-value ,stream))
     (format stream "~a" ,suffix)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
