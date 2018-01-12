
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(time (multiple-value-list 
        (hdb-odbc:with-open-connection 
            (c :user "" :password "" :server "ld9156" :port "30015")
          (with-connection (c)
            (%create-table (make-tabdef :name "XXX" :columns (list (make-coldef :name "C1" :datatype "varchar(100)"))))))))

(time (multiple-value-list 
        (hdb-odbc:with-open-connection 
            (c :user "" :password "" :server "ld9156" :port "30015")
          (with-connection (c)
            (?insert (?t "foo") :into 'xxx)))))

(time (multiple-value-list 
        (hdb-odbc:with-open-connection 
            (c :user "" :password "" :server "ld9156" :port "30015")
          (with-connection (c)
            (?select '* :from 'xxx)))))

(multiple-value-list
  (hdb-odbc:with-open-connection
      (c :user "" :password "" :server "ld9156" :port "30015")
    (sql-exec c "delete from a")))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
