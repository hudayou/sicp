;; divison 1:
;; '(list
;;   #(Hu 5000 Changning)
;;   #(Bi 3000 Minghang)
;;   #(Li 2000 Xuhui))
;; divison 2:
;; '#(vector
;;    (Zhang 1000 Xian)
;;    (Wang 2000 Shengzheng)
;;    (Lin 3000 Beijing))

(define (get-record file employee )
  (get-record-generic 'get-record employee file))

;; What's different in the personal file needs to be specified.
;; In this case, it's the file type.
(define (get-record-generic op employee file)
  (let ((type (file-type file)))
    (let ((proc (get op tag)))
      (if proc
        (apply proc (list employee (file-contents file)))
        (error
          "file type not recognized"
          type)))))

;; What's different in the personal record needs to be specified.
;; In this case, it's the record type.
(define (get-salary record)
  (get-salary-generic 'get-salary record))

(define (get-salary-generic op record)
  (let ((type (record-type record)))
    (let ((proc (get op tag)))
      (if proc
        (apply proc (list (record-contents record)))
        (error
          "record type not recognized"
          type)))))

;; type information of record and files needs to be able
;; to be fetched by the same type-tag procedure.
;; implies same information structure

(define (get-salary record)
  (get-data-generic 'get-salary record))

(define (get-record file employee)
  (get-data-generic 'get-record file employee))

(define (get-data-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "no method for these types -- get-data-generic"
          (list op type-tags))))))

(define (find-employee-record employee list-of-files)
  (if (null? list-of-files)
    #f
    (let ((file (car list-of-files)))
      (let ((record (get-record file employee)))
        (if record
          record
          (find-employee-record employee (cdr list-of-files)))))))

;; When a new company has been taken over, they need to install
;; new data type and new data type handler to the generic op
;; table. They also need to wrap their information to have file
;; type and record type specified.
