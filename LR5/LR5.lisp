;Реалізація 1 та 2 завдань

(defun read-csv (path hash-table key)
  (with-open-file (stream path :direction :input)
    (let ((header (uiop:split-string (read-line stream) :separator ",")))
      (loop for line = (read-line stream nil 'eof)
            until (eq line 'eof)
            do (let* ((fields (uiop:split-string line :separator ","))
                      (record (make-hash-table))
                      (id (parse-integer (first fields))))
                 (case key
                   (:article
                    (setf (gethash :id record) id
                          (gethash :specialty record) (string-trim '(#\Space #\Tab #\Newline #\Return) (second fields))
                          (gethash :article record) (string-trim '(#\Space #\Tab #\Newline #\Return) (third fields))))
                   (:specialty
                    (setf (gethash :id record) id
                          (gethash :specialty record) (string-trim '(#\Space #\Tab #\Newline #\Return) (second fields))))
                   (t (error "Unknown key: ~A" key)))
                 (setf (gethash id hash-table) record))))))

;Реалізація 3 завдання
(defun select (path key &rest filters)
  (lambda (&rest filters)
    (let ((result '())
          (hash-table (make-hash-table :test #'equal)))

      (read-csv path hash-table key)

      (if (null filters)
          (maphash (lambda (key value)
                     (push value result))
                   hash-table)
          
          (let* ((filter-hash (make-hash-table :test #'equal)))
            (loop for (filter-key filter-value) on filters by #'cddr
                  do (setf (gethash filter-key filter-hash) filter-value))

            (maphash (lambda (key value)
                       (let ((nested-hash value)
                             (matches t))
                         (maphash (lambda (filter-key filter-value)
                                    (let ((nested-value (gethash filter-key nested-hash)))
                                      (when (not (equal filter-value nested-value))
                                        (setf matches nil))))
                                  filter-hash)
                         (when matches
                           (push nested-hash result))))
                     hash-table)))
      (reverse result))))

;Реалізація 4 завдання
(defun write-to-csv (path hash-tables)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (let* ((keys (loop for k being the hash-keys of (first hash-tables) collect k))
           (header (mapcar #'string keys)))
      (format stream "~{~a~^,~}~%" header)
      (dolist (hash-table hash-tables)
        (let ((values (mapcar (lambda (key) 
                                (let ((value (gethash key hash-table)))
                                  (if value 
                                      (write-to-string value)
                                      "")))
                              keys)))
          (format stream "~{~a~^,~}~%" values))))))

;Реалізація 5 завдання
(defun hash-table-to-alist (hash-table)
  (let ((alist '()))
    (maphash (lambda (key value)
               (push (cons key value) alist))
             hash-table)
    (nreverse alist)))

;Реалізація 6 завдання
(defun print-hash-table (hash-tables)
  (let ((field (let ((keys '()))
                 (maphash (lambda (key value) (push key keys))
                          (first hash-tables))
                 (reverse keys))))
    (format t "~%")
    (format t "~{~15A~}" (mapcar #'symbol-name field))
    (format t "~%")
    (dolist (table hash-tables)
      (let ((values (mapcar (lambda (key) (gethash key table)) field)))
        (format t "~{~15A~}" values)
        (format t "~%")))))

;; Тестування 
(defun test-read-csv ()
  (format t "~%Tables from article.csv:~%")
  (print-hash-table (funcall (select "LR5/article.csv" :article)))

  (format t "~%Tables from specialty.csv:~%")
  (print-hash-table (funcall (select "LR5/specialty.csv" :specialty)))

  (format t "~%Article with id 4:~%")
  (print-hash-table (funcall (select "LR5/article.csv" :article) :id 4))
  
  (format t "~%Scientific articles for the specialty Biology:~%")
  (print-hash-table (funcall (select "LR5/article.csv" :article) :specialty "Biology"))
  )

(defun test-write-to-csv ()
  (format t "~%Data that will be written to the economics_article.csv file:~%")
  (print-hash-table (funcall (select "LR5/article.csv" :article) :specialty "Economics"))
  
  (write-to-csv "LR5/economics_article.csv" (funcall (select "LR5/article.csv" :article) :specialty "Economics"))
  
  (format t "~%Tables from economics_article.csv:~%")
  (print-hash-table (funcall (select "LR5/economics_article.csv" :article))))

(defun test-hash-table-to-alist ()
  (let* ((selected-articles (funcall (select "LR5/article.csv" :article) :id 8))
         (test-hash-table (first selected-articles))
         (expected-alist '((:id . 8)
                           (:specialty . "Mathematics")
                           (:article . "Innovations in Computational Algebra"))))
    (if test-hash-table
        (let ((generated-alist (hash-table-to-alist test-hash-table)))
          (if (equal expected-alist generated-alist)
              (format t "The result is correct: ~a~%" generated-alist)
              (format t "The result is not correct. ~%Generated: ~a~%Expected:  ~a~%" generated-alist expected-alist))))))


