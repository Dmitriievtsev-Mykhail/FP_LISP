;Реалізація першої функції
(defun reverse-and-nest-tail (lst &optional res)
  (if (null lst)
      res
      (reverse-and-nest-tail
       (cdr lst)
       (cons (car lst) (and res (list res))))))

;Реалізація другої функції
(defun compress-list (lst)
  (when lst
    (let ((count (count-reps (car lst) (cdr lst) 1)))
      (cons (list count (car lst)) (compress-list (nthcdr count lst))))))

(defun count-reps (element lst count)
  (if (and lst (eql element (car lst)))
      (count-reps element (cdr lst) (+ count 1))
      count))


;Функції тестування
(defun check-reverse (name input expected)
  "Execute `reverse-and-nest-tail' on `input', compare result with `expected' and print
comparison status"
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (reverse-and-nest-tail input) expected)
          name))

(defun check-compress (name input expected)
  "Execute `compress-list' on `input', compare result with `expected' and print
  comparison status."
  (format t "~:[FAILED~;PASSED~]... ~a~%"
          (equal (compress-list input) expected)
          name))

;Тестові набори
(defun test-reverse-and-nest-tail ()
  (check-reverse "test 1.1" '(a b c) '(C (B (A))))
  (check-reverse "test 1.2" '()      '())
  (check-reverse "test 1.3" '(1 2 3) '(3 (2 (1))))
  (check-reverse "test 1.4" '(nil 2 3)  '(3 (2 (nil)))) )

(defun test-compress-list ()
  (check-compress "test 2.1" '(1 a a 3 3 3 b) '((1 1) (2 A) (3 3) (1 B)))
  (check-compress "test 2.2" '(1 a a 1 1 a a) '((1 1) (2 A) (2 1) (2 A)))
  (check-compress "test 2.3" '() nil)
  (check-compress "test 2.4" '(1) '((1 1))))


