(defun left-to-right (lst)
  "Рекурсивний прохід зліва направо"
  (if (null (cdr lst))
      lst
      (let ((current (car lst))
            (next (cadr lst)))
        (if (> current next)
            (cons next (left-to-right (cons current (cddr lst))))
            (cons current (left-to-right (cdr lst)))))))

(defun right-to-left (lst)
  "Рекурсивний прохід справа наліво"
  (if (null (cdr lst))
      lst
      (let ((prev (car lst))
            (next (cadr lst)))
        (if (> prev next)
            (cons next (right-to-left (cons prev (cddr lst))))
            (cons prev (right-to-left (cdr lst)))))))

(defun exchange4-rec (lst L R)
  "Рекурсивний алгоритм сортування"
  (if (>= L R)
      lst
      (let ((new-lst (left-to-right lst)))
        (exchange4-rec (right-to-left new-lst) (1+ L) (1- R)))))

(defun exchange4-constructive (lst)
  "Функція шейкерного сортування масива A за незменшенням з використанням конструктивного методу"
  (exchange4-rec lst 0 (1- (length lst))))




(defun check-constructive (name input expected)
  "Execute `exchange4-constructive' on `input', compare result with `expected' and print
  comparison status."
  (format t "~:[FAILED~;PASSED~]... ~a~%"
          (equal (exchange4-constructive input) expected)
          name))

(defun test-exchange4-constructive ()
  (check-constructive "test 1.1" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-constructive "test 1.2" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-constructive "test 1.3" '(5 5 4 4 3) '(3 4 4 5 5))
  (check-constructive "test 1.4" '(-6 3 2 5 1 4 3 ) '(-6 1 2 3 3 4 5 )))



;Імперативний Метод
(defun exchange4-imperative (A)
  "Функція шейкерного сортування масива A за незменшенням з використанням імперативного методу"
  (let* ((A-copy (copy-list A))  ; Створюємо копію списку
         (N (length A-copy))     ; Визначаємо довжину копії
         (L 0)
         (R (- N 1))
         (k 0))
    (loop while (< L R) do
          ;; Перший цикл: пересуваємо максимальні елементи вправо
          (loop for i from L below R do
                (when (>= (nth i A-copy) (nth (+ i 1) A-copy))
                  (rotatef (nth i A-copy) (nth (+ i 1) A-copy))
                  (setf k i)))
          (setf R k)
          
          ;; Другий цикл: пересуваємо мінімальні елементи вліво
          (loop for i from (- R 1) downto L do
                (when (>= (nth i A-copy) (nth (+ i 1) A-copy))
                  (rotatef (nth i A-copy) (nth (+ i 1) A-copy))
                  (setf k i)))
          (setf L (+ k 1)))
    A-copy))  ; Повертаємо відсортовану копію списку

(defun check-imperative (name input expected)
  "Execute `exchange4-imperative' on `input', compare result with `expected' and print
  comparison status."
  (format t "~:[FAILED~;PASSED~]... ~a~%"
          (equal (exchange4-imperative input) expected)
          name))

(defun test-exchange4-imperative ()
  (check-imperative "test 2.1" '(1 2 3 4 5) '(1 2 3 4 5) )
  (check-imperative "test 2.2" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-imperative "test 2.3" '(5 5 4 4 3) '(3 4 4 5 5))
  (check-imperative "test 2.4" '( 3 2 -6 5 1 4 3 ) '(-6 1 2 3 3 4 5)))
