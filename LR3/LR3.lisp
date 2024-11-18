 ;Конструктивний метод
(defun left-to-right (A L R k)
  "Рекурсивний прохід списку зліва направо, обмінюючи місцями елементи."
  (if (>= L R)
      (values k A)
      (let* ((current (nth L A))
             (next (nth (1+ L) A)))
        (if (>= current next)
            (left-to-right
             (append (subseq A 0 L)
                     (list next current)
                     (nthcdr (+ 2 L) A))
             (1+ L) R L)
            (left-to-right A (1+ L) R k)))))

(defun right-to-left (A R L k)
  "Рекурсивний прохід списку справа наліво, обмінюючи місцями елементи."
  (if (<= R L)
      (values k A)
      (let* ((prev (nth (1- R) A))
             (current (nth R A)))
        (if (>= prev current)
            (right-to-left
             (append (subseq A 0 (1- R))
                     (list current prev)
                     (nthcdr (1+ R) A))
             (1- R) L (1- R))
            (right-to-left A (1- R) L k)))))

(defun exchange4-rec (A L R)
  "Рекурсивний алгоритм сортування"
  (if (>= L R)
      A
      (multiple-value-bind (k new-A) (left-to-right A L R L)
        (multiple-value-bind (new-k new-A) (right-to-left new-A k L k)
          (exchange4-rec new-A (1+ new-k) k)))))

(defun exchange4-constructive (A)
  "Функція шейкерного сортування масива A за незменшенням з використанням конструктивного методу"
  (exchange4-rec  A 0 (1- (length A))))



(defun check-constructive (name input expected)
  "Execute `exchange4-constructive' on `input', compare result with `expected' and print
  comparison status."
  (format t "~:[FAILED~;PASSED~]... ~a~%"
          (equal (exchange4-constructive input) expected)
          name))

(defun test-exchange4-constructive ()
  (check-constructive "test 1.1" '(1 2 3 4 5) '(1 2 3 4 5) )
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
