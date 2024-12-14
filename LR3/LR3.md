<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт до лабораторної роботи 3</b><br/>
"Конструктивний і деструктивний підходи до роботи зі списками""<br/>
дисципліни "Вступ до функціонального програмування"
</p>

<p align="right"> 
<b>Студент</b>: 
 Дмитрієвцев Михаїл Валерійович КВ-12</p>

<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
Реалізуйте алгоритм сортування чисел у списку двома способами: функціонально і
імперативно.
1. Функціональний варіант реалізації має базуватись на використанні рекурсії і
конструюванні нових списків щоразу, коли необхідно виконати зміну вхідного списку.
Не допускається використання: псевдо-функцій, деструктивних операцій, циклів,
функцій вищого порядку або функцій для роботи зі списками/послідовностями, що
використовуються як функції вищого порядку. Також реалізована функція не має
бути функціоналом (тобто приймати на вхід функції в якості аргументів).
2. Імперативний варіант реалізації має базуватись на використанні циклів і
деструктивних функцій (псевдофункцій). Не допускається використання функцій
вищого порядку або функцій для роботи зі списками/послідовностями, що
використовуються як функції вищого порядку. Тим не менш, оригінальний список
цей варіант реалізації також не має змінювати, тому перед виконанням
деструктивних змін варто застосувати функцію copy-list (в разі необхідності).
Також реалізована функція не має бути функціоналом (тобто приймати на вхід
функції в якості аргументів).

Алгоритм, який необхідно реалізувати, задається варіантом (п. 3.1.1). Зміст і шаблон звіту
наведені в п. 3.2.

Кожна реалізована функція має бути протестована для різних тестових наборів. Тести
мають бути оформленні у вигляді модульних тестів (наприклад, як наведено у п. 2.3).

## Варіант 8

   Алгоритм сортування обміном №4 ("шейкерне сортування") за незменшенням.


## Лістинг функції з використанням конструктивного підходу
```lisp
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
      (let* ((rest (right-to-left (cdr lst)))
             (current (car lst))
             (next (car rest)))
          (if (> current next)
              (cons next (cons current (cdr rest)))
              (cons current rest)))))

(defun exchange4-rec (lst L R)
  "Рекурсивний алгоритм сортування з прохідними межами L і R."
  (if (>= L R)
      lst
      (let ((new-lst (left-to-right lst)))
        (exchange4-rec (right-to-left new-lst) (1+ L) (1- R)))))

(defun exchange4-constructive (lst)
  "Функція шейкерного сортування масиву lst за незменшенням."
  (exchange4-rec lst 0 (1- (length lst))))

```
### Тестові набори та утиліти
```lisp
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
  (check-constructive "test 1.4" '(-6 3 2 5 1 4 3 ) '(1 2 3 3 4 5 -6)))
```
### Тестування
```lisp
CL-USER> (test-exchange4-constructive)
PASSED... test 1.1
PASSED... test 1.2
PASSED... test 1.3
PASSED... test 1.4
```
## Лістинг функції з використанням деструктивного підходу
```lisp
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
```
### Тестові набори та утиліти
```lisp
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
```
### Тестування
```lisp
CL-USER> (test-exchange4-imperative)
PASSED... test 2.1
PASSED... test 2.2
PASSED... test 2.3
PASSED... test 2.4
NIL
```


