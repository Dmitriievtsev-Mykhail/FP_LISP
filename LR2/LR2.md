<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт до лабораторної роботи 2</b><br/>
"Рекурсія"<br/>
дисципліни "Вступ до функціонального програмування"
</p>

<p align="right"> 
<b>Студент</b>: 
 Дмитрієвцев Михаїл Валерійович КВ-12</p>

<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
Реалізуйте дві рекурсивні функції, що виконують деякі дії з вхідним(и) списком(-ами), за
можливості/необхідності використовуючи різні види рекурсії. Функції, які необхідно
реалізувати, задаються варіантом (п. 2.1.1). Вимоги до функцій:
1. Зміна списку згідно із завданням має відбуватись за рахунок конструювання нового
списку, а не зміни наявного (вхідного).
2. Не допускається використання функцій вищого порядку чи стандартних функцій
для роботи зі списками, що не наведені в четвертому розділі навчального
посібника.
3. Реалізована функція не має бути функцією вищого порядку, тобто приймати функції
в якості аргументів.
4. Не допускається використання псевдофункцій (деструктивного підходу).
5. Не допускається використання циклів.
Кожна реалізована функція має бути протестована для різних тестових наборів. Тести
мають бути оформленні у вигляді модульних тестів (див. п. 2.3).

## Варіант 8

   1.Написати функцію reverse-and-nest-tail , яка обертає вхідний список та утворює
вкладeну структуру з підсписків з його елементами, починаючи з хвоста:

CL-USER> (reverse-and-nest-tail '(a b c))

(C (B (A)))

2.Написати функцію compress-list , яка заміщає сукупності послідовно
розташованих однакових елементів списку двоелементними списками виду
(кількість-повторень елемент):

CL-USER> (compress-list '(1 a a 3 3 3 b))

((1 1) (2 A) (3 3) (1 B))

## Лістинг функції reverse-and-nest-tail
```lisp
(defun reverse-and-nest-tail (lst &optional res)
  (if (null lst)
      res
      (reverse-and-nest-tail
       (cdr lst)
       (cons (car lst) (and res (list res))))))
```
### Тестові набори
```lisp
(defun test-reverse-and-nest-tail ()
  
  (check-reverse "test 1.1" '(a b c) '(C (B (A))))
  (check-reverse "test 1.2" '()      '())
  (check-reverse "test 1.3" '(1 2 3) '(3 (2 (1))))
  (check-reverse "test 1.4" '(nil 2 3)  '(3 (2 (nil)))))

  (defun check-reverse (name input expected)
  "Execute `reverse-and-nest-tail' on `input', compare result with `expected' and print
comparison status"
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (reverse-and-nest-tail input) expected)
          name))
```
### Тестування
```lisp
CL-USER> (test-reverse-and-nest-tail)
passed... test 1.1
passed... test 1.2
passed... test 1.3
passed... test 1.4
NIL
```
## Лістинг функції compress-list
```lisp

(defun compress-list (lst)
  (when lst
    (let ((count (count-reps (car lst) (cdr lst) 1)))
      (cons (list count (car lst)) (compress-list (nthcdr count lst))))))

(defun count-reps (element lst count)
  (if (and lst (eql element (car lst)))
      (count-reps element (cdr lst) (+ count 1))
      count))
```
### Тестові набори
```lisp
(defun test-compress-list ()
  (check-compress "test 2.1" '(1 a a 3 3 3 b) '((1 1) (2 A) (3 3) (1 B)))
  (check-compress "test 2.2" '(1 a a 1 1 a a) '((1 1) (2 A) (2 1) (2 A)))
  (check-compress "test 2.3" '() nil)
  (check-compress "test 2.4" '(1) '((1 1))))

(defun check-compress (name input expected)
  "Execute `compress-list' on `input', compare result with `expected' and print
  comparison status."
  (format t "~:[FAILED~;PASSED~]... ~a~%"
          (equal (compress-list input) expected)
          name))
```
### Тестування
```lisp
CL-USER> (test-compress-list)
passed... test 2.1
passed... test 2.2
passed... test 2.3
passed... test 2.4
NIL
```

