; завдання 3: Замінює в усьому списку атом E на атом B
; функція працює рекурсивно, обходячи вкладені списки

(defun replace-atom (lst old new)
  "Замінює всі входження атома OLD на NEW у списку LST (включно з вкладеними списками)."
  (cond
    ; базовий випадок: порожній список
    ((null lst) nil)
    
    ; якщо поточний елемент є атомом
    ((atom (car lst))
     (if (equal (car lst) old)
         ; Замінюємо атом і продовжуємо обхід
         (cons new (replace-atom (cdr lst) old new))
         ; Залишаємо атом без змін і продовжуємо обхід
         (cons (car lst) (replace-atom (cdr lst) old new))))
    
    ; Якщо поточний елемент є списком — рекурсивно обходимо його
    (t (cons (replace-atom (car lst) old new)
             (replace-atom (cdr lst) old new)))))

; --- Приклад ---
(format t "Вхідний список: (A E (B E) E C)~%")
(format t "Замінюємо E -> B~%")
(format t "Результат: ~a~%" (replace-atom '(A E (B E) E C) 'E 'B))

(format t "~%Ще приклад:~%")
(format t "Вхідний список: (E (E (E)) A E)~%")
(format t "Результат: ~a~%" (replace-atom '(E (E (E)) A E) 'E 'B))
