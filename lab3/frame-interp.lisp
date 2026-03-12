;;;; ============================================================
;;;; Мова представлення знань на основі фреймів - інтерпретатор
;;;; Лабораторна робота 3, Інженерія знань
;;;;
;;;; Структура фрейму (вкладені асоціативні списки):
;;;;   фрейм   = (ім'я-фрейму  слот*)
;;;;   слот    = (ім'я-слоту   аспект*)
;;;;   аспект  = (ім'я-аспекту елемент*)
;;;;   елемент = (значення     коментар*)
;;;;   коментар = (мітка       повідомлення*)
;;;;
;;;; Спеціальні аспекти: $VALUE $DEFAULT $IF-ADDED $IF-REMOVED
;;;; Спеціальні слоти:   AKO
;;;; Мітки коментарів:   |STATUS:| |SLOT:| |FACET:| |PARM:| |PARMQ:|
;;;; Значення статусу:   EVAL  INDIRECT  NOEVAL
;;;; ============================================================


;;; ============================================================
;;; Глобальна база даних фреймів - просто список фреймів
;;; ============================================================

(defvar *frames* nil)

;;; Контекстні змінні, що встановлюються перед запуском демонів,
;;; щоб код демона знав, який фрейм/слот/значення його викликали.
(defvar *env-frame* nil)
(defvar *env-slot*  nil)
(defvar *env-value* nil)


;;; ============================================================
;;; Допоміжні функції нижнього рівня - пошук у вкладених списках
;;; ============================================================

;;; Знайти фрейм за іменем у базі даних
(defun get-frame (name)
  (assoc name *frames*))

;;; Знайти слот за іменем у фреймі
;;; Структура фрейму: (ім'я-фрейму слот1 слот2 ...)
(defun get-slot (frame slot-name)
  (assoc slot-name (cdr frame)))

;;; Знайти аспект за іменем у слоті
(defun get-facet (slot facet-name)
  (assoc facet-name (cdr slot)))

;;; Отримати всі елементи даних з аспекту
;;; Структура аспекту:  (ім'я-аспекту елемент1 елемент2 ...)
;;; Структура елементу: (значення коментар1 коментар2 ...)
(defun facet-data (facet)
  (cdr facet))

;;; Отримати значення елемента (перший елемент списку)
(defun datum-val (datum)
  (car datum))

;;; Знайти коментар за міткою в елементі даних
;;; Структура коментаря: (мітка повідомлення1 повідомлення2 ...)
(defun get-comment (datum label)
  (assoc label (cdr datum)))

;;; Отримати повідомлення з коментаря (все після мітки)
(defun comment-msgs (comment)
  (cdr comment))

;;; Отримати статус елемента даних (EVAL, INDIRECT, NOEVAL або nil)
(defun datum-status (datum)
  (let ((sc (get-comment datum '|STATUS:|)))
    (if sc (car (comment-msgs sc)) nil)))

;;; Перевірити, чи значення вже є в аспекті
(defun value-in-facet (facet val)
  (assoc val (facet-data facet)))


;;; ============================================================
;;; Функції створення структур
;;; ============================================================

;;; Переконатись, що фрейм існує; якщо ні - створити
(defun ensure-frame (name)
  (let ((f (get-frame name)))
    (if f
        f
        (let ((new-frame (list name)))
          (setf *frames* (cons new-frame *frames*))
          new-frame))))

;;; Переконатись, що слот існує у фреймі; якщо ні - створити
(defun ensure-slot (frame-name slot-name)
  (let* ((frame (ensure-frame frame-name))
         (slot  (get-slot frame slot-name)))
    (if slot
        slot
        (let ((new-slot (list slot-name)))
          (nconc frame (list new-slot))
          new-slot))))

;;; Переконатись, що аспект існує у слоті; якщо ні - створити
(defun ensure-facet (frame-name slot-name facet-name)
  (let* ((slot  (ensure-slot frame-name slot-name))
         (facet (get-facet slot facet-name)))
    (if facet
        facet
        (let ((new-facet (list facet-name)))
          (nconc slot (list new-facet))
          new-facet))))


;;; ============================================================
;;; FPUT - додати значення до фрейму/слоту/аспекту
;;; ============================================================

(defun fput (frame-name slot-name facet-name value)
  "Додає VALUE до FRAME-NAME/SLOT-NAME/FACET-NAME.
   Автоматично створює відсутні структури.
   Запускає демони $IF-ADDED при додаванні до $VALUE або $DEFAULT."
  (let ((facet (ensure-facet frame-name slot-name facet-name)))
    ;; Додавати тільки якщо значення ще немає
    (unless (value-in-facet facet value)
      (nconc facet (list (list value))))
    ;; Запустити демони $IF-ADDED при додаванні реального значення
    (when (or (eq facet-name '$value) (eq facet-name '$default))
      (setf *env-frame* frame-name
            *env-slot*  slot-name
            *env-value* value)
      (fire-demons frame-name slot-name '$if-added value)))
  value)


;;; ============================================================
;;; FREMOVE - видалити елементи з мережі фреймів
;;; ============================================================

(defun fremove (frame-name &optional slot-name facet-name value)
  "Видаляє підструктуру з мережі фреймів.
   (fremove F)         - видалити весь фрейм
   (fremove F S)       - видалити слот S
   (fremove F S A)     - видалити аспект A
   (fremove F S A V)   - видалити значення V
   Запускає демони $IF-REMOVED при видаленні з $VALUE."
  (cond
    ;; Видалити весь фрейм
    ((null slot-name)
     (setf *frames* (remove frame-name *frames* :key #'car)))

    ;; Видалити слот
    ((null facet-name)
     (let ((frame (get-frame frame-name)))
       (when frame
         (setf (cdr frame) (remove slot-name (cdr frame) :key #'car)))))

    ;; Видалити аспект
    ((null value)
     (let ((slot (get-slot (get-frame frame-name) slot-name)))
       (when slot
         (setf (cdr slot) (remove facet-name (cdr slot) :key #'car)))))

    ;; Видалити конкретне значення
    (t
     (let* ((frame (get-frame frame-name))
            (slot  (when frame (get-slot frame slot-name)))
            (facet (when slot  (get-facet slot facet-name))))
       (when facet
         (setf (cdr facet) (remove value (cdr facet) :key #'car))
         ;; Запустити $IF-REMOVED при видаленні з $VALUE
         (when (eq facet-name '$value)
           (setf *env-frame* frame-name
                 *env-slot*  slot-name
                 *env-value* value)
           (fire-demons frame-name slot-name '$if-removed value))))))
  t)


;;; ============================================================
;;; Запуск демонів
;;; ============================================================

;;; Запустити всі демони зі слоту DEMON-FACET фрейму FRAME-NAME/SLOT-NAME.
;;; Шукає тільки локально, без підйому по ланцюжку AKO.
(defun fire-demons (frame-name slot-name demon-facet value)
  (let* ((frame (get-frame frame-name))
         (slot  (when frame (get-slot frame slot-name)))
         (facet (when slot  (get-facet slot demon-facet)))
         (data  (when facet (facet-data facet))))
    (when data
      (setf *env-frame* frame-name
            *env-slot*  slot-name
            *env-value* value)
      (dolist (datum data)
        (eval-datum datum demon-facet)))))


;;; ============================================================
;;; Обчислення елементів даних
;;; ============================================================

;;; Перевірити, чи є аспект демонічним
(defun demon-facet-p (facet-name)
  (member facet-name '($if-added $if-removed $if-needed
                       $if-instantiated $if-get $if-put $if-rem)))

;;; Обчислити елемент даних згідно з його статусом STATUS:
(defun eval-datum (datum facet-name)
  (let ((val    (datum-val datum))
        (status (datum-status datum)))
    (cond
      ;; Демонічні аспекти завжди виконуються, якщо не NOEVAL
      ((and (demon-facet-p facet-name) (not (eq status 'noeval)))
       (run-form val datum))
      ;; Явний статус EVAL
      ((eq status 'eval)
       (run-form val datum))
      ;; Пряме значення - повернути як є
      (t val))))

;;; Виконати форму або викликати іменовану функцію з аргументами PARM:/PARMQ:
(defun run-form (val datum)
  (cond
    ;; Складена форма (список) - просто обчислити через eval
    ((consp val)
     (eval val))
    ;; Символ - викликати як функцію, зібравши аргументи з коментарів
    ((symbolp val)
     (let* ((parm-cmt  (get-comment datum '|PARM:|))
            (parmq-cmt (get-comment datum '|PARMQ:|))
            (parm-args  (when parm-cmt  (mapcar #'eval (comment-msgs parm-cmt))))
            (parmq-args (when parmq-cmt (comment-msgs parmq-cmt))))
       (apply val (append parm-args parmq-args))))
    (t val)))


;;; ============================================================
;;; FGET - отримати значення з мережі фреймів
;;; ============================================================

(defun fget (frame-name slot-name &optional (facet-name '$value) keys)
  "Отримує дані з FRAME-NAME/SLOT-NAME/FACET-NAME.
   Успадковує через $DEFAULT та ланцюжок AKO якщо нічого не знайдено локально.
   Ключі: E = повернути тільки перше значення, -H = без AKO, 0 = без $DEFAULT."
  (let ((result (fget-inner frame-name slot-name facet-name keys nil)))
    (if (member 'e keys) (car result) result)))

;;; Внутрішній рекурсивний FGET
;;; SEEN - список вже відвіданих фреймів (захист від нескінченних циклів)
(defun fget-inner (frame-name slot-name facet-name keys seen)
  (when (member frame-name seen)
    (return-from fget-inner nil))

  (let* ((frame     (get-frame frame-name))
         (slot      (when frame (get-slot frame slot-name)))
         (no-ako     (member '-h keys))
         (no-default (member '0  keys)))

    (cond
      ;; Не $VALUE аспект: без успадкування, повернути дані як є
      ((not (eq facet-name '$value))
       (let ((facet (when slot (get-facet slot facet-name))))
         (when facet
           (collect-values (facet-data facet) frame-name slot-name facet-name))))

      ;; Знайдено дані в $VALUE - обчислити і повернути
      ((and slot
            (get-facet slot '$value)
            (facet-data (get-facet slot '$value)))
       (collect-values (facet-data (get-facet slot '$value))
                       frame-name slot-name '$value))

      ;; Немає $VALUE - спробувати успадкування через $DEFAULT
      ((and (not no-default)
            slot
            (get-facet slot '$default)
            (facet-data (get-facet slot '$default)))
       (let* ((defaults (collect-values (facet-data (get-facet slot '$default))
                                        frame-name slot-name '$default))
              (result nil))
         ;; Кожне значення $DEFAULT - це ім'я фрейму для пошуку
         (dolist (df defaults)
           (let ((r (fget-inner df slot-name '$value keys
                                (cons frame-name seen))))
             (when r (setf result r) (return))))
         ;; Якщо все одно нічого - спробувати AKO
         (or result
             (when (not no-ako)
               (ako-inherit frame-name slot-name facet-name keys
                            (cons frame-name seen))))))

      ;; Нічого не знайдено локально - спробувати ланцюжок AKO
      (t
       (when (not no-ako)
         (ako-inherit frame-name slot-name facet-name keys
                      (cons frame-name seen)))))))

;;; Піднятись по слоту AKO до фреймів-прототипів і повторити FGET там
(defun ako-inherit (frame-name slot-name facet-name keys seen)
  (let* ((frame (get-frame frame-name))
         (ako-slot  (when frame (get-slot frame 'ako)))
         (ako-facet (when ako-slot (get-facet ako-slot '$value))))
    (when ako-facet
      (let ((result nil))
        (dolist (ako-datum (facet-data ako-facet))
          (let ((r (fget-inner (datum-val ako-datum) slot-name facet-name
                               keys seen)))
            (when r (setf result r) (return))))
        result))))

;;; Обчислити список елементів даних, застосовуючи INDIRECT/EVAL де потрібно
(defun collect-values (data-list frame-name slot-name facet-name)
  (let ((result nil))
    (dolist (datum data-list)
      (let ((resolved (resolve-one datum frame-name slot-name facet-name)))
        (when resolved
          (if (and (eq (datum-status datum) 'indirect) (listp resolved))
              ;; INDIRECT повертає список значень - вставити всі
              (setf result (append result resolved))
              ;; Все інше - одне значення
              (setf result (append result (list resolved)))))))
    result))

;;; Обчислити один елемент даних згідно з його статусом STATUS:
(defun resolve-one (datum frame-name slot-name facet-name)
  (let ((val    (datum-val datum))
        (status (datum-status datum)))
    (cond
      ;; INDIRECT - перенаправити до іншого фрейму/слоту/аспекту
      ((eq status 'indirect)
       (let* ((tgt-frame  (if (eq val '*) frame-name val))
              (slot-cmt   (get-comment datum '|SLOT:|))
              (tgt-slot   (if slot-cmt (car (comment-msgs slot-cmt)) slot-name))
              (facet-cmt  (get-comment datum '|FACET:|))
              (tgt-facet  (if facet-cmt (car (comment-msgs facet-cmt)) facet-name)))
         (fget tgt-frame tgt-slot tgt-facet)))
      ;; EVAL - обчислити форму або викликати функцію
      ((eq status 'eval)
       (run-form val datum))
      ;; DIRECT - повернути як є
      (t val))))


;;; ============================================================
;;; Демонстраційні приклади
;;; ============================================================

(setf *frames* nil)

;;; Т1: Базові FPUT + FGET
(fput 'person 'category '$value 'living-being)
(print (fget 'person 'category '$value))       ; => (LIVING-BEING)
(print (fget 'person 'category '$value '(e)))  ; => LIVING-BEING

;;; Т2: Успадкування по AKO
(setf *frames* nil)
(fput 'student 'faculty '$value 'engineering)
(fput 'ivan 'ako '$value 'student)
(print (fget 'ivan 'faculty '$value))          ; => (ENGINEERING)

;;; Т3: Елемент з типом INDIRECT
(setf *frames* nil)
(ensure-facet 'f1 's '$value)
(nconc (get-facet (get-slot (get-frame 'f1) 's) '$value)
       (list '(f2 (|STATUS:| indirect) (|SLOT:| q))))
(fput 'f2 'q '$value 'v2)
(print (fget 'f1 's '$value))                  ; => (V2)

;;; Т4: Елемент з типом EVAL
(setf *frames* nil)
(ensure-facet 'calc 'result '$value)
(nconc (get-facet (get-slot (get-frame 'calc) 'result) '$value)
       (list '((+ 2 3) (|STATUS:| eval))))
(print (fget 'calc 'result '$value))           ; => (5)

;;; Т5: Демон $IF-ADDED спрацьовує при додаванні значення
(setf *frames* nil)
(defparameter *added-log* nil)
(ensure-facet 'alice 'score '$if-added)
(nconc (get-facet (get-slot (get-frame 'alice) 'score) '$if-added)
       (list '((push (list *env-frame* *env-slot* *env-value*) *added-log*)
               (|STATUS:| eval))))
(fput 'alice 'score '$value 42)
(print *added-log*)                            ; => ((ALICE SCORE 42))

;;; Т6: Значення за замовчуванням через $DEFAULT
(setf *frames* nil)
(fput 'red-thing 'colour '$value 'red)
(fput 'gadget 'colour '$default 'red-thing)
(print (fget 'gadget 'colour '$value))         ; => (RED)

;;; Т7: FREMOVE - видалення значення
(setf *frames* nil)
(fput 'robot 'status '$value 'active)
(fput 'robot 'status '$value 'charging)
(fremove 'robot 'status '$value 'active)
(print (fget 'robot 'status '$value))          ; => (CHARGING)

;;; Т8: Демон $IF-REMOVED спрацьовує при видаленні значення
(setf *frames* nil)
(defparameter *removed-log* nil)
(ensure-facet 'shelf 'book '$if-removed)
(nconc (get-facet (get-slot (get-frame 'shelf) 'book) '$if-removed)
       (list '((push *env-value* *removed-log*) (|STATUS:| eval))))
(fput 'shelf 'book '$value 'lisp-book)
(fremove 'shelf 'book '$value 'lisp-book)
(print *removed-log*)                          ; => (LISP-BOOK)

;;; Т9: Ланцюжок AKO з трьох рівнів
(setf *frames* nil)
(fput 'animal 'breathes '$value 'air)
(fput 'mammal 'ako '$value 'animal)
(fput 'dog    'ako '$value 'mammal)
(fput 'fido   'ako '$value 'dog)
(print (fget 'fido 'breathes '$value))         ; => (AIR)

;;; Т10: Ключ -H вимикає успадкування по AKO
(print (fget 'fido 'breathes '$value '(-h)))   ; => NIL
