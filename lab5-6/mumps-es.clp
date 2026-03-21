; Лабораторна робота 5-6 — Експертна система "Свинка" (Епідемічний паротит)
; Виконав: Куц І., Варіант 12
;
; Вхідні змінні   : вік, 4 продромальні симптоми, 5 клінічних симптомів
; Проміжні змінні : prodromal (продромальний синдром), clinical (клінічні ознаки)
; Вихідна змінна  : діагноз + рекомендація


; --- defglobal: порогові константи ---

(defglobal
    ?*ADULT-AGE* = 18)


; --- deftemplate: структура фактів ---

(deftemplate patient
    (slot age        (type INTEGER))
    (slot high-fever (type INTEGER) (default 0)))

(deftemplate symptoms
    (slot headache     (type INTEGER) (default 0))
    (slot muscle-pain  (type INTEGER) (default 0))
    (slot chills       (type INTEGER) (default 0))
    (slot dry-mouth    (type INTEGER) (default 0))
    (slot ear-swelling (type INTEGER) (default 0))
    (slot chewing-pain (type INTEGER) (default 0))
    (slot night-pain   (type INTEGER) (default 0))
    (slot pear-face    (type INTEGER) (default 0)))


; --- deffacts: початковий стан ---

(deffacts startup
    (initial-fact))


; --- deffunction: підрахунок продромальних симптомів ---

(deffunction count-prodromal (?h ?m ?c ?d)
    (+ ?h ?m ?c ?d))


; --- Зчитування вхідних даних ---

(defrule data-input
    (initial-fact)
    =>
    (printout t crlf "=== Експертна система діагностики 'Свинка' ===" crlf)

    (printout t crlf "Введіть вік пацієнта: ")
    (bind ?age (read))

    (printout t crlf "--- Продромальні симптоми (1 - так / 0 - ні) ---" crlf)
    (printout t "Чи є головний біль? ")
    (bind ?h (read))
    (printout t "Чи є м'язовий або суглобовий біль? ")
    (bind ?m (read))
    (printout t "Чи є озноб? ")
    (bind ?c (read))
    (printout t "Чи є сильна сухість у роті? ")
    (bind ?d (read))

    (printout t crlf "--- Клінічні симптоми (1 - так / 0 - ні) ---" crlf)
    (printout t "Чи є припухлість у ділянці вушної раковини? ")
    (bind ?e (read))
    (printout t "Чи болить при пережовуванні їжі? ")
    (bind ?cp (read))
    (printout t "Чи є біль переважно вночі? ")
    (bind ?np (read))
    (printout t "Чи набуває обличчя форми груші? ")
    (bind ?pf (read))
    (printout t "Чи є підвищення температури (вище 38°C)? ")
    (bind ?f (read))

    (assert (patient (age ?age) (high-fever ?f)))
    (assert (symptoms
        (headache ?h) (muscle-pain ?m) (chills ?c) (dry-mouth ?d)
        (ear-swelling ?e) (chewing-pain ?cp) (night-pain ?np) (pear-face ?pf))))


; --- Правила для проміжної змінної 1: продромальний синдром ---

(defrule R1
    (symptoms (headache ?h) (muscle-pain ?m) (chills ?c) (dry-mouth ?d))
    (test (= (count-prodromal ?h ?m ?c ?d) 0))
    =>
    (printout t crlf "Продромальний синдром відсутній." crlf)
    (assert (prodromal "none"))
    (assert (prodromalcnst 0)))

(defrule R2
    (symptoms (headache ?h) (muscle-pain ?m) (chills ?c) (dry-mouth ?d))
    (test (and (>= (count-prodromal ?h ?m ?c ?d) 1)
               (<= (count-prodromal ?h ?m ?c ?d) 2)))
    =>
    (printout t crlf "Слабкий продромальний синдром." crlf)
    (assert (prodromal "partial"))
    (assert (prodromalcnst 1)))

(defrule R3
    (symptoms (headache ?h) (muscle-pain ?m) (chills ?c) (dry-mouth ?d))
    (test (>= (count-prodromal ?h ?m ?c ?d) 3))
    =>
    (printout t crlf "Виражений продромальний синдром." crlf)
    (assert (prodromal "full"))
    (assert (prodromalcnst 2)))


; --- Правила для проміжної змінної 2: клінічні ознаки ---

(defrule R4
    (symptoms (ear-swelling ?e))
    (test (= ?e 0))
    =>
    (printout t crlf "Ключові клінічні ознаки свинки відсутні." crlf)
    (assert (clinical_signs "none"))
    (assert (clinicalcnst 0)))

(defrule R5
    (symptoms (ear-swelling ?e) (chewing-pain ?cp) (night-pain ?np) (pear-face ?pf))
    (test (and (= ?e 1) (= (+ ?cp ?np ?pf) 0)))
    =>
    (printout t crlf "Є лише припухлість привушних залоз — неповна клінічна картина." crlf)
    (assert (clinical_signs "partial"))
    (assert (clinicalcnst 1)))

(defrule R6
    (symptoms (ear-swelling ?e) (chewing-pain ?cp) (night-pain ?np) (pear-face ?pf))
    (test (and (= ?e 1) (or (= ?cp 1) (= ?np 1)) (= ?pf 1)))
    =>
    (printout t crlf "Наявні всі ключові клінічні ознаки свинки." crlf)
    (assert (clinical_signs "full"))
    (assert (clinicalcnst 2)))


; --- Правила для вихідної змінної: діагноз ---

(defrule R7
    (patient (age ?age) (high-fever ?f))
    (clinicalcnst ?cc)
    (test (and (= ?cc 2) (>= ?age ?*ADULT-AGE*) (= ?f 1)))
    =>
    (printout t crlf "ДІАГНОЗ: Епідемічний паротит (Свинка) — підтверджено." crlf)
    (printout t "Форма перебігу: ТЯЖКА (доросла людина з підвищеною температурою)." crlf)
    (assert (diagnosis "mumps"))
    (assert (severitycnst 3)))

(defrule R8
    (patient (age ?age) (high-fever ?f))
    (clinicalcnst ?cc)
    (test (and (= ?cc 2) (>= ?age ?*ADULT-AGE*) (= ?f 0)))
    =>
    (printout t crlf "ДІАГНОЗ: Епідемічний паротит (Свинка) — підтверджено." crlf)
    (printout t "Форма перебігу: ПОМІРНА (доросла людина без значного підвищення температури)." crlf)
    (assert (diagnosis "mumps"))
    (assert (severitycnst 2)))

(defrule R9
    (patient (age ?age))
    (clinicalcnst ?cc)
    (test (and (= ?cc 2) (< ?age ?*ADULT-AGE*)))
    =>
    (printout t crlf "ДІАГНОЗ: Епідемічний паротит (Свинка) — підтверджено." crlf)
    (printout t "Форма перебігу: ЛЕГКА (дитина, симптоми зазвичай менш виражені)." crlf)
    (assert (diagnosis "mumps"))
    (assert (severitycnst 1)))

(defrule R10
    (clinicalcnst ?cc)
    (prodromalcnst ?pc)
    (test (and (= ?cc 1) (> ?pc 0)))
    =>
    (printout t crlf "ДІАГНОЗ: Підозра на свинку — є набряк і продромальні симптоми." crlf)
    (assert (diagnosis "suspected")))

(defrule R11
    (clinicalcnst ?cc)
    (prodromalcnst ?pc)
    (test (and (= ?cc 1) (= ?pc 0)))
    =>
    (printout t crlf "ДІАГНОЗ: Можлива свинка — є лише припухлість, без інших симптомів." crlf)
    (assert (diagnosis "suspected")))

(defrule R12
    (clinicalcnst ?cc)
    (prodromalcnst ?pc)
    (test (and (= ?cc 0) (> ?pc 0)))
    =>
    (printout t crlf "ДІАГНОЗ: Свинка не підтверджена — клінічні ознаки відсутні." crlf)
    (assert (diagnosis "not-mumps")))

(defrule R13
    (clinicalcnst ?cc)
    (prodromalcnst ?pc)
    (test (and (= ?cc 0) (= ?pc 0)))
    =>
    (printout t crlf "ДІАГНОЗ: Симптомів не виявлено." crlf)
    (assert (diagnosis "healthy")))


; --- Правила для рекомендацій ---

(defrule R14
    (diagnosis "mumps")
    (severitycnst ?sc)
    (test (= ?sc 3))
    =>
    (printout t crlf "РЕКОМЕНДАЦІЯ: Термінова госпіталізація!" crlf)
    (printout t "Постільний режим, ізоляція, антипіретики за призначенням лікаря." crlf))

(defrule R15
    (diagnosis "mumps")
    (severitycnst ?sc)
    (test (<= ?sc 2))
    =>
    (printout t crlf "РЕКОМЕНДАЦІЯ: Домашнє лікування під наглядом лікаря." crlf)
    (printout t "Постільний режим, м'яка їжа, рясне пиття, ізоляція від дітей." crlf))

(defrule R16
    (diagnosis "suspected")
    =>
    (printout t crlf "РЕКОМЕНДАЦІЯ: Негайно зверніться до інфекціоніста для уточнення діагнозу." crlf))

(defrule R17
    (diagnosis "not-mumps")
    =>
    (printout t crlf "РЕКОМЕНДАЦІЯ: Зверніться до лікаря для з'ясування причини симптомів." crlf))

(defrule R18
    (diagnosis "healthy")
    =>
    (printout t crlf "РЕКОМЕНДАЦІЯ: Стан задовільний, ознак захворювання не виявлено." crlf))
