;;;; Лабораторна робота 4 — Фреймова база знань
;;;; Предметна область: Діагностика паротиту (Свинка)
;;;;
;;;; Вхід : ім'я пацієнта, вік, список симптомів
;;;; Вихід: DIAGNOSIS — MUMPS | NOT-MUMPS
;;;;        SEVERITY  — SEVERE | MILD | UNKNOWN
;;;;
;;;; Критерій: ear_swelling AND (chewing_pain OR night_pain) AND pear_face
;;;; Тяжкість: вік >= 18 + high_fever => SEVERE; вік < 18 => MILD
;;;;
;;;; Ієрархія (AKO):
;;;;   THING -> PERSON -> PATIENT -> MUMPS-PATIENT -> ADULT-MUMPS-PATIENT
;;;;                                               -> CHILD-MUMPS-PATIENT
;;;;                             -> HEALTHY-PATIENT
;;;; Екземпляри: IVAN (adult-mumps), MARIYA (child-mumps), OLEH (healthy)


(load "../lab3/frame-interp.lisp")
(setf *frames* nil)


;;; --- допоміжні функції ---

(defun has-symptom (patient symptom)
  (let ((s (fget patient 'symptoms '$value '(e))))
    (and s (member symptom s))))

(defun patient-age (patient)
  (fget patient 'age '$value '(e)))

(defun compute-diagnosis (patient)
  (if (and (has-symptom patient 'ear_swelling)
           (or (has-symptom patient 'chewing_pain)
               (has-symptom patient 'night_pain))
           (has-symptom patient 'pear_face))
      'mumps 'not-mumps))

(defun compute-severity (patient)
  (cond
    ((not (eq (compute-diagnosis patient) 'mumps)) 'unknown)
    ((and (>= (or (patient-age patient) 0) 18)
          (has-symptom patient 'high_fever)) 'severe)
    ((< (or (patient-age patient) 99) 18) 'mild)
    (t 'unknown)))


;;; --- хвороба ---

(fput 'mumps-disease 'name             '$value 'mumps)
(fput 'mumps-disease 'critical-symptom '$value 'ear_swelling)
(fput 'mumps-disease 'symptoms         '$value
      '(head_pain muscle_pain dry_mouth ear_swelling
        chewing_pain pear_face high_fever))


;;; --- ієрархія прототипів ---

(fput 'thing               'description '$value "root")
(fput 'person              'ako         '$value 'thing)
(fput 'patient             'ako         '$value 'person)
(fput 'mumps-patient       'ako         '$value 'patient)
(fput 'mumps-patient       'disease     '$value 'mumps-disease)
(fput 'adult-mumps-patient 'ako         '$value 'mumps-patient)
(fput 'child-mumps-patient 'ako         '$value 'mumps-patient)
(fput 'healthy-patient     'ako         '$value 'patient)

;;; DIAGNOSIS і SEVERITY — обчислювані слоти (STATUS:EVAL) на прототипі PATIENT
(ensure-facet 'patient 'diagnosis '$value)
(nconc (get-facet (get-slot (get-frame 'patient) 'diagnosis) '$value)
       (list '((compute-diagnosis *env-frame*) (|STATUS:| eval))))

(ensure-facet 'patient 'severity '$value)
(nconc (get-facet (get-slot (get-frame 'patient) 'severity) '$value)
       (list '((compute-severity *env-frame*) (|STATUS:| eval))))


;;; --- правило діагностики ---

(fput 'mumps-diagnosis-rule 'criterion '$value
      "ear_swelling AND (chewing_pain OR night_pain) AND pear_face")

(ensure-facet 'mumps-diagnosis-rule 'check-rule '$value)
(nconc (get-facet (get-slot (get-frame 'mumps-diagnosis-rule) 'check-rule) '$value)
       (list '((lambda (p)
                 (and (has-symptom p 'ear_swelling)
                      (or (has-symptom p 'chewing_pain)
                          (has-symptom p 'night_pain))
                      (has-symptom p 'pear_face)))
               (|STATUS:| eval))))


;;; --- екземпляри пацієнтів ---

(fput 'ivan   'ako      '$value 'adult-mumps-patient)
(fput 'ivan   'age      '$value 22)
(fput 'ivan   'symptoms '$value
      '(head_pain muscle_pain dry_mouth ear_swelling
        chewing_pain pear_face high_fever extreme_weak))

(fput 'mariya 'ako      '$value 'child-mumps-patient)
(fput 'mariya 'age      '$value 8)
(fput 'mariya 'symptoms '$value
      '(head_pain ear_swelling night_pain pear_face))

(fput 'oleh   'ako      '$value 'healthy-patient)
(fput 'oleh   'age      '$value 30)
(fput 'oleh   'symptoms '$value
      '(head_pain high_fever muscle_pain))


;;; --- діагностика ---

(defun run-diagnosis ()
  (format t "~%Діагностика паротиту~%")
  (dolist (p '(ivan mariya oleh))
    (setf *env-frame* p)
    (format t "~A | вік: ~2A | діагноз: ~-10A | тяжкість: ~A~%"
            p
            (patient-age p)
            (compute-diagnosis p)
            (compute-severity p))))

(run-diagnosis)
