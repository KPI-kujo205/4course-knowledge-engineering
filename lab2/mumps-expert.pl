/* Експертна система діагностики захворювання "Свинка" 
   Виконав: Куц І., Варіант 12 
*/

% Підвантаження зовнішніх даних 
:- initialization(['db.pl']).

% правило для перевірки наявності симптому у списку пацієнта
check_symptom(Patient, Symptom) :-
    patient_data(Patient, _, SymptomsList),
    member(Symptom, SymptomsList).

% 1. визначення основної клінічної картини 
is_clinical_mumps(P) :-
    check_symptom(P, ear_swelling),
    (check_symptom(P, chewing_pain) ; check_symptom(P, night_pain)),
    check_symptom(P, pear_face).

% 2. аналіз форми перебігу хвороби
severity(P, severe) :-
    patient_data(P, Age, _), Age >= 18,
    check_symptom(P, high_fever).

severity(P, mild) :-
    patient_data(P, Age, _), Age < 18.

severity(_, unknown).

% 3. головний предикат діагностики 
diagnose(P) :-
    nl, write('--- Результати для пацієнта: '), write(P), write(' ---'), nl,
    (is_clinical_mumps(P) -> 
        (write('Діагноз: Епідемічний паротит (Свинка)'), nl,
         severity(P, S), write('Форма перебігу: '), write(S), nl)
    ;   (write('Діагноз "Свинка" не підтверджено: недостатньо симптомів.'), nl)
    ).

% 4. запуск діагностики для всіх пацієнтів у базі 
run_all :-
    forall(patient_data(Name, _, _), diagnose(Name)).