% Записауємо факти, записуємо чоловіків 
man(ivan). man(oleh). 
man(mykyta). man(vasyl).
man(maksym). man(robert). 
man(pavlo).

% Записауємо факти, записуємо жінок 
woman(nina). woman(hanna).
woman(tetiana). woman(maryna).
woman(mariya). woman(oksana). 
woman(yevheniya). woman(hrystyna).

% roditel(Dytyna, Batko_Maty): Dytyna - людинка, Batko_Maty  - батько чи мати

% Записуємо відношення батьківства
% Для Василя
roditel(vasyl, ivan).
roditel(vasyl, hanna).

% Для Тетяни
roditel(tetiana, oleh).
roditel(tetiana, nina).

% Для Марини
roditel(maryna, oleh).
roditel(maryna, nina).

% Для Марії
roditel(mariya, hanna).
roditel(mariya, ivan).

% Для Максима
roditel(maksym, tetiana).
roditel(maksym, vasyl).

% Для Євгенії
roditel(yevheniya, tetiana).
roditel(yevheniya, vasyl).

% Для Христини
roditel(hrystyna, oksana).
roditel(hrystyna, mykyta).

% Для Робетра
roditel(robert, maksym).
roditel(robert, hrystyna).

% Для Павла
roditel(pavlo, maksym).
roditel(pavlo, hrystyna).


% Питання, на які треба відповісти
% Правило для пошуку людей, які є батьками і самі мають батьків
maie_pokolinnya(Lyudyna) :- 
    roditel(_, Lyudyna),    % Lyudyna є батьком для когось
    roditel(Lyudyna, _).    % Lyudyna є дитиною для когось

% Правило для пошуку тих, хто не має дітей
bez_ditay(Lyudyna) :- 
    (man(Lyudyna); woman(Lyudyna)), 
    \+ roditel(_, Lyudyna). % Не існує факту, де Lyudyna - батько



% 
% ПРЕДИКАТИ РОДИННИХ ЗВ'ЯЗКІВ (ВАРІАНТ 12)
% Логіка: roditel(Дитина, Батько_Мати)
% 

% 1. Батько
batko(Batko, Dytyna) :- 
    roditel(Dytyna, Batko), 
    man(Batko).

% 2. Мати
maty(Maty, Dytyna) :- 
    roditel(Dytyna, Maty), 
    woman(Maty).

% 3. Син
syn(Syn, Batko_Maty) :- 
    roditel(Syn, Batko_Maty), 
    man(Syn).

% 4. Дочка
dochka(Dochka, Batko_Maty) :- 
    roditel(Dochka, Batko_Maty), 
    woman(Dochka).

% 5. Брат (спільний батько/мати Z, і це різні люди)
brat(Brat, Liudyna) :- 
    roditel(Brat, Z), 
    roditel(Liudyna, Z), 
    man(Brat), 
    Brat \= Liudyna.

% 6. Сестра
sestra(Sestra, Liudyna) :- 
    roditel(Sestra, Z), 
    roditel(Liudyna, Z), 
    woman(Sestra), 
    Sestra \= Liudyna.

% 7. Дід (батько батька або матері)
did(Did, Onuk) :- 
    roditel(Onuk, Batko_Maty), 
    roditel(Batko_Maty, Did), 
    man(Did).

% 8. Баба (мати батька або матері)
baba(Baba, Onuk) :- 
    roditel(Onuk, Batko_Maty), 
    roditel(Batko_Maty, Baba), 
    woman(Baba).

% 9. Онук
onuk(Onuk, Did_Baba) :- 
    (did(Did_Baba, Onuk); baba(Did_Baba, Onuk)), 
    man(Onuk).

% 10. Онучка
onuchka(Onuchka, Did_Baba) :- 
    (did(Did_Baba, Onuchka); baba(Did_Baba, Onuchka)), 
    woman(Onuchka).

% 11. Дядько (брат батька або матері)
diadko(Diadko, Pleminnyk) :- 
    roditel(Pleminnyk, Batko_Maty), 
    brat(Diadko, Batko_Maty).

% 12. Тітка (сестра батька або матері)
titka(Titka, Pleminnyk) :- 
    roditel(Pleminnyk, Batko_Maty), 
    sestra(Titka, Batko_Maty).

% 13. Небіж (племінник - син брата або сестри)
nebizh(Nebizh, Diadko_Titka) :- 
    (brat(Z, Diadko_Titka); sestra(Z, Diadko_Titka)), 
    roditel(Nebizh, Z), 
    man(Nebizh).

% 14. Небога (племінниця - дочка брата або сестри)
neboha(Neboha, Diadko_Titka) :- 
    (brat(Z, Diadko_Titka); sestra(Z, Diadko_Titka)), 
    roditel(Neboha, Z), 
    woman(Neboha).