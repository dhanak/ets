% This is a -*- prolog -*- file.
:- module(templates, []).

:- use_module(library(system)).
:- use_module(library(sockets)).
:- use_module(library(lists)).
:- use_module(utils).

test_class_closed('Hataridon kivuli beadas', Name,
          date(FY,FM,FD), date(TY,TM,TD)) :-
    monthname(FM, FMN),
    monthname(TM, TMN),
    format('~NA(z) ~w csak ~w. ~w ~w. és ~w. ~w ~w. között adható be.~n',
           [Name,FY,FMN,FD,TY,TMN,TD]).

ill_formed_mail('Hibas levelformatum', LogF) :-
    read_lines(LogF, Lines),
    write('A hiba oka:'), nl,
    nl,
    write_lines(Lines).

mail_spooled('HF felveve', ClassName, Version, Pos, Running) :-
    format('A(z) ~w ~w. verziója megérkezett.~n', [ClassName, Version]),
    write('A feldolgozási sorban '),
    (   Pos = replace(I)
    ->  write('felváltotta a programod legutóbbi verzióját, így '), nl
    ;   Pos = new(I)
    ),
    format('a(z) ~w. helyre került.~n', [I]),
    (   Running > 0
    ->  write('A rendszer jelenleg a programod korábbi, '),
        format('~w. verzióját teszteli.~n', [Running])
    ;   true
    ).

test_done('HF tesztnaplo', LogF) :-
    read_lines(LogF, Lines),
    write_lines(Lines).

print_report_head(ID) :-
    datime(datime(_,Month,Day,Hour,Min,_)),
    monthname(Month, MName),
    current_host(Host),
    atom_codes(ID, IDC),
    append(Name, [0'.|Version], IDC),
    fail_on_error(number_codes(_, Version), silent),
    format('~tTesztnapló~t~70+~n~t----------~t~70+~2n', []),
    format('A hallgató neve és azonosítója: ~s~n', [Name]),
    (   catch(guts:test_name(TestName), _, fail)
    ->  format('A teszt neve:                   ~w~n', [TestName])
    ;   true
    ),
    format('A program verziószáma:          ~s~n', [Version]),
    format('A teszt idõpontja:              ~w ~d. ~|~`0t~d~2+:~|~`0t~d~2+~n',
           [MName,Day,Hour,Min]),
    format('A tesztet futtató hoszt:        ~w~2n', [Host]).

print_lang_separator :-
    nl,
    print('==============================================================='), nl,
    print('###############################################################'), nl,
    print('==============================================================='), nl,
    nl.

print_lang_head(ml) :-
    print('Az SML program tesztelése'), nl,
    print('-------------------------'), nl,
    nl.
print_lang_head(pl) :-
    print('A Prolog program tesztelése'), nl,
    print('---------------------------'), nl,
    nl.

print_testcase(N, Limit) :-
    format('~t~d~2+. teszteset, idõlimit = ~|~t~d~3+ sec~n', [N, Limit]),
    print('---------------------------------'), nl.

print_lang_tail(Total, Good) :-
    print('------------------------'), nl,
    bol(Total, Bol),
    format('~t~d~2+ megoldás jó a ~|~t~d~2+-~p.~n', [Good, Total, Bol]),
    print('------------------------'), nl.

explain_status(timeout,_) :-
    nl, print('>>>>> Túllépte az idõkorlátot.'), nl, nl.
explain_status(sizeout,_) :-
    nl, print('>>>>> Túllépte a memóriakorlátot (pl. nagy adatstruktúrák vagy a konzolra kiírt üzenetek miatt)'), nl, nl.
explain_status(signal(Signal),_) :-
    format('~n>>>>> A futás megszakadt, oka ismeretlen (~d signal).~n',
           [Signal]).
explain_status(exception,_) :-
    nl, print('>>>>> Kezeletlen kivétel.'), nl, nl.
explain_status(Status,Time) :-
    (   Time = 'N/A' -> TimeStr = ''
    ;   atom_number(TimeA, Time),
        atom_concat([' ', TimeA, ' sec alatt'], TimeStr)
    ),
    format('~n>>>>> A program lefutott~a, a megoldás ', [TimeStr]),
    (   Status = success
    ->  print('HELYES.')
    ;   print('HIBÁS.')
    ), nl, nl.

monthname(1, január).
monthname(2, február).
monthname(3, március).
monthname(4, április).
monthname(5, május).
monthname(6, június).
monthname(7, július).
monthname(8, augusztus).
monthname(9, szeptember).
monthname(10, október).
monthname(11, november).
monthname(12, december).

bol(Num, Bol) :-                % Num < 100
    Num mod 10 =:= 0, !,
    Tens is (Num // 10) mod 10,
    (   member(Tens, [1,4,5,7,9])
    ->  Bol = 'bõl'
    ;   Bol = 'ból'
    ).
bol(Num, Bol) :-
    Ones is Num mod 10,
    (   member(Ones, [1,2,4,5,7,9])
    ->  Bol = 'bõl'
    ;   Bol = 'ból'
    ).
