% This is a -*- prolog -*- file.
:- module(eval, [test/1,test/3,variant/2,sorted_variant/2]).
:- initialization main.

:- assert(user:portray_message(informational, _)).

:- use_module(library(lists)).
:- use_module(library(terms)).
:- use_module(refsol).

% Használat
% sicstus -l eval -a khfx teszt.pl [output]

% main/0
% A modul betöltődése után automatikusan meghívódik.
main :-
    catch(action, E, true),
    (   var(E) -> halt(0)
    ;   print_message(error,E), halt(1)
    ).

% action/0
% Ellenőrzi a parancssori paramétereket és meghívja test/3-at.
action :-
    prolog_flag(argv, Args),
    (   Args = [ProgF, TestF, ResF] -> true
    ;   format(user_error, 'Hibás argumentumszám: ~w~n', [Args]), halt(1)
    ),
    test(ProgF, TestF, ResF).

% test(+ProgFile, +TestFile, +ResultFile)
% Betölti ProgFile-t, majd TestFile minden tesztjét végrehajtja,
% az eredmény(eke)t jelző 0/1-eket pedig ResultFile-ba írja.
test(ProgF, TestF, ResF):-
    load_files('$module_being_tested':ProgF),
    open_file(ResF, write, ResS),
    call_cleanup(test_to_stream(TestF, ResS), close(ResS)).

% test(+TestFile)
% Belső teszteléshez: ellenőrzi a TestFile-ból beolvasható összes
% tesztesetet.
test(TestF) :-
    open_null_stream(Null),
    call_cleanup(test_to_stream(TestF, Null), close(Null)).

% test_to_stream(+TestFile, +ResultStream)
% TestFile minden tesztjét végrehajtja, az eredmény(eke)t jelző 0/1-eket
% pedig a ResultStream-re írja.
test_to_stream(TestF, ResS) :-
    open_file(TestF, read, TestS),
    call_cleanup(test_fromto_stream(TestS, ResS), close(TestS)).

% test_fromto_stream(+TestStream, +ResultStream)
% Végrehajtja a TestStream-ről beolvasható teszteket, az eredmény(eke)t
% jelző 0/1-eket pedig a ResultStream-re írja.
test_fromto_stream(TestS, ResS) :-
    repeat,
    read_term(TestS, Term, [variable_names(VarNames)]),
    (   Term = end_of_file -> !
    ;   test_term(Term, VarNames, ResS),
        fail
    ).

% test_term(+Term, +VarNames, +ResS)
% A Term term által leírt tesztesetet teszteli, az eredményt a ResS
% folyamra írja.  Ha Term nem tesztet ad meg, vagy a teszthívás hibát
% jelez, a hibaüzenetet ad, de sikerül.
test_term(Vars^Goal=Sols, VarNames, ResS) :- !,
    catch(test(Vars, Goal, Sols, VarNames, Result), E,
          test_exception(Goal, VarNames, E, Result)),
    format(ResS, '~w~n', [Result]).
test_term(Term, _, _) :-
    format(user_error, 'Rosszul formált teszteset: ~w~n', [Term]).

% test_exception(+Goal, +VarNames, +Exception, -Result)
% Kezeli a teszt során felmerülő futási hibát.
test_exception(Goal, VarNames, E, _Result) :-
    substitute_varnames(VarNames),
    format(user_error, 'A ~w teszt meghiúsult, mert:~n', [Goal]),
    print_message(error, E),
    fail.
test_exception(_Goal, _VarNames, _Exception, e).

% test(+Vars, +Goal, +Sols, +VarNames, -Result)
% Végrehajtja a tesztet.
test(Vars, Goal, Sols0, VarNames, Result) :-
    test_option(Sols0, Vars, Goal, Sols, Option),
    get_solutons('$module_being_tested':Goal, Vars, SolsGot),
    show_test(Vars, Goal, Sols, VarNames),
    show_result(Vars, SolsGot, VarNames),
    nl,
    %print(compare_result(Option,Sols,SolsGot)), nl,
    (   compare_result(Option, Sols, SolsGot)
    ->  Result = 1 %% HP, write('Helyes megoldás.\n'), nl %%%% pts %%%%
    ;   Result = 0 %% HP, write('Helytelen megoldás.\n'), nl %%%% pts %%%%
    ).

% test_option(+OptionAndSolutions, -Solutions, -Option)
test_option(refsol, Vars, Goal, Sols, Opt) :- !, %%%% pts %%%%
    Opt = variant,
    findall(Vars, refsol:Goal, Sols). %%%% pts %%%% a referenciamegoldás meghívása
test_option(refsol_sorted, Vars, Goal, Sols, Opt) :- !, %%%% pts %%%%
    Opt = sorted_variant,
    findall(Vars, refsol:Goal, Sols0), %%%% pts %%%% a referenciamegoldás meghívása
    mysort(Sols0, Sols).
test_option(refsol_sorted_items, Vars, Goal, Sols, Opt) :- !, %%%% pts %%%%
    Opt = sorted_items_variant,
    findall(Vars, refsol:Goal, Sols0), %%%% pts %%%% a referenciamegoldás meghívása
    mysort_items(Sols0, Sols).
test_option(Opt0:Sols0, _Vars, _Goal, Sols, Opt) :- !,
    Sols = Sols0,
    Opt = Opt0.
test_option(Sols, _Vars, _Goal, Sols, variant).

compare_result(Pred0, Sols, SolsGot) :-
    Pred0 =.. FArgs0,
    append(FArgs0, [Sols,SolsGot], FArgs),
    Goal =.. FArgs,
    eval:Goal. %%%% pts %%%% user: -> eval:

sorted_variant(Sols, SolsGot) :-
    mysort(SolsGot, SortedSolsGot),
    variant(Sols, SortedSolsGot).

%%%% pts %%%%
sorted_items_variant([], []).
sorted_items_variant([Sol|Sols], [SolGot|SolsGot]) :-
    mysort(SolGot, SortedSolGot),
    variant(Sol, SortedSolGot),
    sorted_items_variant(Sols, SolsGot).

%%%% pts %%%%
mysort_items([], []).
mysort_items([Sol|Sols], [SortedSol|SortedSols]) :-
    mysort(Sol, SortedSol),
    mysort_items(Sols, SortedSols).


mysort([], []).
mysort([X|L0], SL) :-
    mysort(L0, SL0),
    insert(X, SL0, SL).

insert(X, [Y|L0], [Y|L]) :-
    X @> Y, !,
    insert(X, L0, L).
insert(X, L, [X|L]).

% get_solutons(+Goal, +Vars, -SolGot)
get_solutons(Goal, Vars, SolsGot) :-
    findall(Vars, call_residue_vars(Goal,_), SolsGot).

show_test(Vars, Goal, Sols, VarNames) :-
    substitute_varnames(VarNames),
    substitute_vars_as_void(Goal+Sols),
    (   Vars == [] -> % no vars
        conv_sols(Sols, Expd),
        format('Cél: ~q~n~w vártam', [Goal,Expd])
    ;   format('Cél: ~q~n~w értékeit keressük.~n', [Goal,Vars]),
        show_solution_list(Sols, 'vártam')
    ),
    put_code(0',),
    fail.
show_test(_, _, _, _).

substitute_varnames([]).
substitute_varnames([Name=Var|NVs]) :-
    (   Var = '$VAR'(Name) -> true
    ;   true
    ),
    substitute_varnames(NVs).

substitute_vars_as_void(Term) :-
    term_variables(Term, Vars),
    substitute_void(Vars).

substitute_void([]).
substitute_void([Var|Vs]) :-
    Var = '$VAR'('_'),
    substitute_void(Vs).


show_result(Vars, SolsGot, VarNames) :-
    substitute_varnames(VarNames),
    numbervars(SolsGot, 0, _),
    (   Vars == [] -> % no vars
        conv_sols(SolsGot, Got),
        format('~n~w kaptam', [Got])
    ;   nl, show_solution_list(SolsGot, 'kaptam')
    ),
    put_code(0'.), nl,
    fail.
show_result(_, _, _).

show_solution_list([], Post) :- !,
    format('Meghiúsulást ~w', [Post]).
%show_solution_list([X], Post) :-
%	!,
%	format('1 megoldást ~w: ~q', [Post,X]). %%%% pts %%%% ~w' -> ~q'
show_solution_list(L, Post) :-
    length(L,H),
    %format('~w megoldást ~w', [H,Post]).
    list2commaseparated(L,Lc),
    format('~w megoldást ~w: ~q', [H,Post,Lc]). %%%% pts %%%% ~p' -> ~q'

list2commaseparated([X],X) :- !.
list2commaseparated([X|L],(X,L1)) :-
    list2commaseparated(L,L1).

conv_sols([], 'Meghiúsulást') :- !.
conv_sols([_], 'Sikert') :- !.
conv_sols(L, N-R) :-
    length(L, N),
    szeres(N, Sz),
    atom_concat(Sz, ' sikert', R).

szeres(N, Sz) :-
    N1 is N mod 10,
    N2 is N // 10,
    (   N1 =\= 0
    ->  szeres0(N1, Sz)
    ;   szeres1(N2, Sz)
    ).

szeres0(E, Sz) :-
    (   E == 5
    ->  Sz = 'szörös'
    ;   memberchk(E, [1,2,4,7,9])
        ->  Sz = 'szeres'
    ;   Sz = 'szoros'
    ).

szeres1(T, Sz) :-
    (   memberchk(T, [10,40,50,70,90])
    ->  Sz = 'szeres'
    ;   Sz = 'szoros'
    ).


% open_file(+File, +Mode, -Stream):
% A File allomanyt megnyitja a Stream folyam formajaban Mode modban.
open_file(user, M, S) :-
    !, standard_stream(M, S).
open_file(File, Mode, Stream) :-
    open(File, Mode, Stream).

% standard_stream(?Mode, ?Stream):
% A Mode modu szabvanyos folyam a Stream.
standard_stream(read, user_input).
standard_stream(write, user_output).
