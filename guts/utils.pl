% This is a -*- prolog -*- file.
:- module(utils, [
              fail_on_error/1,	% fail_on_error(+Goal)
              fail_on_error/2,	% fail_on_error(+Goal, +Option)
              on_nonsuccess/2,	% on_nonsuccess(+Goal, +Cleanup)
              ensure_success/1,	% ensure_success(+Goal)
              append_lists/2,	% append_lists(?Lists, ?List)
              between/3,		% between(?int, +int, +int)
              stable_sort/2,	% stable_sort(+List, -List)

              atom_number/2,	% atom_number(?Number, ?Atom)
              atom_concat/2,	% atom_concat(+Atoms, -Concatenated)

              split_path/3,		% split_path(+Path, -Dir, -File)
              mkdirhier/1,		% mkdirhier(+Directory)
              read_file/2,		% read_file(+File, -Data)
              write_file/2,		% write_file(+File, +Data)
              get_line/2,		% get_line(+Stream, -CharList)
              read_lines/1,		% read_lines(-Lines)
              read_lines/2,		% read_lines(+File, -Lines)
              write_lines/1,	% write_lines(+Lines)
              write_lines/2,	% write_lines(+File, +Lines)

              directory_exists/1,	% directory_exists(+Directory)
              match/2,              % match(+File, +Pattern)
              match/3,              % match(+File, +Pattern, -Matches)
              lock/1,               % lock(+File)
              unlock/1,             % unlock(+File)
              ext_command/1,        % ext_command(+CommandList)
              ext_command/2,        % ext_command(+CommandList, +Queries)
              relative_file_name/2,	% relative_file_name(+FAbs, -FRel)
              create_soft_link/2,	% create_soft_link(+From, +To)

              warning/2,		% warning(+Format, +Args)
              error/2           % error(+Format, +Args)
          ]).

:- use_module(library(lists)).
:- use_module(library(system)).

%%% fail_on_error(Goal): call Goal and in case of an exception print error
%%% message and fail.
%%% :- pred fail_on_error(+goal).
:- meta_predicate fail_on_error(:).
fail_on_error(Goal) :-
    fail_on_error(Goal, verbose).

%%% fail_on_error(Goal, Option): call Goal and fail in case of an exception.
%%% Behaviour depends on Option.
%%% :- pred fail_on_error(+goal, +term).
:- meta_predicate fail_on_error(:,+).
fail_on_error(Goal, silent) :- !,
    catch(Goal, _, fail).
fail_on_error(Goal, informative) :- !,
    catch(Goal, _, error('~w: goal failed.', [Goal])).
fail_on_error(Goal, verbose) :- !,
    catch(Goal, E, (print_message(error, E), fail)).
fail_on_error(Goal, error(Msg, Args)) :- !,
    catch(Goal, _, error(Msg, Args)).
fail_on_error(Goal, warning(Msg, Args)) :-
    catch(Goal, _, (warning(Msg, Args), fail)).

%%% on_nonsuccess(Goal, Cleanup): call Goal, and if it fails or causes an
%%% exception then call Cleanup before letting on.
%%%  :- pred on_nonsuccess(+goal, +goal).
:- meta_predicate on_nonsuccess(:,:).
on_nonsuccess(Goal, Cleanup) :-
    (   catch(Goal, E, (Cleanup, throw(E))) -> true
    ;   call(Cleanup), fail
    ).

%%% ensure_success(Goal): call Goal exactly once, and succeed in any case.
%%% :- pred ensure_success(+goal).
:- meta_predicate ensure_success(:).
ensure_success(Goal) :-
    (   catch(Goal, _, true) -> true
    ;   true
    ).

%%% append_lists(Lists, List): List is the concatenation of Lists.
%%% :- pred append_lists(?list(list(term)) ?list(term)).
append_lists([], []).
append_lists([L0|Ls], L) :-
    append(L0, L1, L),
    append_lists(Ls, L1).

%%% between(I, N, M): N <= I <= M.
%%% :- pred between(?int, +int, +int).
between(I, N, M) :-
    number(I), !,
    N =< I,
    I =< M.
between(I, N, M) :-
    between0(I, N, M).

between0(N, N, M) :- N =< M.
between0(I, N, M) :-
    N < M,
    N1 is N+1,
    between0(I, N1, M).

%%% stable_sort(List1, List2): The elements of List1 are sorted into the
%%% standard order, yielding List2, any duplicates are preserved.
stable_sort(List1, List2) :-
    findall(X-1, member(X, List1), List1K),
    keysort(List1K, List2K),
    findall(X, member(X-1, List2K), List2).

%%% atom_number(Atom, Number): converts atom Atom to number Number or vice versa.
%%% :- pred atom_number(+atom, -number), atom_number(-atom, +number).
atom_number(Atom, Number) :-
    atom(Atom), !,
    atom_codes(Atom, Codes),
    number_codes(Number, Codes).
atom_number(Atom, Number) :-
    number_codes(Number, Codes),
    atom_codes(Atom, Codes).

%%% atom_concat(Atoms, Atom): atom is the concatenation of all atoms in Atoms.
%%% :- pred atom_concat(+list(atom), -atom).
atom_concat([], '').
atom_concat([Atom0|Atoms], Atom) :-
    atom_concat0(Atoms, Atom0, Atom).

atom_concat0([], Atom, Atom).
atom_concat0([H|T], A0, A) :-
    atom_concat(A0, H, A1),
    atom_concat0(T, A1, A).

%%% split_path(FullPath, Dir, FileName): FullPath is Dir/FileName.
%%% :- pred split_path(atom, atom, atom).
%%% :- mode split_path(+, -, -), split_path(-, +, +).
split_path(Full, Dir, File) :-
    atom(Full), !,
    atom_codes(Full, FullC),
    reverse(FullC, FullR),
    (   append(FileR, [0'/|DirR], FullR)
    ->  (    DirR = [] -> DirC = [0'/]
        ;	reverse(DirR, DirC)
        ),
        reverse(FileR, FileC)
    ;   DirC = [0'.],
        FileC = FullC
    ),
    atom_codes(Dir, DirC),
    atom_codes(File, FileC).
split_path(Full, Dir, File) :-
    join_path(Dir, File, Full).

join_path(Dir, File, Full) :-
    (   Dir = '' ; Dir = '.' ; Dir = './'
    ), !,
    Full = File.
join_path(Dir, File, Full) :-
    (   atom_concat(_, '/', Dir)
    ->  atom_concat(Dir, File, Full)
    ;   atom_concat([Dir, '/', File], Full)
    ).

%%% mkdirhier(D): make directory hierarchy leading to D.
%%% :- pred mkdirhier(+atom).
mkdirhier(D) :-
    directory_exists(D), !.
mkdirhier(D) :-
    atom_codes(D, DC),
    (   append(D0C, [0'/|_], DC),
        \+ D0C = []
    ;   D0C = DC
    ),
    atom_codes(D0, D0C),
    (   directory_exists(D0) -> true
    ;   catch(make_directory(D0),_,fail) -> true
    ;   !
    ),
    fail.
mkdirhier(_).

%%% read_file(F, T): reads a list of terms T from the file F.
%%% :- pred read_file(+atom, -term).
read_file(F, T) :-
    fail_on_error(open(F, read, S)),
    call_cleanup((read(S, T0), read_more_terms(S, T0, T)),
                 close(S)).

read_more_terms(_, end_of_file, T) :- !, T = [].
read_more_terms(S, T0, [T0|T]) :-
    read(S, T1),
    read_more_terms(S, T1, T).

%%% write_file(F, T): writes a list of terms T to the file F.
%%% :- pred write_file(+atom, +term).
write_file(F, T) :-
    fail_on_error(open(F, write, S)),
    call_cleanup(write_terms(S, T), close(S)).

write_terms(S, TL) :-
    member(T, TL),
    format(S, '~q.~n', [T]),
    fail.
write_terms(_,_).

%%% get_line(S, CL): CL is the string read up to the end of the line from S.
%%% If reading past end of file, returns 'end_of_file' in CL first, raises
%%% an exception second time.
%%% :- pred get_string(+stream, -list(int)).
get_line(S, CL) :-
    peek_code(S, C),
    (   C = -1
    ->  get_code(S, _),
        CL = end_of_file
    ;   get_line(S, C, CL)).

get_line(_, -1, CL) :- !, CL = [].	% leave end of file mark on stream
get_line(S, 0'\n, CL) :- !,
    get_code(S, _),
    CL = [].
get_line(S, C, [C|CL]) :-
    get_code(S, _),
    peek_code(S, NC),
    get_line(S, NC, CL).

%% read_lines(L): reads lines from current input to L.  L is a list of list
%% of character codes, newline characters are not included.
%% :- pred read_lines(-list(list(char))).
read_lines(L) :-
    current_input(In),
    get_line(In, L0),
    read_lines(In, L0, L).

%% read_lines(F, L): reads lines from F to L.  L is a list of list of character
%% codes, newline characters are not included.
%% :- pred read_lines(+atom, -list(list(char))).
read_lines(F, L) :-
    fail_on_error(open(F, read, S)),
    call_cleanup((get_line(S, L0),
                  read_lines(S, L0, L)),
                 close(S)).

read_lines(_, end_of_file, L) :- !, L = [].
read_lines(S, H, [H|T]) :-
    get_line(S, NH),
    read_lines(S, NH, T).

%% write_lines(L): writes lines L to current output.  L is a list of list
%% of character codes, newline characters are not included.
%% :- pred read_lines(+list(list(char))).
write_lines(L) :-
    current_output(Out),
    write_lines0(Out, L).

%% write_lines(F, L): writes lines in L to F.  L is a list of list of character
%% codes, newline characters are not included.
%% :- pred write_lines(+atom, +list(list(char))).
write_lines(F, L) :-
    fail_on_error(open(F, write, S)),
    call_cleanup(write_lines0(S, L),
                 close(S)).

write_lines0(S, Lines) :-
    member(L, Lines),
    format(S, '~s~n', [L]),
    fail.
write_lines0(_,_).

%%% directory_exists(D): D is an existing directory.
%%% :- pred directory_exists(+atom).
directory_exists(D) :-
    %	file_exists(D, [read,write]),	% FIXME: causes mkdirhier to fail!
    file_exists(D, [exists]).
%	file_property(D, type(directory)). % FIXME: causes mkdirhier to fail for softlinks!

%%% match(File, Pattern): Pattern wildcard pattern matches file name File.
%%% :- pred match(+atom, +atom).
match(File, Pattern) :-
    match(File, Pattern, _).

%%% match(File, Pattern, Matches): Pattern wildcard pattern matches file
%%% name File.  Matches is a list of atoms matching the wildcards.
%%% :- pred match(+atom, +atom, -list(atom)).
match(File, Pattern, Wildcards) :-
    atom_codes(File, FileC),
    atom_codes(Pattern, PatternC),
    pattern_match(PatternC, FileC, Wildcards0), !,
    findall(W, (member(W0, Wildcards0), atom_codes(W, W0)), Wildcards).

pattern_match(Pattern, Name, Wildcards) :-
    append(FrontP, [0'*|RearP], Pattern), !,
    pattern_match0(FrontP, FrontN, FrontW),
    append_lists([FrontN,Any,RearN], Name),
    append(FrontW, [Any|RearW], Wildcards),
    pattern_match(RearP, RearN, RearW).
pattern_match(Pattern, Name, Wildcards) :-
    pattern_match0(Pattern, Name, Wildcards).

pattern_match0(Pattern, Name, [[Any]|Wildcards]) :-
    append(FrontN, [0'?|RearP], Pattern), !,
    pattern_match0(RearP, RearN, Wildcards),
    append(FrontN, [Any|RearN], Name).
pattern_match0(Name, Name, []).

%%% lock(F): creates a lockfile on F. (The directory of F must be writable.)
%%% :- pred lock(+atom).
lock(F) :-
    lockfile(F, LockF),
    atom_concat([lockfile, ' \'', LockF, '\''], Cmd),
    exec(Cmd, [null, null, pipe(S)], PID),
    call_cleanup((	 wait(PID, 0) -> true
                 ;	 get_line(S, ErrorC),
                     atom_codes(Error, ErrorC),
                     throw(lockfile(Error))
                 ), close(S)).

%%% unlock(F): removes lockfile from F.
%%% :- pred unlock(+atom).
unlock(F) :-
    lockfile(F, LockF),
    delete_file(LockF, []).

lockfile(F, LockF) :-
    atom_concat(F, '.lock', LockF).

%%% ext_command(Cmd) = ext_command(Cmd, 0).
%%% :- pred ext_command(+atom), ext_command(+list(atom)).
ext_command(Cmd) :- ext_command(Cmd, [status(0)]).

%%% ext_command(Cmd, Queries): Cmd is an atom or a list of atoms: the head
%%% is the command to execute, the rest is the argument list.  Queries is a
%%% list of queries regarding the run.
%%% :- pred ext_command(+atom, +list(term)), ext_command(+list(atom), +list(term)).
ext_command(Cmd, Queries) :-
    atom(Cmd), !,
    ext_command([Cmd], Queries).
ext_command(Cmd, Queries) :-
    select(time(Real,Usr,Sys), Queries, Queries1), !, % measure time!
    time(Cmd, Queries1, Real, Usr, Sys).
ext_command([Cmd|Args], Queries) :-
    ext_command(Args, Cmd, Queries).

ext_command([Arg|Args], Cmd, Queries) :-
    (   Arg = q(Arg1)
    ->  atom_concat([Cmd, ' \'', Arg1, '\''], Cmd1)
    ;   Arg = qq(Arg1)
        ->  atom_concat([Cmd, ' "', Arg1, '"'], Cmd1)
    ;   atom_concat([Cmd, ' ', Arg], Cmd1)
    ),
    ext_command([Cmd1|Args], Queries).
ext_command([], Cmd, Queries) :-
    ensure_success(memberchk(status(Status), Queries)),
    ensure_success(memberchk(cmdline(Cmd), Queries)),
    system(Cmd, Status0),
    Status is Status0 // 256.

%%% time(Cmd, Queries, Real, Usr, Sys): runs command like ext_command/2
%%% but also measures used CPU time and returns results in third argument.
%%% :- pred time(+list(atom), +list(term), -num, -num, -num).
time(Cmd, Queries, Real, User, System) :-
    mktemp('/tmp/time.XXXXXX', TimeF),
    call_cleanup((ext_command(['/usr/bin/time', '-o', q(TimeF), '-p'|Cmd], Queries),
                  read_lines(TimeF, Lines),
                  memberchk([0'r,0'e,0'a,0'l|RealC], Lines),
                  memberchk([0'u,0's,0'e,0'r|UserC], Lines),
                  memberchk([0's,0'y,0's|SysC], Lines),
                  number_codes(Real, RealC),
                  number_codes(User, UserC),
                  number_codes(System, SysC)),
                 delete_file(TimeF, [ignore])).

%%% relative_file_name(FAbs, FRel): FRel is the relative path of FAbs with
%%% respect to the current working directory.
%%% :- pred relative_file_name(+atom, -atom).
relative_file_name(FAbs, FRel) :-
    working_directory(Dir0, Dir0),
    split_path(FAbs, FDir0, FName),
    atom_codes(Dir0, Dir0C),
    atom_codes(FDir0, FDir0C),
    drop_common_root(Dir0C, FDir0C, Dir1C, FDir1C),
    atom_codes(Dir1, Dir1C),
    atom_codes(FDir1, FDir1C),
    relative_directory(Dir1, FDir1, FDir),
    split_path(FRel, FDir, FName).

drop_common_root([], [], DirA, DirB) :- !,
    DirA = [],
    DirB = [].
drop_common_root(DirA0, DirB0, DirA, DirB) :-
    drop_root(DirA0, HeadA, DirA1),
    drop_root(DirB0, HeadB, DirB1),
    HeadA = HeadB, !,
    drop_common_root(DirA1, DirB1, DirA, DirB).
drop_common_root(DirA, DirB, DirA, DirB).

drop_root(Dir0, Head, Dir) :-
    (   append(Head, [0'/|Dir], Dir0)
    ->  true
    ;   Head = Dir0, Dir = []
    ).

relative_directory('', DirB, Dir) :- !,
    Dir = DirB.
relative_directory('.', DirB, Dir) :- !,
    Dir = DirB.
relative_directory(DirA0, DirB0, Dir) :-
    split_path(DirA0, DirA, _),
    (   DirB0 = ''
    ->  DirB = '..'
    ;   atom_concat('../', DirB0, DirB)
    ),
    relative_directory(DirA, DirB, Dir).

%%% create_soft_link(From, To): creates a soft link from From to To (latter
%%% could either be a filename or a directory).  The link is created as
%%% relative as possible.
%%% :- pred create_soft_link(+atom, +atom).
create_soft_link(From, To) :-
    (   directory_exists(To)
    ->  working_directory(OldD, To),
        ToF = '.'
    ;   split_path(To, Dir, ToF),
        working_directory(OldD, Dir)
    ),
    relative_file_name(From, FromRel),
    call_cleanup(ext_command([ln, '-s', q(FromRel), q(ToF)]),
                 working_directory(_, OldD)).

%%% warning(Txt, Args): The format Txt with Args as an argument list is
%%% printed as a warning.
%%% :- pred warning(+atom, +list(term)).
warning(Txt, Args) :-
    print_message(warning, format(Txt, Args)).

%%% error(Txt, Args): The format Txt with Args as an argument list is
%%% printed as a warning. Always fails.
%%% :- pred error(+atom, +list(term)).
error(Txt, Args) :-
    print_message(error, format(Txt, Args)), fail.
