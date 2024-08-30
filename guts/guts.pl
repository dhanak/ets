% This is a -*- prolog -*- file.
:- module(guts, [main/0]).

:- use_module(library(file_systems)).
:- use_module(library(lists)).
:- use_module(library(process)).
:- use_module(library(system)).
:- use_module(library(codesio)).

:- use_module(utils).
:- use_module(log).
:- use_module(db).
:- use_module(templates).

:- dynamic testcase_count/1.

%%%
%%% Directory configuration.
%%%

%% directory(+Key, -Dir): Dir is the path associated with key Key.
directory(Key, Dir) :-
    atom(Key), !,			% atomic keys are cached
    (   bb_get(Key, Dir) -> true
    ;   directory0(Key, Dir),
        bb_put(Key, Dir)
    ).
directory(Key, Dir) :-
    directory0(Key, Dir).

directory0(work,       D) :- environ('GUTS_WORK_DIR', D).
directory0(env0,       D) :- subdirectory(work, 'env', D).
directory0(env_var,    D) :- current_class_subdirectory(env0, D).
directory0(env(Lang),  D) :- atom(Lang), subdirectory(env_var, Lang, D).
directory0(spools,     D) :- subdirectory(work, 'spools', D).
directory0(hwks0,      D) :- subdirectory(work, 'hwks', D).
directory0(hwks,       D) :- current_class_subdirectory(hwks0, D).
directory0(hwks_mails, D) :- subdirectory(hwks, 'mails', D).
directory0(hwks_rep,   D) :-
    testID(_,Suite),
    split_path(D0, 'reports', Suite),
    subdirectory(hwks, D0, D).
directory0(student(Name),  D) :-
    ground(Name),
    subdirectory(hwks, Name, D).

%% subdirectory(+BaseKey, +SubDir, -Dir): Dir is the subdirectory called
%% SubDir of the directory associated with key BaseKey.
subdirectory(Base, SubDir, Dir) :-
    directory(Base, BaseDir),
    split_path(Dir, BaseDir, SubDir).

%% current_class_subdirectory(+BaseKey, -Dir): Dir is the subdirectory belonging
%% to the current test class in the directory associated with key BaseKey.
current_class_subdirectory(Base, D) :-
    semester(Semester),
    testID(Class,_),
    split_path(D0, Semester, Class),
    subdirectory(Base, D0, D).

%%%
%%% File configuration.
%%%

%% filename(+Key, -Path): Path is the path of the file associated with key Key.
filename(spoolfile, F) :-
    directory(spools, SpD),
    testID(Class, Suite),
    atom_concat([Class, '.', Suite], Sp),
    split_path(F, SpD, Sp).
filename(mail(Name,Version), F) :-
    directory(hwks_mails, MailD),
    atom_number(VersionA, Version),
    atom_concat([MailD, '/', Name, '.', VersionA], F).
filename(report(Name), F) :-
    directory(hwks_rep, RepD),
    split_path(F, RepD, Name).
filename(report(Name,Lang), F) :-
    filename(report(Name), F0),
    atom_concat([F0, '.', Lang], F).
filename(setup(Lang), F) :-
    directory(env(Lang), LangD),
    split_path(F, LangD, 'setup').
filename(timeguard, F) :-
    environ('GUTS_ROOT', Root),
    split_path(F, Root, 'timeguard').

%% testfiles(SubDir, Dir, Files): Files is a list of file names in the SubDir
%% test directory of the current test class.  Dir is the full path of the
%% directory.
testfiles(SubDir, Dir, Files) :-
    subdirectory(env_var, SubDir, Dir),
    findall(K-F, (file_member_of_directory(Dir, F, _),
                  match(F, 'test*d.txt', [K0]),
                  atom_number(K0, K)), Files1),
    keysort(Files1, Files2),
    findall(F, member(_-F, Files2), Files).

%%%
%%% Miscellanious tools.
%%%

%% split_name(+Full, -Name, -Neptun): Full = Name @ [0.'] @ Neptun
split_name(Full, Name, Neptun) :-
    atom_codes(Full, FullC),
    append(NameC, [0'.|NeptunC], FullC), !,
    atom_codes(Name, NameC),
    atom_codes(Neptun, NeptunC).

%% delete_files(+Patterns): all files matching Patterns are deleted.
delete_files([]) :- !.
delete_files(Patterns) :-
    process_create(path(sh), ['-c', 'rm'|Patterns], [wait(exit(0))]).

%% link_files(+FromDir, +Files): files Files from the directory FromDir are
%% soft linked to the current directory, with the shortest possible relative
%% path.
link_files(FromDir, Files) :-
    member(File, Files),
    split_path(From, FromDir, File),
    create_soft_link(From, '.'),
    fail.
link_files(_, _).

%% exitcode_to_status(+Code, -Status): Status is a term describing exit
%% code Code.
exitcode_to_status( 152, Status) :- !, Status = timeout.
exitcode_to_status( 153, Status) :- !, Status = sizeout.
exitcode_to_status(   0, Status) :- !, Status = ok.
exitcode_to_status(Code, Status) :-
    (   Code >= 128
    ->  Signal is Code - 128, Status = signal(Signal)
    ;   Status = exception
    ).

%% predicate_member(+Head, +List, -Body): Succeeds exactly once if a
%% predicate with head Head is spreaded in the list.  Bodies concatenated
%% with ';' are collected into Body.
predicate_member(Head, [(Head :- Body0)|Rest], Body) :- !,
    (   predicate_member(Head, Rest, Body1)
    ->  Body = (Body0 ; Body1)
    ;   Body = Body0
    ).
predicate_member(Head, [_|Rest], Body) :-
    predicate_member(Head, Rest, Body).

%% read_testfile(+File, -Tests): reads all test lines from file File.
read_testfile(File, Tests) :-
    directory(env_var, Dir),
    split_path(Path, Dir, File),
    read_lines(Path, Tests0),
    findall(T, (member(T, Tests0),
                \+ prefix("%", T), % ignore comments
                \+ prefix("(*", T),
                \+ prefix("#", T),
                \+ prefix("//", T),
                \+ T = []),	% ignore empty lines
            Tests).

%% read_mail_ID(-ID): ID is the mail file name in the current test directory.
read_mail_ID(ID) :-
    process_create(path(readlink), ['-f', mail],
                   [wait(exit(0)),stdout(pipe(Out))]),
    read_line(Out, Line),
    close(Out),
    atom_codes(Path, Line),
    split_path(Path, _, ID).

%% safe_testclass
safe_testclass(Class, LongName, Suite, PeriodBeg, PeriodEnd, Opts) :-
    on_nonsuccess(testclass(Class, LongName, Suite,
                            PeriodBeg, PeriodEnd, Opts),
                  warning('No test specification found for ~w', [Class])).

%%%
%%% Spool handling predicates.
%%%

%% spool_transaction(-SpoolFile, +Goal)
:- meta_predicate spool_transaction(:).
spool_transaction(SpoolF, Goal) :-
    filename(spoolfile, SpoolF),
    lock(SpoolF),
    call_cleanup(Goal, unlock(SpoolF)).

%% lookup_job(-Job): Job is the next job in the current spool file.  Fails if
%% there aren't any jobs.
lookup_job(Job) :-
    spool_transaction(F, read_file(F, [Job|_])).

%% unspool_job: the next job is removed from the spool file.
unspool_job :-
    spool_transaction(F, (read_file(F, [_|Jobs]),
                          write_file(F, Jobs))).

%% spool_job(+Name, +Version, -Position, -Running): Spool a new job for
%% Name with Version.  Position can either be new(Pos) if no older job for
%% same Name was found, replace(Pos) if an older job was replaced.  Pos
%% always means the absolute position of the new job.  If the currently
%% processed job also belongs to Name, Running stores its version number,
%% otherwise Running is 0.
spool_job(Name, Version, Position, Running) :-
    spool_transaction(F, spool_job(F, Name, Version, Position, Running)).

%% spool_job(+File, +Name, +Version, -Position, -Running): same as above,
%% File is the spoolfile.
spool_job(F, Name, Version, Position, Running) :-
    read_file(F, Jobs0),
    Jobs0 = [FirstJob|Jobs1], !,
    add_job(Jobs1, 2, Name, Version, Jobs, Position),
    ( auto_job(FirstJob, Name, Running) -> true ; Running = 0 ),
    write_file(F, [FirstJob|Jobs]).
spool_job(F, Name, Version, Position, 0) :-
    add_job([], 1, Name, Version, Jobs, Position),
    write_file(F, Jobs).

%% add_job(+Jobs0, +CurPos, +Name, +Version, -Jobs, -Position): adds or
%% replaces Name-Version job to a jobs list.
add_job([], I, Name, Version, [Job], new(I)) :-
    auto_job(Job, Name, Version).
add_job([Job0|Jobs0], I, Name, Version, Jobs, Position) :-
    auto_job(Job0, Name, _), !,
    auto_job(Job, Name, Version),
    Jobs = [Job|Jobs0],
    Position = replace(I).
add_job([Job|Jobs0], I, Name, Version, [Job|Jobs], Position) :-
    I1 is I+1,
    add_job(Jobs0, I1, Name, Version, Jobs, Position).

%% auto_job(?Job, ?Name, ?Version): Job is an auto-spooled job for Name
%% with Version.
auto_job(job(Name, [extract(Version),mail]), Name, Version).

%% next_job(-Job): Job is the next job to do.  Waits until there is such a job.
next_job(Job) :-
    lookup_job(Job), !.
next_job(Job) :-
    sleep(60),
    next_job(Job).

%%%
%%% Evaluator:  Compares a solution with the reference solution.
%%%

%% evaluate(+Ref, +Sol, -Result): Result is the result of comparing solution
%% stored in file Sol with reference solution stored in file Ref.
evaluate(RefF, SolF, Result) :-
    (   evaluate(RefF, SolF)
    ->  Result = success
    ;   Result = failure
    ).

%% evaluate(+Ref, +Sol): Succeeds if solution stored in file Sol is the same
%% as the reference solution stored in file Ref.
evaluate(RefF, SolF) :-
    evaluator(Eval),
    compare_with(Eval, RefF, SolF).

%% compare_with(+Method, +Ref, +Sol): Succeeds if solution stored in file Sol
%% is the same as the reference solution stored in file Ref, compared with
%% method Method.
compare_with(builtin, RefF, SolF) :-
    compare(RefF, SolF).
compare_with(separate(EvalPrg), RefF, SolF) :-
    run(file(EvalPrg), [file(RefF), file(SolF)]).
compare_with(embedded, _, SolF) :-
    open(SolF, read, S),
    call_cleanup(get_code(S, 0'1), close(S)).

%% compare(+Ref, +Sol): Succeeds if terms stored in file Sol and in file Ref
%% are both lists of equal length, and the sorted version of the former is
%% equal to the latter.
compare(RefF, SolF) :-
    fail_on_error(read_file(RefF, [Ref])),
    fail_on_error(read_file(SolF, [Sols])),	% file size limit may cause incomplete parse
    same_length(Sols, Ref),	% to avoid sort if unnecessary
    stable_sort(Sols, SSols),
    SSols == Ref.

%%%
%%% Mail handling predicates (extractor, purger, spooler, sender).
%%%

%% extract(+Name +Version, +WorkDir, +Report): the file Name of Version is
%% extracted to the directory WorkDir.  WorkDir is created from the
%% scratch.  If something goes wrong, fails and produces a report.  For the
%% values of Report, see produce_report/4.
extract(Name, Version, WorkDir, Report) :-
    filename(mail(Name,Version), MailF), % check mail file
    on_nonsuccess(file_exists(MailF, read),
                  log('Extract failed: mailfile ~w is not readable', [MailF])),
    (   file_exists(WorkDir)	% reset working directory
    ->  delete_file(WorkDir)
    ;   true
    ),
    make_directory(WorkDir),
    split_path(MailL, WorkDir, 'mail'), % create soft link
    log('Linking mail file ~w to ~w', [MailF,MailL]),
    create_soft_link(MailF, MailL),
    split_path(LogF, WorkDir, 'extract.log'), % open logfile
    tell(LogF),
    log('Unpacking mail'),
    (   call_cleanup(unpack(MailL, WorkDir), told)
    ->  delete_file(LogF)
    ;   log('Unpacking mail failed, sending report'),
        produce_report(Report, Name, Subject,
                       templates:ill_formed_mail(Subject, LogF)),
        fail
    ).

%% extract(+Name, +WorkDir, +Report): the latest version of file Name is
%% extracted, see extract/3.
extract(Name, WorkDir, Report) :-
    find_latest_version(Name, Version),
    log('Latest version is ~w', [Version]),
    extract(Name, Version, WorkDir, Report).

%% find_latest_version(+Name, -Version): Version is the latest version of the
%% mail files called Name.
find_latest_version(Name, Version) :-
    directory(hwks_mails, MailD),
    findall(V,
            (file_member_of_directory(MailD, F, _), file_version(F, Name, V)),
            Versions),
    max_member(Version, Versions).

%% file_version(?File, ?Name, ?Version): File is called Name and has Version.
file_version(File, Name, Version) :-
    atom(File), !,
    \+ File = '.',
    \+ File = ..,
    atom_codes(File, FileC),
    (   append(NameC, [0'.|VerC], FileC),
        fail_on_error(number_codes(Version, VerC), silent)
    ->  atom_codes(Name, NameC)
    ;   log_warning('Illegally formatted mail file name ~w', [File]),
        fail
    ).
file_version(File, Name, Version) :-
    atom_number(VersionA, Version),
    atom_concat([Name, '.', VersionA], File).

%% unpack(+File, +WorkDir): file MailF is extracted to WorkDir.
unpack(MailF, WorkDir) :-
    process_create(path(uudecode), [file(MailF)], [wait(exit(0)),cwd(WorkDir)]).

%% purge(+Directory): purges mail directory Directory.
purge(Dir) :-
    findall(F,
            (file_member_of_directory(Dir, F, _), file_version(F,_,_)),
            Files),
    working_directory(OldD, Dir),
    purge0(Files),
    working_directory(_, OldD).

%% purge0(+Files): purge Files in current directory.
purge0([]) :- !.
purge0(Files) :-
    select_mails(Files, _, Mails0, Rest),
    keysort(Mails0, Mails1),
    reverse(Mails1, [_|Mails2]),
    findall(M, member(_-M, Mails2), OldMails),
    delete_files(OldMails),
    purge0(Rest).

%% select_mails(+All, ?Name, -Mails, -Rest): selects mails from Name into
%% Mails the rest is Rest.
select_mails([H|T], Name, Mails, Rest) :-
    file_version(H, Name, V), !,
    Mails = [V-H|Mails1],
    select_mails(T, Name, Mails1, Rest).
select_mails([H|T], Name, Mails, [H|Rest]) :-
    select_mails(T, Name, Mails, Rest).
select_mails([], _, [], []).

%% read_mail(-Mail, -Name, -Class): reads Mail from standard input, and
%% unifies Name and Class with the sender's name and the test class encoded
%% in the mail respectively.
read_mail(Mail, Name, Class) :-
    read_lines(Mail),
    append("Sender-Full-Name: ", NameL, NameLine),
    append("Test-Class: ", ClassL, ClassLine),
    memberchk(NameLine, Mail),
    memberchk(ClassLine, Mail),	% if these fail, we silently fail
    atom_codes(Name, NameL),
    atom_codes(Class, ClassL).

%% store_mail(+Mail, +Name, -Version): stores Mail as newest mail with
%% Version for student Name.
store_mail(Mail, Name, Version) :-
    directory(hwks_mails, MailD),
    mkdirhier(MailD),
    (   find_latest_version(Name, LastVersion)
    ->  Version is LastVersion + 1
    ;   Version = 1
    ),
    filename(mail(Name,Version), MailF),
    write_lines(MailF, Mail).

%% spool_mail(+Name, +Version, +ClassName, SendMail): spool Mail of Version
%% in the appropriate spool file and notify student in mail.  ClassName is
%% the long name of the test class.  For the values of Report, see
%% produce_report/4.
spool_mail(Name, Version, ClassName, Report) :-
    touch_spoolfile,
    spool_job(Name, Version, Pos, Running),
    produce_report(Report, Name, Subject,
                   templates:mail_spooled(Subject, ClassName, Version,
                                          Pos, Running)).

%% produce_report(+Report, +Name, +Subject, +PrintReport): PrintReport is a
%% goal printing a report.
%%  * Report = mail:   the report is emailed to the student called Name with
%%                     subject Subject
%%  * Report = print:  the report is printed to standard output
%%  * Report = silent: nothing is done
produce_report(mail, Name, Subject, PrintReport) :- !,
    mail_student(Name, Subject, PrintReport).
produce_report(print, _, _, PrintReport) :- !,
    call(PrintReport).
produce_report(silent, _, _, _) :- !.
produce_report(Report , _, _, _) :-
    log_warning('Unknown report type ~w', [Report]).

%% mail_student(+Name, +Subject, +MailPrinter)
mail_student(Name, Subject, MailPrinter) :-
    split_name(Name, _, Neptun),
    db_get_email(Neptun, Email),
    with_output_to_codes(MailPrinter, MailBody),
    environ('CONTACT', Sender),
    smtp_server(Url, UserPwd),
    format_to_atom('From: ~s', Sender, FromH),
    format_to_atom('To: ~s', Email, ToH),
    format_to_atom('Subject: ~s', Subject, SubjectH),
    Args0 = ['--ssl',           % use STARTTLS if available
             '--no-progress-meter',
             '--url', Url,
             '--mail-from', Sender,
             '--mail-rcpt', Email,
             '--header', FromH,
             '--header', ToH,
             '--header', SubjectH,
             % read body from stdin and encode it in quoted printable form
             '--form', '=<-;encoder=quoted-printable'],
    (   UserPwd = ':'
    ->  Args = Args0
    ;   Args = [ '--user', UserPwd|Args0]
    ),
    process_create(path(curl), Args,
                   [stdin(pipe(In)),stdout(null),process(Proc)]),
    format(In, '~s', [MailBody]),
    close(In),
    process_wait(Proc, exit(EC)),
    (   EC = 0
    ->  !
    ;   log_warning('Sending mail finished with code ~d', [EC]),
        fail
    ).
mail_student(Name, _, _) :-
    log_warning('Sending mail to ~w failed', [Name]).

%% smtp_url(-Url, -UserPwd): return URL and user:pwd of smtp serrver
smtp_server(Url, UserPwd) :-
    environ('SMTP_HELO', Helo),
    environ('SMTP_HOST', Host),
    environ('SMTP_PORT', Port),
    format_to_atom('smtp://~w:~w/~w', [Host,Port,Helo], Url),
    environ('SMTP_USERNAME', User),
    environ('SMTP_PASSWORD', Pwd),
    format_to_atom('~w:~w', [User,Pwd], UserPwd).

%% receive_homework(+Conf, +Class, +Mail, +FullName, +DateRangeCheck, +Report): common
%% predicate for guts_mail and guts_submit.
receive_homework(Conf, Class, Mail, Name, DateRangeCheck, Report) :-
    fail_on_error(load_files([Conf]),
                  error('Couldn''t read configuration file: ~w', [Conf])),
    safe_testclass(Class, LongName, Suite, PeriodBeg, PeriodEnd, Opts),
    (   (   DateRangeCheck
        ->	datime(datime(Year,Month,Day,Hour,_,_)),
            PeriodBeg @=< date(Year,Month,Day),
            date(Year,Month,Day,Hour) @=< PeriodEnd
        ;	true
        )
    ->  assert(testID(Class,Suite)),
        %% store mail
        store_mail(Mail, Name, Version),
        %% spool mail unless stated otherwise
        (	memberchk(nospool, Opts)
        ->	true
        ;	spool_mail(Name, Version, LongName, Report)
        ),
        %% extract mail if required
        (	memberchk(extract, Opts)
        ->	directory(student(Name), Dir),
            extract(Name, Version, Dir, Report)
        ;	true
        )
    ;   produce_report(Report, Name, Subject,
                       templates:test_class_closed(Subject, LongName,
                                                   PeriodBeg, PeriodEnd))
    ).

%%%
%%% testing predicates.
%%%

%% run_all_tests(Name, WorkDir, Opts): all tests are run for Name in WorkDir,
%% with options Opts.
run_all_tests(Name, WorkDir, Opts) :-
    %% determine languages to be tested
    (   member(language(Lang), Opts)
    ->  Langs = [Lang]
    ;   languages(Langs)
    ),
    %% run tests
    working_directory(OldD, WorkDir),
    ensure_success((read_mail_ID(ID),
                    templates:print_report_head(ID))),
    run_lang_tests(Name, Langs),
    working_directory(_, OldD).

%% run_lang_tests(+Name, +Langs): all tests for languages Langs are run.
run_lang_tests(_, []).
run_lang_tests(Name, [Lang]) :- !,
    run_lang_test(Name, Lang).
run_lang_tests(Name, [L|Ls]) :-
    run_lang_test(Name, L),
    ensure_success(templates:print_lang_separator),
    run_lang_tests(Name, Ls).

%% run_lang_test(+Name, +Lang): all tests for Lang language are run.
run_lang_test(Name, Lang) :-
    log('Starting ~w test for ~w', [Lang,Name]),
    ensure_success(templates:print_lang_head(Lang)),
    %% read language dependent setup
    filename(setup(Lang), SetupF),
    log('Reading language dependant setup file ~w', [SetupF]),
    read_file(SetupF, Setup),
    member(program(Program), Setup),
    %% delete neccessary files
    log('Deleting unnecessary files'),
    (   member(delete(ToDelete), Setup)
    ->  delete_files(ToDelete)
    ;   ToDelete = []
    ),
    log('Linking files'),
    %% link neccessary files
    (   member(link(ToLink), Setup)
    ->  directory(env(Lang), LangD),
        link_files(LangD, ToLink)
    ;   ToLink = []
    ),
    %% get initialization part
    (   predicate_member(init, Setup, Init) -> true
    ;   Init = true
    ), !,
    %% run tests and delete neccessary files
    append(ToDelete, ToLink, All),
    (   log('Running initialization code'),
        call(Init)
    ->  log('Running tests'),
        run_tests(Program, Good)
    ;   log('Initialization code failed'),
        Good = 0
    ),
    log('Cleaning up files'),
    delete_files(All),
    %% update database if neccessary
    (   database(update(Field,Type))
    ->  log('Updating database entries'),
        testcase_count(Total),
        update_score_field(Name, Field-Lang, Type-Good/Total)
    ;   true
    ).
run_lang_test(_,_).			% if initialization fails

%% run_tests(+Program, -Good): all tests are run with Program, the number good
%% good solutions is Good.
run_tests(Program, Good) :-
    mktemp('solution.XXXXXX', Out),
    findall(1, (get_testfile(N, In, Ref),
                successful_test(Program, N, In, Out, Ref)), Goods),
    ensure_success(delete_file(Out)),
    length(Goods, Good),
    testcase_count(Total),
    ensure_success(templates:print_lang_tail(Total, Good)).

%% get_testfile(N, In, Ref): The Nth test file is In, the reference solution
%% is in Ref.
get_testfile(N, In, Ref) :-
    testsuite_placement(directory(SubDir)), !,
    testfiles(SubDir, Dir, Files),
    nth(N, Files, In0),
    atom_codes(In0, InC),
    (   append(Front, [0'd|Rear], InC)
    ->  append(Front, [0's|Rear], RefC)
    ),
    atom_codes(Ref0, RefC),
    split_path(In, Dir, In0),
    split_path(Ref, Dir, Ref0).
get_testfile(N, In, '/dev/null') :-
    testsuite_placement(file(File)), !,
    read_testfile(File, Tests),
    mktemp('data.XXXXXX', In),
    undo(guts:ensure_success(guts:delete_file(In))), % delete when backtracking
    nth(N, Tests, Test),
    write_lines(In, [Test]).
get_testfile(N, In, '/dev/null') :-
    testsuite_placement(embedded(Count)),
    testID(_,Suite),
    between(N, 1, Count),
    atom_number(NA, N),
    atom_concat([Suite, '/', NA], In).

%% successful_test(+Program, +N, +In, +Out, +Ref): Program runs successfully
%% on Nth test case stored in In, and the solution produced in Out is the same
%% as the reference solution stored in Ref.
successful_test(Program, N, In, Out, Ref) :-
    filename(timeguard, Guard),
    timelimits(Default, Limits),
    (   nth(N, Limits, Limit) -> true
    ;   Limit = Default
    ),
    atom_number(LimitA, Limit),
    ensure_success(templates:print_testcase(N, Limit)),
    log('Running test ~w with time limit ~w', [N,Limit]),
    %% TODO: redirect stdout and stderr to current_output/1?
    time([file(Guard), LimitA, '5', file(Program), In, Out], Code, Time),
    (   Code = 0
    ->  evaluate(Ref, Out, Status)
    ;   exitcode_to_status(Code, Status)
    ),
    ensure_success(templates:explain_status(Status, Time)),
    log('Test ~w resulted in ~w', [N,Status]),
    Status = success.		% fail if solution is wrong

%% update_score_field(+Name, +Field, +Score): score field Field for Name is
%% updated in database with Score.
update_score_field(Name, Field, Score) :-
    score_value(Score, Value),
    split_name(Name, _, Neptun),
    log('Setting score ~w for ~w to ~w', [Field,Name,Value]),
    on_nonsuccess(db_set_score(Neptun, Field, Value),
                  log_warning('DB update failed for ~w-~w', [Neptun,Field])).

%% score_value(Score, Value): Value is the value of Score.
score_value(binary-T/T,     B) :- !, B = 1.
score_value(binary-_,       B) :- !, B = 0.
score_value(percent-G/T,    P) :- !, score_value(range(0,100)-G/T, P).
score_value(range(A,B)-G/T, R) :- !, R is A + (B-A)*G//T.
score_value(count-G/_,      C) :- !, C = G.
score_value(ratio-G/T,      R) :- !, R = G/T.
score_value(snap(A,B)-G/T,  C) :- !,
    (   G < A -> C = 0
    ;   G > B -> C = T
    ;   C = G
    ).
score_value(goal(Goal0)-G/T, S) :- !,
    Goal0 =.. Goal0L,
    append(Goal0L, [G,T,S], GoalL),
    Goal =.. GoalL,
    call(Goal).
score_value(Type-_, _) :-
    log_warning('Unknown score type ~w', [Type]),
    fail.

%%%
%%% Main test loop.
%%%

%% make_directories: creates all directories needed by the system.
make_directories :-
    directory(_, D),
    mkdirhier(D),
    fail.
make_directories.

%% touch_spoolfile: ensures that the spoolfile exists.
touch_spoolfile :-
    directory(spools, SpoolD),
    mkdirhier(SpoolD),
    filename(spoolfile, SpoolF),
    (   file_exists(SpoolF)
    ->  true
    ;   write_file(SpoolF, [])
    ).

%% count_test_cases(+Location): asserts testcase_count(Count) predicate after
%% counting test cases in Location.
count_test_cases(directory(SubDir)) :- !,
    testfiles(SubDir, _, Files),
    length(Files, Count),
    count_test_cases(Count).
count_test_cases(file(File)) :- !,
    read_testfile(File, Tests),
    length(Tests, Count),
    count_test_cases(Count).
count_test_cases(embedded(Count)) :- !,
    count_test_cases(Count).
count_test_cases(Count) :-
    Count > 0,
    assert(testcase_count(Count)).

%% test_loop: the main test loop.  Exits when instructed to by the spool file.
test_loop :-
    repeat,
    next_job(Job),
    log('Starting to process job ~w', [Job]),
    (   Job = halt
    ->  !, unspool_job	% test loop exit
    ;   run_job(Job),
        log('Unspooling job ~w', Job),
        unspool_job,
        fail
    ).

%% run_job(+Job): runs test job Job.
run_job(job(Name)) :- !,
    run_job(job(Name, [])).
run_job(job(Name,Opts)) :-
    %% check language specifications
    (   member(language(Lang), Opts),
        languages(Langs),
        non_member(Lang, Langs)
    ->  log_warning('Language ~w is invalid for job ~w', [Lang,Name]),
        fail
    ;   true
    ),
    %% Check report mode
    (   member(mail, Opts)
    ->  Report = mail
    ;   Report = silent
    ),
    %% ensure that mail is unpacked
    directory(student(Name), WorkDir),
    (   member(extract(Version), Opts) % extract specific version
    ->  log('Extracting mail ~w.~w', [Name,Version]),
        extract(Name, Version, WorkDir, Report)
    ;   member(extract, Opts)	% ... or extract latest
        ->  log('Extracting latest mail of ~w', [Name]),
            extract(Name, WorkDir, Report)
    ;   log('Testing existing directory ~w', [WorkDir]),
        directory_exists(WorkDir)	% ... or directory exists
    ),
    %% run tests sending output to logfile
    (   member(language(Lang), Opts)
    ->  filename(report(Name, Lang), RepF)
    ;   filename(report(Name), RepF)
    ),
    tell(RepF),
    log('Running tests'),
    call_cleanup(run_all_tests(Name, WorkDir, Opts), told),
    log('Producing report'),
    produce_report(Report, Name, Subject,
                   templates:test_done(Subject, RepF)),
    !.
run_job(Job) :-
    log_warning('Do not know how to handle job ~w', [Job]).

%% run_testd(+ConfFile): run the test daemon
run_testd(Conf) :-
    log('Launching test daemon'),
    log('Reading configuration file'),
    fail_on_error(load_files([Conf]),
                  error('Couldn''t read configuration file: ~w', [Conf])),
    log('Making directories'),
    make_directories,
    log('Counting test cases'),
    testsuite_placement(Placement),
    count_test_cases(Placement),
    touch_spoolfile,
    log('Starting test loop'),
    test_loop,
    log('Halting test daemon').

%%%
%%% Main service entry points
%%%

%% Each predicate called guts_<service> is responsible for a service.
guts_help :-
    print('Possible actions and mandatory arguments:'), nl,
    print('  help     print this help screen'), nl,
    nl,
    print('  confcsv  <submit.conf>'), nl,
    print('           dump contents of a <submit.conf> file in csv format'), nl,
    print('  extract  <semester> <class> <name> [<version>]'), nl,
    print('           extract a mail of a student'), nl,
    print('  mail     <config-file>'), nl,
    print('           receive and process a mail on standard input, w/ deadline check'), nl,
    print('  purge    <semester> <class>'), nl,
    print('           clean up mail directory, leaving only latest versions'), nl,
    print('  report   <semester> <class> <suite>'), nl,
    print('           send test reports to students'), nl,
    print('  submit   <config-file> <class> <name>'), nl,
    print('           receive standard input as a homework (for WWW submission, no deadline check)'), nl,
    print('  testd    <config-file>'), nl,
    print('           run test daemon with specified configuration'), nl.

guts_confcsv(Conf) :-
    fail_on_error(load_files([Conf]),
                  error('Couldn''t read configuration file: ~w', [Conf])),
    format('semester,class,name,suite,start,end,opts~n', []),
    semester(Semester), !,
    (   testclass(Class, Name, Suite, Begin, End, Opts),
        Begin = date(BY,BM,BD),
        End = date(EY,EM,ED),
        format('~w,~w,"~w",~w,~d-~|~`0t~d~2+-~|~`0t~d~2+,~d-~|~`0t~d~2+-~|~`0t~d~2+,"~w"~n',
               [Semester,Class,Name,Suite,BY,BM,BD,EY,EM,ED,Opts]),
        fail
    ;   true
    ).

guts_extract(Semester, Class, Name) :-
    guts_extract(Semester,Class,Name,latest).
guts_extract(Semester, Class, Name, Version) :-
    assert(semester(Semester)),
    assert(testID(Class,0)),
    directory(student(Name), Dir),
    (   Version = latest
    ->  extract(Name, Dir, print)
    ;   atom_number(Version, Ver),
        extract(Name, Ver, Dir, print)
    ).

guts_mail(Conf) :-
    read_mail(Mail, Name, Class),
    receive_homework(Conf, Class, Mail, Name, true, mail).

guts_purge(Semester, Class) :-
    assert(semester(Semester)),
    assert(testID(Class,0)),
    directory(hwks_mails, MailDir),
    (   directory_exists(MailDir) -> true
    ;   error('No mail directory for semester ~w, test class ~w.',
              [Semester, Class])
    ),
    purge(MailDir).

guts_report(Semester, Class, Suite) :-
    assert(semester(Semester)),
    assert(testID(Class,Suite)),
    directory(hwks_rep, RepD),
    %% iterate-by-failure starts here
    file_member_of_directory(RepD, Name, File),
    print(Name), nl,
    mail_student(Name, Subject, templates:test_done(Subject, File)),
    fail.
guts_report(_, _, _).

guts_submit(Conf, Class, Name) :-
    read_lines(Mail),
    receive_homework(Conf, Class, Mail, Name, fail, print).

guts_testd(Conf) :-
    open_log,
    call_cleanup(run_testd(Conf), close_log).

%% action(Action, Args): does action Action with args Args.  Calls guts_<action>
action(Action0, Args) :-
    atom_concat('guts_', Action0, Action),
    Goal =.. [Action|Args],
    predicate_property(Goal, _), !,
    (   call(Goal) -> true
    ;   error('~w failed.', [Action0])
    ).
action(Action, _) :-
    error('Unknown action ~w or wrong number of arguments. See "help" for more...',
          [Action]).

main :-
    prolog_flag(argv, Args),
    (   Args = [Action|Rest]
    ->  action(Action, Rest)
    ;   warning('Action expected as an argument, use "help" to get a list.', []),
        fail
    ),
    halt.
main :-
    halt(1).

user:runtime_entry(start) :- main.
