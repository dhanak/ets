% This is a -*- prolog -*- file.
:- module(log, [
              open_log/0,
              close_log/0,
              log/1,			% log(+MessageAtom)
              log/2,			% log(+MessageFormat, +ArgList)
              log_warning/2     % log_warning(+MessageFormat, +Args)
          ]).

:- use_module(library(system)).
:- use_module(utils).

open_log :-
    logfile_open, !,
    warning('Log file already open').
open_log :-
    logfile(F),
    fail_on_error(open(F, write, _, [alias(log)])).

close_log :-
    logfile_open, !,
    fail_on_error(close(log)).
close_log.

log(Message) :-
    logfile_open, !,
    timestamp,
    write(log, Message),
    nl(log),
    flush_output(log).
log(_).

log(Format, Args) :-
    logfile_open, !,
    timestamp,
    format(log, Format, Args),
    nl(log),
    flush_output(log).
log(_, _).

log_warning(Format, Args) :-
    warning(Format, Args),
    atom_concat('*** Warning: ', Format, WFormat),
    log(WFormat, Args).

logfile(F) :-
    guts:subdirectory(work, 'logs', LD),
    pid(P),
    number_codes(P, PC),
    append_lists(["guts_",PC,".log"], LFC),
    atom_codes(LF, LFC),
    split_path(F, LD, LF).

logfile_open :-
    once(stream_property(_, alias(log))).

timestamp :-
    datime(datime(Y,Mo,D,H,Mi,S)),
    format(log, '~|~`0t~d~4+.~|~`0t~d~2+.~|~`0t~d~2+. ', [Y,Mo,D]),
    format(log, '~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+   ', [H,Mi,S]).
