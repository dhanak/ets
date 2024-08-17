% This is a -*- prolog -*- file.
:- module(db, [
              db_transaction/2,	% db_transaction(-DB, +Goal)
              db_query/3,       % db_query(+QueryFmt, +Args, -Rows)
              db_get_email/2,   % db_get_email(+Neptun, -Email)
              db_set_score/3    % db_set_score(+Neptun, +ScoreName, +Value)
          ]).

:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(codesio)).
:- use_module(mysql).

:- meta_predicate db_transaction(-,:).

format_to_atom(Fmt, Args, Atom) :-
    format_to_codes(Fmt, Args, Chars),
    atom_codes(Atom, Chars).

db_transaction(DB, Goal) :-
    environ('DB_HOST', Host),
    environ('DB_USER', User),
    environ('DB_PASSWORD', Pwd),
    environ('DB_DATABASE', DBName),
    mysql_connect(Host, User, Pwd, DBName, DB),
    call_cleanup(Goal, mysql_close(DB)).

db_query(Fmt, Args, Rows) :-
    format_to_atom(Fmt, Args, Query),
    db_transaction(DB, db_query0(DB, Query, Rows)).

db_query0(DB, Query, Rows) :-
    mysql_query(DB, Query, Result),
    call_cleanup(findall(Row, mysql_fetch_row(Result, Row), Rows),
                 mysql_end_query(Result)).

db_get_email(Neptun, Email) :-
    db_query('SELECT email FROM people WHERE neptun="~w"',
             [Neptun], [[email-Email]]).

db_set_score(Neptun, Field, Value) :-
    db_transaction(DB, db_set_score(DB, Neptun, Field, Value)).

db_set_score(DB, Neptun, Field, Value) :-
    datime(datime(Y,Mo,D,H,Mi,S)),
    format_to_atom('GUTS - ~w.~w.~w. ~w:~w:~w', [Y,Mo,D,H,Mi,S], Note),
    format_to_atom('INSERT INTO scores (id,neptun,value,notes) VALUES ("~w","~w",~w,"~w")',
                   [Field,Neptun,Value,Note], Insert),
    format_to_atom('UPDATE scores SET value=~w, notes="~w" WHERE id="~w" AND neptun="~w"',
                   [Value,Note,Field,Neptun], Update),
    catch(mysql_query(DB, Insert, _), mysql_error(_,_),
          mysql_query(DB, Update, _)).
