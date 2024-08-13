% This is a -*- prolog -*- file.
:- module(mysql, [
              mysql_connect/5, % mysql_connect(+Host, +User, +Pwd, +DB, -Handle)
              mysql_close/1,   % mysql_close(+Handle)
              mysql_query/3,   % mysql_query(+Handle, +Query, -Result)
              mysql_end_query/1,	% mysql_end_query(+Result)
              mysql_fetch_row/2,	% mysql_fetch_row(+Result, -Row)
              mysql_affected_rows/2	% mysql_affected_rows(+Handle, -RowNum)
          ]).

:- dynamic foreign/2, foreign_resource/2.

foreign_resource(mysql, [
                     mys_connect,
                     mys_close,
                     mys_query,
                     mys_end_query,
                     mys_fetch_row,
                     mys_affected_rows
                 ]).

foreign(mys_connect,       mysql_connect(+string, +string, +string, +string,
                                         [-address('mys_handle')])).
foreign(mys_close,         mysql_close(+address('mys_handle'))).
foreign(mys_query,         mysql_query(+address('mys_handle'), +string,
                                       [-address('mys_result')])).
foreign(mys_end_query,     mysql_end_query(+address('mys_result'))).
foreign(mys_fetch_row,     mysql_fetch_row(+address('mys_result'), -term)).
foreign(mys_affected_rows, mysql_affected_rows(+address('mys_handle'), [-integer])).

:- load_foreign_resource(mysql).

:- multifile user:generate_message_hook/3.
:- dynamic   user:generate_message_hook/3.
user:generate_message_hook(mysql_error(Pred, Msg)) --> !,
    ['MySQL error in ~w:'-[Pred], nl, Msg-[], nl].
