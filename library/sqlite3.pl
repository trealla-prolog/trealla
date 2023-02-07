:- module(sqlite3, [
	sqlite3_query/4
	]).

:- use_module(library(sqlite3_register)).

% WARNING: This is experimental!

sqlite3_query(Conn, Sql, Cols, Names) :-
	flag('SQLITE_OK', SQLITE_OK),
	flag('SQLITE_ROW', SQLITE_ROW),
	sqlite3_prepare_v2(Conn, Sql, -1, Statement, _, Ret), Ret =:= SQLITE_OK,
	repeat,
		(	( sqlite3_step(Statement, Ret2), Ret2 =:= SQLITE_ROW )
		->	get_row(Statement, Cols, Names)
		;	( !, sqlite3_finalize(Statement, _Ret), fail )
		).

:- help(sqlite3_query(--stream, +string, +integer, +list), [iso(false)]).

get_col(_, 0, Cols, Cols, Names, Names) :- !.
get_col(Statement, Count, Col, Cols, Name, Names) :-
	flag('SQLITE_INTEGER', SQLITE_INTEGER),
	flag('SQLITE_FLOAT', SQLITE_FLOAT),
	flag('SQLITE_TEXT', SQLITE_TEXT),
	flag('SQLITE_BLOB', SQLITE_BLOB),
	Count1 is Count - 1,
	sqlite3_column_name(Statement, Count1, NewName),
	sqlite3_column_type(Statement, Count1, Type),
	(	Type =:= SQLITE_INTEGER -> sqlite3_column_int64(Statement, Count1, Value)
	;	Type =:= SQLITE_FLOAT -> sqlite3_column_double(Statement, Count1, Value)
	;	Type =:= SQLITE_TEXT -> sqlite3_column_text(Statement, Count1, Value)
	;	Type =:= SQLITE_BLOB -> sqlite3_column_text(Statement, Count1, Value)
	;	Value = '(void)'
	),
	get_col(Statement, Count1, [Value|Col], Cols, [NewName|Name], Names).

get_row(Statement, Cols, Names) :-
	sqlite3_column_count(Statement, Count),
	get_col(Statement, Count, [], Cols, [], Names).
