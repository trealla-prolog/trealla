:- module(sqlite3_register, [sqlite_flag/2]).

% Note on Linux & FreeBSD systems *.so is fine for dynamic libs. On
% MacOS systems Trealla will automatically replace '.so' with '.dylib'

:- initialization(
	('$dlopen'('libsqlite3.so', 0, H),
	'$register_predicate'(H, sqlite3_open, [cstr, -ptr], int64),
	'$register_predicate'(H, sqlite3_exec, [ptr,cstr,ptr,ptr,-ptr], int64),
	'$register_predicate'(H, sqlite3_prepare_v2, [ptr,cstr,ptr,-ptr,-const_cstr], int64),
	'$register_predicate'(H, sqlite3_step, [ptr], int64),
	'$register_predicate'(H, sqlite3_finalize, [ptr], int64),
	'$register_predicate'(H, sqlite3_column_count, [ptr], int64),
	'$register_predicate'(H, sqlite3_column_name, [ptr,int64], const_cstr),
	'$register_predicate'(H, sqlite3_column_type, [ptr,int64], int64),
	'$register_predicate'(H, sqlite3_column_int64, [ptr,int64], int64),
	'$register_predicate'(H, sqlite3_column_double, [ptr,int64], fp64),
	'$register_predicate'(H, sqlite3_column_text, [ptr,int64], const_cstr),
	'$register_predicate'(H, sqlite3_close, [ptr], int64),
	true
	)).

:- help(sqlite3_open(+atom, --stream,-integer), [iso(false),desc('Open an Sqlite3 database returning a connection (as a stream).')]).
:- help(sqlite3_exec(+stream,+atom,+integer,+integer,-integer,-integer), [iso(false),desc('Execute an SQL statement on an Sqlite3 database connection')]).
:- help(sqlite3_prepare_v2(+stream,+atom,+integer,-integer,-integer,-integer), [iso(false)]).
:- help(sqlite3_step(+stream,-integer), [iso(false)]).
:- help(sqlite3_finalize(+stream,-integer), [iso(false)]).
:- help(sqlite3_column_count(+stream,-integer), [iso(false)]).
:- help(sqlite3_column_name(+stream,+integer,-atom), [iso(false)]).
:- help(sqlite3_column_type(+stream,+integer,-integer), [iso(false)]).
:- help(sqlite3_column_int64(+stream,+integer,-integer), [iso(false)]).
:- help(sqlite3_column_double(+stream,+integer,-float), [iso(false)]).
:- help(sqlite3_column_text(+stream,+integer,-string), [iso(false)]).
:- help(sqlite3_close(+stream,-integer), [iso(false),desc('Close an Sqlite3 database connection.')]).

sqlite_flag('SQLITE_OK', 0).
sqlite_flag('SQLITE_ERROR', 1).
sqlite_flag('SQLITE_ABORT', 4).
sqlite_flag('SQLITE_BUSY', 5).
sqlite_flag('SQLITE_LOCKED', 6).
sqlite_flag('SQLITE_READONLY', 8).
sqlite_flag('SQLITE_FULL', 13).
sqlite_flag('SQLITE_ROW', 100).
sqlite_flag('SQLITE_DONE', 101).

sqlite_flag('SQLITE_INTEGER', 1).
sqlite_flag('SQLITE_FLOAT', 2).
sqlite_flag('SQLITE_TEXT', 3).
sqlite_flag('SQLITE_BLOB', 4).
sqlite_flag('SQLITE_NULL', 5).

:- help(sqlite_flag(+atom,-integer), [iso(false)]).
