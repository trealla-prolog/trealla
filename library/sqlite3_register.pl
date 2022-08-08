:- module(sqlite3_register, [flag/2]).

% Note on Linux & FreeBSD systems *.so is fine for dynamic libs. On
% MacOS systems Trealla will automatically replace '.so' with '.dylib'

:- initialization(
	('$dlopen'('libsqlite3.so', 0, H),
	'$register_predicate'(H, sqlite3_open, [cstr, -ptr], int64),
	'$register_predicate'(H, sqlite3_close, [ptr], int64),
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
	true
	)).

flag('SQLITE_OK', 0).
flag('SQLITE_ERROR', 1).
flag('SQLITE_ABORT', 4).
flag('SQLITE_BUSY', 5).
flag('SQLITE_LOCKED', 6).
flag('SQLITE_READONLY', 8).
flag('SQLITE_FULL', 13).
flag('SQLITE_ROW', 100).
flag('SQLITE_DONE', 101).

flag('SQLITE_INTEGER', 1).
flag('SQLITE_FLOAT', 2).
flag('SQLITE_TEXT', 3).
flag('SQLITE_BLOB', 4).
flag('SQLITE_NULL', 5).
