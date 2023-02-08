:- use_module(library(sqlite3)).

run :-
	test('samples/sqlite3.db', 'SELECT * FROM company').

test(Database, Query) :-
	sqlite_flag('SQLITE_OK', SQLITE_OK),
	sqlite3_open(Database, Connection, Ret), Ret =:= SQLITE_OK,
	bagof(Row, sqlite3_query(Connection, Query, Row, _), Results),
	writeq(Results), nl,
	sqlite3_close(Connection, _).

run2 :-
	test2('samples/sqlite3.db', 'INSERT INTO companys VALUES(7,\'Josepth\',47,\'Vanuatu\',15000.0)').

test2(Database, Query) :-
	sqlite_flag('SQLITE_OK', SQLITE_OK),
	sqlite3_open(Database, Connection, Ret), Ret =:= SQLITE_OK,
	sqlite3_exec(Connection, Query, 0, 0, _, Ret2), Ret2 =:= SQLITE_OK,
	sqlite3_close(Connection, _).
