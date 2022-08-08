:- module(auth, [
	adduser/2, deluser/1, login/5, logout/1, checkin/3,
	setuser_email/2, getuser_email/2,
	setuser_nick/2, getuser_nick/2,
	setuser_locked/2, getuser_locked/2,
	setuser_pass/2,
	session_set/3, session_get/3,
	listusers/1, save/0
	]).

:- use_module(library(dict)).

% These are our database records:

:- dynamic(auth_session/2).
:- persist(auth_user/2).

adduser(User, Pass) :-
	\+ auth_user(User, _),
	rand(Salt),
	atomic_concat(Salt, Pass, Str),
	crypto_data_hash(Str, Hash, [algorithm(sha256)]),
	split(User, '@', _, R),
	now(Now),
	dict:set([], created, Now, D0),
	(var(R) -> dict:set(D0, nick,User,D1) ; dict:set(D0, email, User, D1)),
	dict:set(D1, modified, Now, D2),
	dict:set(D2, salt, Salt, D3),
	dict:set(D3, hash, Hash, D4),
	assertz(auth_user(User, D4)).

deluser(User) :-
	retract(auth_user(User, _)).

login(User, Pass, SessId, Keep, Expires) :-
	auth_user(User, D),
	dict:get(D, salt, Salt),
	dict:get(D, hash, Hash),
	atomic_concat(Salt, Pass, Str),
	crypto_data_hash(Str, Hash2, [algorithm(sha256)]),
	Hash == Hash2,
	dict:get(D, locked, 0),
	uuid(Uuid),
	crypto_data_hash(Uuid, SessId, [algorithm(sha256)]),
	(Keep == true -> MaxAge is 1440 * 60 * 7 ; MaxAge is 600),
	Expires is now + MaxAge,
	dict:set([], user, User, D1),
	dict:set(D1, expires, Expires, D2),
	dict:set(D2, maxage, MaxAge, D3),
	assert(auth_session(SessId, D3)).

checkin(SessId, ValidatedUser, NewExpires) :-
	auth_session(SessId, D),
	dict:get(D, user, User),
	nonvar(User),
	dict:get(D, expires, Expires),
	dict:get(D, maxage, MaxAge),
	Expires > now,
	ValidatedUser = User,
	NewExpires is now + MaxAge,
	retract(auth_session(SessId, _)),
	dict:set(D, expires,NewExpires, D1),
	assert(auth_session(SessId, D1)).
checkin(SessId, _, _) :-
	retract(auth_session(SessId, _)),
	fail.

logout(SessId) :-
	retract(auth_session(SessId, _)).

setuser_email(User, Email) :-
	now(Now),
	retract(auth_user(User, D)),
	dict:set(D, email, Email, D1),
	dict:set(D1, modified, Now, D2),
	assertz(auth_user(User, D2)).

getuser_email(User, Email) :-
	auth_user(User, D),
	dict:get(D, email, Email).

setuser_nick(User, Nick) :-
	now(Now),
	retract(auth_user(User, D)),
	dict:set(D, nick, Nick, D1),
	dict:set(D1, modified, Now, D2),
	assertz(auth_user(User, D2)).

getuser_nick(User, Nick) :-
	auth_user(User, D),
	dict:get(D, nick, Nick).

setuser_locked(User, 0) :-
	retract(auth_user(User, D)),
	dict:del(D, locked, D1),
	assertz(auth_user(User, D1)).

setuser_locked(User, Locked) :-
	retract(auth_user(User, D)), !,
	dict:set(D, locked,Locked, D1),
	assertz(auth_user(User, D1)).

getuser_locked(User, Locked) :-
	auth_user(User, D),
	dict:get(D, locked, Locked).

setuser_pass(User, Pass) :-
	now(Now),
	rand(Salt),
	atomic_concat(Salt, Pass, Str),
	crypto_data_hash(Str, Hash, [algorithm(sha256)]),
	retract(auth_user(User, D)),
	dict:set(D, hash, Hash, D1),
	dict:set(D1, salt, Salt, D2),
	dict:set(D2, modified, Now, D3),
	assertz(auth_user(User, D3)).

session_set(SessId, Name, Value) :-
	retract(auth_session(SessId, D)),
	atomic_concat('user$', Name, ActualName),
	dict:set(D, ActualName, Value, D1),
	assertz(auth_session(SessId, D1)).

session_get(SessId, Name, Value) :-
	auth_session(SessId, D),
	atomic_concat('user$', Name, ActualName),
	dict:get(D, ActualName, Value).

listusers(L) :-
	findall(User, auth_user(User, _), L).

save :-
	db_save.
