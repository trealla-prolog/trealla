:- module(task_actors, [
	task_actor_spawn/2,
	task_actor_spawn/3,
	task_actor_self/1,
	task_actor_send/2,
	task_actor_recv/1,
	task_actor_recv/2,
	task_actor_link/1,
	task_actor_unlink/1
	]).

% Actors on tasks rather than threads - same shape as library(thread_actors),
% different backend, so skynet-sized actor counts stay off the OS
% thread ceiling. Pid is a qid (task_self/1, task_create/2), not a
% thread id: the two libraries' Pids are not interchangeable, and
% neither is a drop-in replacement for the other - see
% docs/DESIGN-GUSTTO.md phase 5 for why both are kept.
%
% No handshake needed for spawn+link atomicity, unlike the thread
% version: tasks on one OS thread run cooperatively, so a freshly
% task_create/2'd child provably has not run a single instruction by
% the time task_create/2 returns - nothing yields between the spawn and
% the link below. A short-lived thread can die before its creator
% installs a link; a short-lived task cannot outrun code that has not
% yielded yet.
%
% recv/1 does not block (it never did, even before GUSTTO), so
% task_actor_recv/1,2 spin on yield/0 until something arrives or a
% timeout elapses. That is real CPU spent polling, not truly parked -
% fine for an actor that's mostly busy, questionable for one that's
% mostly waiting. A blocking recv is future work, not a promise this
% file makes.

:- use_module(library(lists)).

:- dynamic('$task_actor_link'/2).		% when A dies, tell B
:- dynamic('$task_actor_reason'/2).

task_actor_self(Pid) :- task_self(Pid).

task_actor_send(Pid, Msg) :- send(Pid, Msg).

task_actor_recv(Msg) :-
	repeat,
	( recv(Msg) -> ! ; yield, fail ).

task_actor_recv(Msg, Opts) :-
	( memberchk(timeout(T), Opts) ->
		get_time(Start),
		'$task_actor_recv_until'(Msg, Start, T)
	;	task_actor_recv(Msg)
	).

'$task_actor_recv_until'(Msg, Start, T) :-
	( recv(Msg) -> true
	;	get_time(Now),
		Elapsed is Now - Start,
		( Elapsed >= T -> fail
		;	yield,
			'$task_actor_recv_until'(Msg, Start, T)
		)
	).

task_actor_spawn(Goal, Pid) :- task_actor_spawn(Goal, Pid, []).

task_actor_spawn(Goal, Pid, Opts) :-
	task_create('$task_actor_body'(Goal), Pid),
	( memberchk(link(true), Opts) -> task_actor_link(Pid) ; true ).

:- meta_predicate(task_actor_spawn(0,-)).
:- meta_predicate(task_actor_spawn(0,-,+)).

'$task_actor_body'(Goal) :-
	task_self(Me),
	( catch(Goal, E, true) -> ( var(E) -> R = true ; R = exception(E) ) ; R = false ),
	forall(retract('$task_actor_link'(Me, Other)), task_actor_send(Other, exit(Me, R))),
	retractall('$task_actor_link'(_, Me)).

% Links are bidirectional: either death notifies the other.

task_actor_link(Pid) :-
	task_self(Me),
	assertz('$task_actor_link'(Me, Pid)),
	assertz('$task_actor_link'(Pid, Me)).

task_actor_unlink(Pid) :-
	task_self(Me),
	retractall('$task_actor_link'(Me, Pid)),
	retractall('$task_actor_link'(Pid, Me)).

:- help(task_actor_spawn(+callable,-integer), [iso(false)]).
:- help(task_actor_spawn(+callable,-integer,+list), [iso(false)]).
:- help(task_actor_self(-integer), [iso(false)]).
:- help(task_actor_send(+integer,+term), [iso(false)]).
:- help(task_actor_recv(?term), [iso(false)]).
:- help(task_actor_recv(?term,+list), [iso(false)]).
:- help(task_actor_link(+integer), [iso(false)]).
:- help(task_actor_unlink(+integer), [iso(false)]).
