:- module(actors, [
	actor_spawn/2,
	actor_spawn/3,
	actor_self/1,
	actor_send/2,
	actor_recv/1,
	actor_recv/2,
	actor_link/1,
	actor_unlink/1,
	supervisor_start/2,
	supervisor_start/3,
	supervisor_stop/1
	]).

% Actors on top of threads. A thread is already a mailbox with
% order-preserving selective receive, so this adds only what was
% missing: links, and a death notice carrying why.
%
% Pid is a thread id, so anything taking a thread works here too.

:- use_module(library(lists)).

:- dynamic('$actor_link'/2).		% when A dies, tell B
:- dynamic('$actor_reason'/2).

actor_self(Pid) :- thread_self(Pid).

actor_send(Pid, Msg) :- thread_send_message(Pid, Msg).

actor_recv(Msg) :- thread_self(Me), thread_get_message(Me, Msg).

actor_recv(Msg, Opts) :- thread_self(Me), thread_get_message(Me, Msg, Opts).

actor_spawn(Goal, Pid) :- actor_spawn(Goal, Pid, []).

% The body waits for '$actor_go' before running, so a caller asking for
% link(true) can install the link before the actor can possibly die.
% Without that handshake a short-lived actor races its own linking.

actor_spawn(Goal, Pid, Opts) :-
	(	memberchk(alias(A), Opts)
	->	TOpts = [alias(A), detached(true), at_exit('$actor_died')]
	;	TOpts = [detached(true), at_exit('$actor_died')]
	),
	thread_create('$actor_body'(Goal), Pid, TOpts),
	( memberchk(link(true), Opts) -> actor_link(Pid) ; true ),
	actor_send(Pid, '$actor_go').

:- meta_predicate(actor_spawn(0,-)).
:- meta_predicate(actor_spawn(0,-,+)).

'$actor_body'(Goal) :-
	thread_self(Me),
	thread_get_message(Me, '$actor_go'),
	(	catch(Goal, E, true)
	->	( var(E) -> R = true ; R = exception(E) )
	;	R = false
	),
	assertz('$actor_reason'(Me, R)).

% Runs on the dying actor, whatever way it ended.

'$actor_died' :-
	thread_self(Me),
	( retract('$actor_reason'(Me, R)) -> true ; R = true ),
	forall(retract('$actor_link'(Me, Other)), actor_send(Other, exit(Me, R))),
	retractall('$actor_link'(_, Me)).

% Links are bidirectional: either death notifies the other.

actor_link(Pid) :-
	thread_self(Me),
	assertz('$actor_link'(Me, Pid)),
	assertz('$actor_link'(Pid, Me)).

actor_unlink(Pid) :-
	thread_self(Me),
	retractall('$actor_link'(Me, Pid)),
	retractall('$actor_link'(Pid, Me)).

:- help(actor_spawn(+callable,-thread), [iso(false)]).
:- help(actor_spawn(+callable,-thread,+list), [iso(false)]).
:- help(actor_self(-thread), [iso(false)]).
:- help(actor_send(+thread,+term), [iso(false)]).
:- help(actor_recv(?term), [iso(false)]).
:- help(actor_recv(?term,+list), [iso(false)]).
:- help(actor_link(+thread), [iso(false)]).
:- help(actor_unlink(+thread), [iso(false)]).


% A minimal supervisor: link to each child, restart the one that dies.
%
% one_for_one only, and a restart budget - max_restarts(N) within
% period(Seconds), defaulting to 5 in 5. Without the budget a child that
% dies immediately on start spins forever, which is the part people get
% wrong writing this by hand. Exhausting it stops the supervisor.

supervisor_start(Children, Sup) :- supervisor_start(Children, Sup, []).

supervisor_start(Children, Sup, Opts) :-
	( memberchk(max_restarts(M), Opts) -> true ; M = 5 ),
	( memberchk(period(P), Opts) -> true ; P = 5 ),
	actor_spawn('$sup_init'(Children, M, P), Sup).

supervisor_stop(Sup) :- actor_send(Sup, '$sup_stop').

'$sup_init'(Children, M, P) :-
	findall(Pid-G, (member(G, Children), actor_spawn(G, Pid, [link(true)])), Running),
	'$sup_loop'(Running, M, P, []).

'$sup_loop'(Running, M, P, Hist) :-
	actor_recv(Msg),
	'$sup_msg'(Msg, Running, M, P, Hist).

'$sup_msg'('$sup_stop', Running, _, _, _) :-
	!,
	forall(member(Pid-_, Running), catch(thread_cancel(Pid), _, true)).

'$sup_msg'(exit(Pid, _), Running, M, P, Hist) :-
	selectchk(Pid-Goal, Running, Rest),
	!,
	get_time(Now),
	Cutoff is Now - P,
	include('$sup_recent'(Cutoff), Hist, Recent),
	length(Recent, N),
	(	N >= M
	->	forall(member(Other-_, Rest), catch(thread_cancel(Other), _, true))
	;	actor_spawn(Goal, New, [link(true)]),
		'$sup_loop'([New-Goal|Rest], M, P, [Now|Recent])
	).

'$sup_msg'(_, Running, M, P, Hist) :-
	'$sup_loop'(Running, M, P, Hist).

'$sup_recent'(Cutoff, T) :- T >= Cutoff.

:- help(supervisor_start(+list,-thread), [iso(false)]).
:- help(supervisor_start(+list,-thread,+list), [iso(false)]).
:- help(supervisor_stop(+thread), [iso(false)]).
