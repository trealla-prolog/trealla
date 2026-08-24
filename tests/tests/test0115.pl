% Regression test for a heap-use-after-free fixed in
% "An if-then-else use-after-free fix": do_if_then_else()/
% do_soft_if_then_else() (src/bif_control.c) built a barrier-protected
% continuation without marking the calling frame no_recov, so
% resume_frame()'s tail-call heap-reclaim fast path (query.c) could
% free memory the continuation still needed. Needed real thread
% concurrency to manifest - a single OS thread, however heavily
% interleaved via cooperative tasks, never reproduced it - so this
% exercises thread_send_message/thread_get_message with the receiver
% selecting on the result via if-then-else, the same shape that
% crashed 100% of the time before the fix (heap-use-after-free at
% query.c:1333 in resume_frame, under -fsanitize=address).

:- initialization(main).

worker(Target, N) :-
	forall(between(1, N, I), thread_send_message(Target, msg(I))).

collector(0, _) :- !.
collector(N, Me) :-
	( thread_get_message(Me, msg(_I), [timeout(0.001)]) ->
		N1 is N - 1,
		collector(N1, Me)
	;	collector(N, Me)
	).

main :-
	thread_self(Me),
	NThreads = 2,
	PerThread = 200,
	findall(Id, (
		between(1, NThreads, _),
		thread_create(worker(Me, PerThread), Id, [])
	), Ids),
	Total is NThreads * PerThread,
	collector(Total, Me),
	forall(member(Id, Ids), thread_join(Id, _)),
	writeln(ok).
