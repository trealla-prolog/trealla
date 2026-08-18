% Issue #1112: statistics/2 should raise a domain error for an invalid key
% rather than silently failing.
%
% https://github.com/trealla-prolog/trealla/issues/1112

:- initialization(main).

t(N, G) :-
	(  catch(G, E, (format("~w: ERROR ~w~n", [N,E]), fail))
	-> format("~w: ok~n", [N])
	;  format("~w: FAILED~n", [N])
	).

main :-
	t(invalid_key,
	  catch((statistics(nonsense, _), fail),
	        error(domain_error(statistics_key, nonsense), _), true)),
	t(valid_key, statistics(runtime, [_Total,_SinceLast])),
	true.
