:- use_module(library(freeze)).
:- use_module(library(dif)).
:- initialization(main).

% bb_put/bb_get preserve variable attributes (residualized via copy_term/3)

main :-
	% frozen goal survives the blackboard
	freeze(X, true), bb_put(k1, X),
	bb_get(k1, Y), frozen(Y, G), write(G), nl,

	% frozen goal fires when the retrieved copy is bound
	freeze(A, (write(fired), nl)), bb_put(k2, A),
	bb_get(k2, 1),

	% the original variable is unaffected by put/get
	frozen(A, GA), write(GA), nl,

	% sharing of attributed vars inside a compound is preserved
	freeze(B, true), bb_put(k3, f(B, g(B))),
	bb_get(k3, f(P, g(Q))),
	(	P == Q ->
		write(sharing_preserved), nl
	;	write(sharing_broken), nl
	),

	% dif/2 survives the blackboard
	dif(C, b), bb_put(k4, C),
	bb_get(k4, Z1),
	(	Z1 = b ->
		write(dif_lost), nl
	;	write(dif_blocks), nl
	),
	bb_get(k4, Z2),
	(	Z2 = c ->
		write(dif_allows), nl
	;	write(dif_broken), nl
	),

	% plain terms take the raw path unchanged
	bb_put(k5, hello(world)), bb_get(k5, hello(world)),
	write(plain_ok), nl,

	% bb_delete and bb_update preserve attributes too
	freeze(D, true), bb_put(k6, D),
	bb_delete(k6, D2), frozen(D2, GD), write(GD), nl,
	(	bb_get(k6, _) ->
		write(delete_broken), nl
	;	write(deleted), nl
	),
	freeze(E, true), bb_put(k7, E),
	bb_update(k7, O, E), frozen(O, GO), write(GO), nl,
	bb_get(k7, E2), frozen(E2, GE), write(GE), nl.
