% Issue #1104: min/2 corrupted the query's shared bigint accumulator.
%
% bif_iso_min_2() called mp_int_clear(&q->tmp_ival) before mp_int_copy()
% into it. mp_int_clear() frees the digit array and NULLs z->digits but
% leaves z->alloc stale, so the following copy's s_pad() saw enough
% capacity, skipped reallocating, and memcpy'd into NULL.
%
% Reached whenever min/2 selected a *bigint* result, ie min(Big,Small)
% with Big negative, or min(Small,Big) with Big negative. max/2 was
% always correct; min/2 was a copy of it with the clears added.
%
% Reported via clpz: 6^X#=X^18, X in 0..25 crashed, because 6^25 first
% exceeds 2^63 and the propagator narrows bounds with min/2.
%
% https://github.com/trealla-prolog/trealla/issues/1104

:- use_module(library(clpz)).
:- initialization(main).

t(N, G) :-
	(  catch(G, E, (format("~w: ERROR ~w~n", [N,E]), fail))
	-> format("~w: ok~n", [N])
	;  format("~w: FAILED~n", [N])
	).

main :-
	B is 6^25,			% first 6^N over 2^63
	NB is -B,
	B2 is 6^26,
	NB2 is -B2,

	% the crashing shapes: min/2 returning a bigint
	t(min_negbig_small,  (X1 is min(NB,3),    X1 =:= NB)),
	t(min_small_negbig,  (X2 is min(3,NB),    X2 =:= NB)),
	t(min_negbig_negbig, (X3 is min(NB,NB2),  X3 =:= NB2)),
	t(min_big_big,       (X4 is min(B,B2),    X4 =:= B)),
	t(min_negbig_float,  (X5 is min(NB,2.5),  X5 =:= NB)),

	% repeated use must not corrupt the shared accumulator
	t(min_repeat,        (findall(V, (between(1,50,_), V is min(NB,3)), L),
	                      length(L,50), sort(L,[S]), S =:= NB)),

	% interleaving min/2 with other bigint arithmetic
	t(min_interleave,    (Y1 is min(NB,3), Z is B*B, Y2 is min(NB2,3),
	                      Y1 =:= NB, Y2 =:= NB2, Z =:= 6^50)),

	% max/2 was already correct for the crash, keep it that way
	t(max_negbig_small,  (X6 is max(NB,3),    X6 =:= 3)),
	t(max_big_small,     (X7 is max(B,3),     X7 =:= B)),

	% second bug: bigint vs float, bigint wins. Both min/2 and max/2 set
	% q->accum then fell through to SET_ACCUM(), overwriting it from a
	% stale q->tmp_ival, so the answer was a previous result. Needs a
	% prior bigint op to poison tmp_ival, hence the C1/C2 goals.
	t(min_float_stale,   (C1 is min(NB,NB2), X10 is min(NB,2.5),
	                      C1 =:= NB2, X10 =:= NB)),
	t(max_float_stale,   (C2 is max(B,B2),   X11 is max(B,2.5),
	                      C2 =:= B2, X11 =:= B)),
	t(min_float_rev,     (C3 is min(NB,NB2), X12 is min(2.5,NB),
	                      C3 =:= NB2, X12 =:= NB)),
	t(max_float_rev,     (C4 is max(B,B2),   X13 is max(2.5,B),
	                      C4 =:= B2, X13 =:= B)),
	t(min_float_wins,    (X14 is min(B,2.5), X14 =:= 2.5)),
	t(max_float_wins,    (X15 is max(NB,2.5), X15 =:= 2.5)),

	% smallint and float paths unaffected
	t(min_small,         (X8 is min(2,3),     X8 =:= 2)),
	t(min_mixed_float,   (X9 is min(2.0,3),   X9 =:= 2.0)),
	t(max_small,         (XA is max(2,3),     XA =:= 3)),

	% the reported clpz goal
	t(clpz_0_24,         \+ (6^X#=X^18, X in 0..24)),
	t(clpz_0_25,         \+ (6^X#=X^18, X in 0..25)),
	t(clpz_0_30,         \+ (6^X#=X^18, X in 0..30)),

	true.
