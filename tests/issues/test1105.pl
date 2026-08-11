% Issue #1105: between/3 should handle bigints rather than throwing
% domain_error(small_integer_range).
%
% https://github.com/trealla-prolog/trealla/issues/1105

:- initialization(main).

t(N, G) :-
	(  catch(G, E, (format("~w: ERROR ~w~n", [N,E]), fail))
	-> format("~w: ok~n", [N])
	;  format("~w: FAILED~n", [N])
	).

offs(Lo, Hi, Base, L) :-
	findall(D, (between(Lo,Hi,V), D is V-Base), L).

main :-
	X is 4^4^4,
	NX is -X,
	Hi is X+3,
	NXHi is NX+2,
	Big is 2^70,
	NB1 is -(2^63)-1,
	NB2 is NB1+3,

	% the reported case
	t(issue,          between(X,X,X)),

	% checking a bound value (no enumeration)
	t(low_gt_high,    \+ between(Hi,X,_)),
	t(check_in,       (M is X+1, between(X,Hi,M))),
	t(check_out_hi,   \+ between(X,Hi,0)),
	t(check_out_lo,   (Y is X-1, \+ between(X,Hi,Y))),
	t(big_p3_small,   \+ between(1,10,X)),

	% enumeration with bigint bounds
	t(enum_det,       (offs(X,X,X,L1), L1 == [0])),
	t(enum_4,         (offs(X,Hi,X,L2), L2 == [0,1,2,3])),
	t(enum_neg,       (offs(NX,NXHi,NX,L3), L3 == [0,1,2])),
	t(small_lo_big_hi,(once(between(1,Big,V1)), V1 == 1)),

	% values that start big and demote to smallint
	t(demote,         (findall(V,between(NB1,NB2,V),L4), length(L4,4),
	                   last(L4,Last), Last is NB1+3)),

	% smallint path must be unaffected
	t(small_enum,     (findall(V,between(1,5,V),L5), L5 == [1,2,3,4,5])),
	t(small_det,      (findall(V,between(3,3,V),L6), L6 == [3])),
	t(small_neg,      (findall(V,between(-2,1,V),L7), L7 == [-2,-1,0,1])),
	t(small_cross,    (offs(-3,2,0,L8), L8 == [-3,-2,-1,0,1,2])),

	% errors are unchanged
	t(err_var,        catch((between(_,3,_),fail), error(instantiation_error,_), true)),
	t(err_type1,      catch((between(a,3,_),fail), error(type_error(integer,a),_), true)),
	t(err_type3,      catch((between(1,3,a),fail), error(type_error(integer,a),_), true)),

	true.
