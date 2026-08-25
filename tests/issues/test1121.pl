% Issue #1121: dif/2 misbehaved on cyclic (rational) terms whose cycle
% passes through more than one variable - a tail chain reaching a slot
% whose own head also refers back to that same slot. reinforce_goals's
% copy_term_nat/2 probe silently corrupted such terms (a dangling fresh
% variable in place of the far side of the cycle), so dif/2 either
% reported a wrong `false` or oscillated forever re-deriving the goal.

:- use_module(library(dif)).
:- initialization(main).

main :-
	(   dif(A, B), C=[[]|C], A=[C|D], D=[D|A], B=[C|A]
	->  write(query2_ok)
	;   write('FAIL query2: dif wrongly failed')
	), nl,

	(   C2=[[]|C2], A2=[C2|D2], D2=[D2|A2], B2=[C2|A2], dif(A2, B2)
	->  write(query3_ok)
	;   write('FAIL query3: dif wrongly failed')
	), nl,

	% still correctly fails when the cyclic terms really are equal
	(   \+ (C3=[[]|C3], A3=[C3|D3], D3=[D3|A3], dif(A3, A3))
	->  write(equal_case_still_fails_ok)
	;   write('FAIL: dif(X,X) succeeded for a cyclic term')
	), nl,

	% plain, non-cyclic dif/2 is untouched
	(   \+ dif(a, a), dif(a, b)
	->  write(noncyclic_ok)
	;   write('FAIL: plain dif/2 regressed')
	), nl,

	halt.
