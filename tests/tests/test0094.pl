:- meta_predicate(g(:)).

g(Goal) :- compound(Goal), write('ok: '), write(Goal), nl.

run :- g(true).

:- initialization(run).
:- initialization(g(foo:true)).

