% A blocks grammar in DCG

s --> vp.
s --> qest.
qest --> wh_loc, vbe, np.
qest --> wh_obj1, vbe, pp.
qest --> wh_obj2, snp, vbe, pp.
qest --> vbe, np, pp.
vp --> v, np.
np --> pn.
np --> det, snp.
np --> det, snp, pp.
snp --> noun.
snp --> ap, noun.
ap --> adj.
ap --> adj, ap.
pp --> prep, np.

noun --> [block].
noun --> [box].
noun --> [table].
noun --> [one].
pn --> [it].
v --> [put].
v --> [move].
v --> [pickup].
v --> [putdown].
vbe --> [is].
wh_loc --> [where].
wh_obj1 --> [what].
wh_obj2 --> [which].
adj --> [white].
adj --> [red].
adj --> [blue].
adj --> [green].
adj --> [big].
adj --> [small].
adj --> [large].
adj --> [little].
prep --> [on].
prep --> [onto].
prep --> [above].
prep --> [over].
det --> [each].
det --> [every].
det --> [the].
det --> [a].
det --> [some].

/*
    Test supplied sentence for parsing...
*/

test(S) :- phrase(s,S), write(S), write(' '), write('OK!'), nl.
test(S) :- write(S), write(' '), write('*** ERROR?'), nl.

main :-
    test([pickup,the,small,white,box]),
    test([pickup,the,white,small,box]),
    test([pickup,the,small,box]),
    test([pickup,the,box]),
    test([pickup,box]),             % should error
    test([paint,the,box]).          % should error
