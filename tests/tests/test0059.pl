:- initialization(main).
:- use_module(library(lists)).

main :- test1, test2, test3, test4a, test4b, test5a, test5b, test6a, test6b, test6c, test7,
        test8, test9a, test9b, test9c, test10, test11a, test11b, test12, test13, test14,
        test15a, test15b, test15c, test15d, test16a, test16b, test16c, test17, test18, test19.

test1 :-
    write('Test1  :\t'),
    F = f(a,_,c),
    functor(F,f,3),
    arg(2,F,b),
    F = f(a,b,c),
    write('PASSED!'), nl, !.

test2 :-
    write('Test2  :\t'),
    F = f(a,b,c),
    copy_term(F,X),
    F=X,
    write('PASSED!'), nl, !.

test3 :-
    write('Test3  :\t'),
    F = f(A,B,C),
    copy_term(F,X),
    F=X,
    A=a, B=b, C=c,
    arg(2,X,b),
    write('PASSED!'), nl, !.

test4a :-
    write('Test4a :\t'),
    (true -> write('PASSED!') ; write('ERROR')),
    nl.

test4b :-
    write('Test4b :\t'),
    (fail -> write('ERROR') ; write('PASSED!')),
    nl.

name5(john).
name5(mary).
name5(tom).

test5a :-
    write('Test5a :\t'),
    (name5(john) ; write('ERROR')),
    write('PASSED!'), nl, !.

test5b :-
    write('Test5b :\t'),
    (name5(fred) ; name5(mary)),
    write('PASSED!'), nl, !.

test6a :-
    write('Test6a :\t'),
    F = {a,b,c},
    functor(F,{},1),
    write('PASSED!'), nl, !.

test6b :-
    write('Test6b :\t'),
    F = [a,b,c],
    functor(F,'.',2),
    write('PASSED!'), nl, !.

test6c :-
    write('Test6c :\t'),
    F = (a,b,c),
    functor(F,',',2),
    write('PASSED!'), nl, !.

test7 :-
    write('Test7  :\t'),
    (1/2/3) = ((1/2)/3),
    (a,b,c) = (a,(b,c)),
    {a,b,c} = {a,(b,c)},
    write('PASSED!'), nl, !.

test8 :-
    write('Test8  :\t'),
    arg(1,f(a,b,c),a),
    arg(2,f(a,b,c),b),
    arg(3,f(a,b,c),c),
    write('PASSED!'), nl, !.

test9a :-
    write('Test9a :\t'),
    arg(1,{a,b,c},(a,b,c)),
    write('PASSED!'), nl, !.

test9b :-
    write('Test9b :\t'),
    arg(1,[a,b,c],a),
    arg(2,[a,b,c],[b,c]),
    write('PASSED!'), nl, !.

test9c :-
    write('Test9c :\t'),
    arg(1,(a,b,c),a),
    arg(2,(a,b,c),(b,c)),
    write('PASSED!'), nl, !.

test10 :-
    write('Test10 :\t'),
    Item = {
       'author': 'Philip K Dick',
       'works': [
          {'title': 'The Man in the High Castle'},
          {'title': 'Do Androids Dream of Electric Sheep'}
       ]
    },
    Item = {Author,_Works},
    Author = ('author':V),
    V == 'Philip K Dick',
    write('PASSED!'), nl, !.

age11(peter,7).
age11(anne,5).
age11(pat,8).
age11(tom,5).

test11a :-
    write('Test11a:\t'),
    findall(Name,age11(Name,Age),L1),
    L1 = [peter,anne,pat,tom],
    findall(Age,age11(Name,Age),L2),
    L2 = [7,5,8,5],
    write('PASSED!'), nl, !.

test11b :-
    write('Test11b:\t'),
    findall(X,member(X,[one,two,three]),L),
    L = [one,two,three],
    write('PASSED!'), nl, !.

test12 :-
    write('Test12 :\t'),
    number_chars(123, ['1','2','3']),
    number_codes(123, [49,50,51]),
    atom_chars('123', ['1','2','3']),
    atom_codes('123', [49,50,51]),
    atom_codes('一二三', [19968,20108,19977]),
    write('PASSED!'), nl, !.

test13 :-
    write('Test13 :\t'),
    F =.. [a,1,2],
    F = a(1,2),
    a(1,2,3) =.. L, L=[a,1,2,3],
    write('PASSED!'), nl, !.

bar14([A],B) :- A=B.
foo14(A,B) :- bar14(A,B).

test14 :-
    write('Test14 :\t'),
    A=a, foo14([A],B), A=B,
    write('PASSED!'), nl, !.

test15a :-
    write('Test15a:\t'),
    call(X is 1+2), X =:= 3,
    write('PASSED!'), nl, !.

test15b :-
    write('Test15b:\t'),
    call((X is 1+2, true)), X =:= 3,
    write('PASSED!'), nl, !.

test15c :-
    write('Test15c:\t'),
    call(is,X,1+2), X =:= 3,
    write('PASSED!'), nl, !.

test15d :-
    write('Test15d:\t'),
    compare(<,1,2),
    compare(=,2,2),
    compare(>,3,2),
    write('PASSED!'), nl, !.

test16a  :-
    write('Test16a:\t'),
    once(X is 1+2), X =:= 3,
    write('PASSED!'), nl, !.

test16b  :-
    write('Test16b:\t'),
    once((X is 1+2, true)), X =:= 3,
    write('PASSED!'), nl, !.

test16c  :-
    write('Test16c:\t'),
    findall(X,once(age11(X,_)),L),
    ground(L), L=[peter],
    write('PASSED!'), nl, !.

test17  :-
    write('Test17 :\t'),
    assertz({abc,123}), assertz({xyz,456}),
    clause({abc,X},B), X = 123, B = true,
    clause({xyz,Y},C), Y = 456, C = true,
    {abc,Z}, Z = 123,
    retract({xyz,W}), W = 456,
    write('PASSED!'), nl, !.

test18a2(X) :- X = f(a,b), fail.
test18a2(X) :- X = f(b,c), fail.
test18a2(X) :- X = f(_,world).
test18a(X) :- test18a2(X).

test18b2(X) :- X = f(e,f), fail.
test18b2(X) :- X = f(f,g), fail.
test18b2(X) :- X = f(hello,_).
test18b(X) :- test18b2(X).

test18 :-
    write('Test18 :\t'),
    X = f(_,_),
    test18a(X),
    \+ ground(X),
    test18b(X),
    ground(X),
    X = f(hello,world),
    write('PASSED!'), nl, !.

test19 :-
    write('Test19 :\t'),
    [a] = '.'(a,[]),
    [a] = .(a,[]),
    write('PASSED!'), nl, !.
