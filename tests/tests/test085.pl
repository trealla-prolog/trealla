% Traversing graph paths

oneway(paris,orleans).
oneway(paris,chartres).
oneway(paris,amiens).
oneway(orleans,blois).
oneway(orleans,bourges).
oneway(blois,tours).
oneway(chartres,lemans).
oneway(lemans,angers).
oneway(lemans,tours).
oneway(angers,nantes).

path(A,B) :-
    oneway(A,B).
path(A,B) :-
    oneway(A,C),
    path(C,B).

% test cases
case(path(_,nantes)).

test :-
    case(A),
    A,
    write(A),
    write('.'),
    nl,
    fail.
test :-
    halt.

:- initialization(test).
