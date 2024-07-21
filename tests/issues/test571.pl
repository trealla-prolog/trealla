:-initialization(main).

% Traversing graph paths

'https://josd.github.io/cigol#oneway'('http://example.org/#paris','http://example.org/#orleans').
'https://josd.github.io/cigol#oneway'('http://example.org/#paris','http://example.org/#chartres').
'https://josd.github.io/cigol#oneway'('http://example.org/#paris','http://example.org/#amiens').
'https://josd.github.io/cigol#oneway'('http://example.org/#orleans','http://example.org/#blois').
'https://josd.github.io/cigol#oneway'('http://example.org/#orleans','http://example.org/#bourges').
'https://josd.github.io/cigol#oneway'('http://example.org/#blois','http://example.org/#tours').
'https://josd.github.io/cigol#oneway'('http://example.org/#chartres','http://example.org/#lemans').
'https://josd.github.io/cigol#oneway'('http://example.org/#lemans','http://example.org/#angers').
'https://josd.github.io/cigol#oneway'('http://example.org/#lemans','http://example.org/#tours').
'https://josd.github.io/cigol#oneway'('http://example.org/#angers','http://example.org/#nantes').

'https://josd.github.io/cigol#path'(A,B) :-
    'https://josd.github.io/cigol#oneway'(A,B).
'https://josd.github.io/cigol#path'(A,C) :-
    'https://josd.github.io/cigol#oneway'(A,B),
    'https://josd.github.io/cigol#path'(B,C).

% query
query('https://josd.github.io/cigol#path'(_City,'http://example.org/#nantes')).

main :-
    query(Q),
    Q,
    writeq(Q),
    write('.\n'),
    fail;
    true.
