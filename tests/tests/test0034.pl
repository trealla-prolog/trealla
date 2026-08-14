:-initialization(main).

main :-
    number_chars(123, ['1','2','3']),
    number_codes(123, [49,50,51]),
    atom_chars('123', ['1','2','3']),
    atom_codes('123', [49,50,51]),
    atom_codes('一二三', [19968,20108,19977]),
    write('PASSED!'), nl.
