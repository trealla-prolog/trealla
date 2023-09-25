:- use_module(library(pio)).
:- use_module(library(dcgs)).

like(What) --> "I like ", seq(What), ".", ... .

test :-  phrase_from_file(like(What), 'samples/like.txt'), write_term(What,[double_quotes(true)]), nl, fail; true.

% $ tpl -g test,halt samples/test_phrase_from_file.pl
