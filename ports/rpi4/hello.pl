% A minimal freestanding application: the bare-metal counterpart of the
% standalone-executable example in README.md.
%
%   make rpi4-app main=ports/rpi4/hello.pl
%
% initialization/1 runs at the end of the consult, and samples/freestanding_app.c
% halts the board afterwards, so the program needs no halt of its own.

:- initialization(main).

main :-
    write('Hello from bare metal'), nl.
