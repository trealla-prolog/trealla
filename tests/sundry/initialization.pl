% An initialization/1 goal runs when the file that recorded it has
% finished loading - not when some nested load finishes.
%
% library(builtins) and library(iso_ext) have no module directive of
% their own, so they load into whatever module is consulting them. The
% run_init flag lives on the module, so the end of that nested load
% used to fire the consulting file's goals, before the rest of the
% file that defines them had been read: main/0 did not exist yet.

:- initialization(main).

:- write(before), nl.
:- use_module(library(iso_ext)).
:- write(after), nl.


% one recorded after the nested load, to show they still run in the
% order they were seen and still run at the end

:- initialization((write(second), nl)).

main :- write(main_ran), nl.
