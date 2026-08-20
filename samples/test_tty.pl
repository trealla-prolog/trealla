% A demo/tester for library(tty).
%
%     tpl samples/test_tty.pl -g "run,halt"
%
% Needs a real terminal: tty_size/2 reads the live window through
% ioctl(TIOCGWINSZ), and the menu reads single keystrokes with echo off.
% Under a pipe it still runs, but reports the 24x80 fallback.
%
% For a non-interactive check of everything except the menu:
%
%     tpl samples/test_tty.pl -g "report,halt"

:- use_module(library(tty)).
:- use_module(library(lists)).
:- use_module(library(charsio)).		% get_single_char/1

run :-
	menu('library(tty) demo',
		% Distinct initials, so each is reachable by its first letter as
		% well as by its number. Two options starting with the same
		% letter is legal - the menu then asks for one more character -
		% but it makes for a confusing demo.
		[ size    : 'Size of terminal'
		, cursor  : 'Cursor positioning'
		, actions : 'Actions on the screen'
		, flash   : 'Flash the screen'
		, quit    : 'Quit'
		], Choice),
	do(Choice).

do(quit) :-
	!,
	tty_action(clear),
	format("bye~n").
do(Choice) :-
	demo(Choice),
	pause,
	run.

pause :-
	nl,
	format("-- any key to continue --"),
	ttyflush,
	get_single_char(_),
	nl.

% ---------------------------------------------------------------

demo(size) :-
	tty_action(clear),
	tty_size(Rows, Cols),
	format("terminal is ~w rows x ~w columns~n~n", [Rows, Cols]),
	format("resize the window and pick this again - the numbers follow it,~n"),
	format("because tty_size/2 asks the kernel rather than the TERM entry.~n").

% Walk the cursor around the edge of a box to show that tty_goto/2 takes
% (Column, Row), counted from 0, as SWI's does.

demo(cursor) :-
	tty_action(clear),
	tty_size(_, Cols),
	W is min(Cols - 2, 40),
	H = 8,
	box(W, H),
	tty_goto(0, H + 2),
	format("a ~wx~w box drawn with tty_goto/2~n", [W, H]).

demo(actions) :-
	tty_action(clear),
	tty_action(center('centred with tty_action(center(_))')),
	tty_nl(2),
	format("This line gets truncated ->"),
	format(" XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"),
	tty_goto(27, 2),
	tty_action(clear_line),
	tty_nl(2),
	format("      ^ tty_action(clear_line) removed the X's~n"),
	nl,
	format("counting back over a word: abcdef"),
	ttyflush,
	sleep(0.4),
	tty_action(back(6)),
	tty_action(clear_line),
	format("<gone>~n").

demo(flash) :-
	tty_action(clear),
	format("flashing three times...~n"),
	ttyflush,
	forall(between(1, 3, _), (tty_flash, sleep(0.15))),
	format("done - if nothing flashed, the terminal ignores ESC[?5h~n").

% ---------------------------------------------------------------

box(W, H) :-
	Right is W - 1,
	Bottom is H - 1,
	forall(between(0, Right, X), (tty_goto(X, 0), write(-))),
	forall(between(0, Right, X), (tty_goto(X, Bottom), write(-))),
	forall(between(0, Bottom, Y), (tty_goto(0, Y), write('|'))),
	forall(between(0, Bottom, Y), (tty_goto(Right, Y), write('|'))),
	Mid is Bottom // 2,
	MidX is max(0, W // 2 - 6),
	tty_goto(MidX, Mid),
	write('tty_goto/2').

% ---------------------------------------------------------------
% Everything the demo can check without a keystroke. Escapes are shown
% with ESC spelled out, so this is safe to run into a pipe.

report :-
	tty_size(Rows, Cols),
	format("tty_size/2      ~w rows x ~w cols~n", [Rows, Cols]),
	show('tty_goto(9,4)',        tty_goto(9, 4)),
	show('tty_action(clear)',    tty_action(clear)),
	show('tty_action(clear_line)', tty_action(clear_line)),
	show('tty_action(clear_display)', tty_action(clear_display)),
	show('tty_action(home)',     tty_action(home)),
	show('tty_action(back(3))',  tty_action(back(3))),
	show('tty_nl(2)',            tty_nl(2)),
	show('tty_clear',            tty_clear),
	format("tty_action(center(hi))~n  ["),
	with_output_to(atom(C), tty_action(center(hi))),
	write(C), write(']'), nl,
	(	catch(tty_action(no_such_action), E, true)
	->	format("bad action      ~w~n", [E])
	;	format("bad action      NO ERROR - should have thrown~n")
	).

show(Label, Goal) :-
	with_output_to(atom(A), Goal),
	atom_chars(A, Cs),
	format("~w~t~32|", [Label]),
	visible(Cs),
	nl.

visible([]).
visible([C|Cs]) :-
	char_code(C, X),
	(	X =:= 27 -> write('ESC')
	;	X =:= 8  -> write('BS')
	;	X =:= 10 -> write('LF')
	;	X < 32   -> format("^~w", [X])
	;	write(C)
	),
	visible(Cs).
