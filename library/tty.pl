:- module(tty, [
	tty_clear/0,
	tty_flash/0,
	tty_size/2,
	tty_goto/2,
	tty_action/1,
	tty_nl/1,
	ttyflush/0,
	menu/3,

	% SWI has this as a global builtin, and no menu-like program can be
	% written without it. Re-exported so that use_module(library(tty))
	% is enough on its own - relying on charsio leaking through from the
	% import below works by accident, not by rule.
	get_single_char/1
	]).

% Terminal operations, after SWI-Prolog's library(tty).
%
% Three sections: simple operations on the terminal, a set of screen
% actions, and a generic predicate for building simple menus.
%
% SWI drives the terminal through termcap, so it needs a terminfo entry
% and links against ncurses. This module writes ANSI/VT100 escapes
% directly instead. Every terminal emulator in use understands them,
% including Windows 10+ consoles, and it keeps the module free of a
% foreign dependency - a failed use_foreign_module/2 aborts loading of
% the whole module, so a termcap version would be unusable anywhere
% ncurses was missing rather than merely degraded.
%
% Two consequences of that choice. There is no tty_get_capability/3 or
% tty_put/2: there are no capabilities to look up. And nothing emits
% termcap's padding delays, which matter only for serial terminals that
% cannot keep up with the escape sequence.
%
% SWI also adds ~T and ~l directives to format/2 via format_predicate/2,
% which Trealla does not have. tty_action/1 and tty_nl/1 below take the
% same argument terms, so
%
%     format('~T~l', [clear])        becomes    tty_action(clear), tty_nl(1)
%     format('~T', [goto(0,10)])     becomes    tty_action(goto(0,10))

:- use_module(library(lists)).
:- use_module(library(charsio)).

% ANSI escapes. '\x1b\' is the ISO hexadecimal escape for ESC; the
% trailing backslash terminates it.

esc(clear,         '\x1b\[H\x1b\[2J').	% clear display and home
esc(clear_line,    '\x1b\[K').			% clear to end of line
esc(clear_display, '\x1b\[J').			% clear to end of display
esc(flash_on,      '\x1b\[?5h').		% reverse video
esc(flash_off,     '\x1b\[?5l').

%% tty_clear is det.
%
% Clear the display.

tty_clear :-
	esc(clear, S),
	write(S),
	ttyflush.

%% tty_flash is det.
%
% Give a visual signal if possible, otherwise beep. The reverse-video
% flash has to be held briefly or nothing is drawn between the two
% escapes; this is what termcap spends its padding on.

tty_flash :-
	esc(flash_on, On),
	esc(flash_off, Off),
	write(On),
	ttyflush,
	sleep(0.05),
	write(Off),
	ttyflush.

%% ttyflush is det.

ttyflush :-
	flush_output.

%% tty_size(-Rows, -Cols) is det.
%
% The size of the terminal, in the same argument order as SWI's builtin.
%
% '$tty_size'/2 is an ioctl(TIOCGWINSZ) and reports the live window, so
% it stays right across a resize. It fails when no standard stream is a
% terminal - output redirected to a file, say - and then LINES/COLUMNS
% are worth a try even though a shell usually keeps them to itself.
% Falling back on 24x80 keeps callers from having to handle failure.

tty_size(Rows, Cols) :-
	(	'$tty_size'(R, C)
	->	true
	;	env_num('LINES', R, 24),
		env_num('COLUMNS', C, 80)
	),
	Rows = R,
	Cols = C.

env_num(Name, Value, _) :-
	getenv(Name, Atom),
	atom_number(Atom, Value),
	integer(Value),
	Value > 0,
	!.
env_num(_, Default, Default).

%% tty_goto(+X, +Y) is det.
%
% Put the cursor at column X, row Y, both counted from 0. The argument
% order is SWI's - column first - while the ANSI sequence wants row
% first and counts from 1.

tty_goto(X, Y) :-
	Row is Y + 1,
	Col is X + 1,
	format("\x1b\[~d;~dH", [Row, Col]),
	ttyflush.

%% tty_action(+Action) is det.
%
% Perform a screen action, or a list of them. Takes the terms SWI passes
% to its ~T format directive:
%
%   * goto(X, Y)     - cursor to column X, row Y, from 0
%   * home           - same as goto(0, 0)
%   * clear          - clear the display and home the cursor
%   * clear_line     - clear from the cursor to the end of the line
%   * clear_display  - clear from the cursor to the end of the display
%   * center(Text)   - write Text centred on the line
%   * back(N)        - move the cursor back N columns
%   * flush          - flush pending output

tty_action([]) :- !.
tty_action([A|B]) :-
	!,
	tty_action(A),
	tty_action(B).
tty_action(goto(X, Y)) :-
	!,
	tty_goto(X, Y).
tty_action(home) :-
	!,
	tty_goto(0, 0).
tty_action(flush) :-
	!,
	ttyflush.
tty_action(center(Text)) :-
	!,
	% SWI writes tty_size(W, _) here, which centres inside the row count
	% rather than the width. Cols is what centring wants.
	tty_size(_, Cols),
	format("~t~a~t~*|", [Text, Cols]).
tty_action(back(N)) :-
	!,
	forall(between(1, N, _), put_code(8)).
tty_action(Action) :-
	esc(Action, S),
	!,
	write(S).
tty_action(Action) :-
	throw(error(domain_error(tty_action, Action), tty_action/1)).

%% tty_nl(+N) is det.
%
% N newlines, each clearing the rest of the line first. This is SWI's ~l
% directive; `default` means one line.

tty_nl(default) :-
	!,
	tty_nl(1).
tty_nl(N) :-
	esc(clear_line, Ce),
	forall(between(1, N, _), (write(Ce), nl)).


                 /*******************************
                 *             MENU             *
                 *******************************/

%% menu(+Title, +Options, -Choice) is semidet.
%
% Show a menu. The display is cleared, the title is centred at the top,
% the options are listed, and the user's choice is returned. The screen
% looks like this:
%
% ```
%         --------------------------------------------
%         |                                          |
%         |                  Title                   |
%         |                                          |
%         |   1) Option One                          |
%         |   2) Option Two                          |
%         |   3) Quit                                |
%         |                                          |
%         |   Your Choice? *                         |
%         |                                          |
% ```
%
% An item is selected by pressing its number, or the first letter of its
% text. If more than one option matches, the common prefix of those
% matching is echoed and the next character is read. Illegal input
% flashes the screen.
%
% Text fields - the title and the option texts - are either plain atoms
% or terms Fmt/Args, the latter formatted with format/3.
%
% An option is a term PrologName:UserName. PrologName is returned as the
% choice; UserName is displayed. Entries are numbered automatically.
%
% ```
% get_action(Choice) :-
%         menu('Title',
%                 [ option_1 : 'Option One'
%                 , option_2 : 'Option Two'
%                 , quit     : 'Quit'
%                 ], Choice).
% ```

menu(Title, List, Choice) :-
	show_title(Title),
	build_menu(List),
	get_answer(List, Choice).

show_title(Title) :-
	to_text(Title, T),
	tty_action(clear),
	tty_nl(1),
	tty_action(center(T)),
	tty_nl(2).

build_menu(List) :-
	build_menu(List, 1),
	nl,
	format("      Your choice? ", []),
	tty_action(clear_display),
	ttyflush.

build_menu([], _).
build_menu([_:H|T], N) :-
	to_text(H, TH),
	format("~t~d~6|) ~a", [N, TH]),
	tty_nl(1),
	succ(N, NN),
	build_menu(T, NN).

to_text(Fmt/Args, Text) :-
	!,
	format(atom(Text), Fmt, Args).
to_text(Text, Text).

:- dynamic(menu_indent/1).

menu_indent(Old, New) :-
	(	retract(menu_indent(Old0))
	->	Old = Old0
	;	Old = 0
	),
	assertz(menu_indent(New)).

get_answer(List, Choice) :-
	menu_indent(_, 0),
	get_answer(List, [], Choice).

get_answer(List, Prefix, Choice) :-
	get_single_char(A),
	process_answer(A, List, Prefix, NewPrefix, Ch, Ok),
	(	Ok == yes
	->	Ch = Choice,
		nl
	;	get_answer(List, NewPrefix, Choice)
	).

% get_single_char/1 hands back a character here, where SWI's hands back
% a code, so the whole menu works in characters.

process_answer(Del, _, _, [], _, no) :-
	rubout(Del),
	!,
	feedback('').
process_answer(D, List, _, _, Choice, yes) :-
	char_type(D, decimal_digit),
	atom_number(D, N),
	nth1(N, List, Choice:Name),
	!,
	feedback(Name).
process_answer(D, _, _, [], _, no) :-
	char_type(D, decimal_digit),
	!,
	feedback(''),
	tty_flash.
process_answer(C, List, Prefix, NewPrefix, Choice, Ok) :-
	append(Prefix, [C], NPrefix),
	matching(List, NPrefix, Matching),
	(	Matching == []
	->	tty_flash,
		NewPrefix = Prefix,
		Ok = no
	;	Matching = [Choice:Name]
	->	Ok = yes,
		feedback(Name)
	;	common_prefix(Matching, NewPrefix),
		feedback(NewPrefix),
		Ok = no
	).

rubout(C) :- char_code(C, 127).
rubout(C) :- char_code(C, 8).

matching([], _, []).
matching([H|T], Prefix, [H|R]) :-
	prefix_of(Prefix, H),
	!,
	matching(T, Prefix, R).
matching([_|T], Prefix, R) :-
	matching(T, Prefix, R).

prefix_of(Prefix, _:Name) :-
	text_chars(Name, Chars),
	common_prefix_chars(Prefix, Chars, Prefix),
	!.

common_prefix([_:Name|T], Prefix) :-
	text_chars(Name, Chars),
	common_prefix(T, Chars, Prefix).

common_prefix([], Prefix, Prefix).
common_prefix([_:Name|T], Sofar, Prefix) :-
	text_chars(Name, Chars),
	common_prefix_chars(Chars, Sofar, NewSofar),
	common_prefix(T, NewSofar, Prefix).

common_prefix_chars([H1|T1], [H2|T2], [H1|R]) :-
	lower(H1, L),
	lower(H2, L),
	!,
	common_prefix_chars(T1, T2, R).
common_prefix_chars(_, _, []).

% char_type/2 gives back the folded character as a one-element string,
% so it comes apart with a unification rather than atom_chars/2.

lower(C, L) :-
	(	char_type(C, lower([L0]))
	->	L = L0
	;	L = C
	).

text_chars(Text, Chars) :-
	(	atom(Text)
	->	atom_chars(Text, Chars)
	;	Chars = Text
	).

feedback(Text) :-
	atomic(Text),
	!,
	atom_length(Text, New),
	menu_indent(Old, New),
	tty_action(back(Old)),
	format("~a", [Text]),
	tty_action(clear_line),
	ttyflush.
feedback(Text) :-
	length(Text, New),
	menu_indent(Old, New),
	tty_action(back(Old)),
	format("~s", [Text]),
	tty_action(clear_line),
	ttyflush.
