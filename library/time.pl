/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Written 2020-2024 by Markus Triska (triska@metalevel.at)
   Part of Scryer Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/** This library provides predicates for reasoning about time.
*/

:- module(time, [current_time/1,
                 format_time//2
                ]).

:- use_module(library(format)).
:- use_module(library(iso_ext)).
:- use_module(library(error)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(charsio), [read_from_chars/2]).

%% current_time(-T)
%
%  Yields the current system time _T_ in an opaque form, called a
%  _time stamp_. Use `format_time//2` to describe strings that contain
%  attributes of the time stamp.

current_time(T) :-
	date_time(Y, M, D, HH, MM, SS),
	format(string(Y2), "~d", [Y]),
	format(string(M2), "~|~`0t~d~2+", [M]),
	format(string(D2), "~|~`0t~d~2+", [D]),
	format(string(HH2), "~|~`0t~d~2+", [HH]),
	format(string(MM2), "~|~`0t~d~2+", [MM]),
	format(string(SS2), "~|~`0t~d~2+", [SS]),
	Yy is Y mod 100, format(string(Yy2), "~|~`0t~d~2+", [Yy]),
	T = [
		'Y'= Y2,
		'm'= M2,
		'd'= D2,
		'H'= HH2,
		'M'= MM2,
		'S'= HH2,
		'y'= Yy2
	].

%% format_time(FormatString, TimeStamp)//
%
% The nonterminal format_time//2 describes a list of characters that
% are formatted according to a format string. Usage:
%
% ```
%    phrase(format_time(FormatString, TimeStamp), Cs)
% ```
%
% TimeStamp represents a moment in time in an opaque form, as for
% example obtained by `current_time/1`.
%
% FormatString is a list of characters that are interpreted literally,
% except for the following specifiers (and possibly more in the future):
%
% |  `%Y` |  year of the time stamp. Example: 2020.                |
% |  `%m` |  month number (01-12), zero-padded to 2 digits         |
% |  `%d` |  day number (01-31), zero-padded to 2 digits           |
% |  `%H` |  hour number (00-24), zero-padded to 2 digits          |
% |  `%M` |  minute number (00-59), zero-padded to 2 digits        |
% |  `%S` |  second number (00-60), zero-padded to 2 digits        |
% |  `%b` |  abbreviated month name, always 3 letters              |
% |  `%a` |  abbreviated weekday name, always 3 letters            |
% |  `%A` |  full weekday name                                     |
% |  `%j` |  day of the year (001-366), zero-padded to 3 digits    |
% |  `%%` |  the literal `%`                                       |
%
% Example:
%
% ```
%    ?- current_time(T), phrase(format_time("%d.%m.%Y (%H:%M:%S)", T), Cs).
%       T = [...], Cs = "11.06.2020 (00:24:32)".
% ```

format_time([], _) --> [].
format_time(['%','%'|Fs], T) --> !, "%", format_time(Fs, T).
format_time(['%',Spec|Fs], T) --> !,
        (   { member(Spec=Value, T) } ->
            seq(Value)
        ;   { domain_error(time_specifier, Spec, format_time//2) }
        ),
        format_time(Fs, T).
format_time([F|Fs], T) --> [F], format_time(Fs, T).
