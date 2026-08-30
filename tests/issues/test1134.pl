:- initialization(main).

% Issue #1134: a double-quoted string closed right at end of line, with
% only a single '|' (not '||') starting the next line, broke parsing -
% not a quads bug as such, but quads' (|)/2 alternative-answer syntax
% is exactly the shape that exposes it. The lookahead for the '||'
% string-concatenation syntax has to eat_space() past the string to
% see whether a second '|' follows; when doing so crosses a line, the
% getline() refill invalidates the pointer that lookahead meant to
% rewind to on finding only one '|', corrupting the rest of the parse.
%
% Both the empty-string and a non-empty-string case are covered here:
% they failed with different symptoms ("incomplete" vs "unexpected
% term") before the fix.

1 ?- L = "".
   L = ""
|  L = _.

2 ?- L = "a".
   L = "a"
|  L = _.

main :-
	use_module(library(quads)),
	run_quads.
