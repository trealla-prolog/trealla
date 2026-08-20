% split_string/4 followed SWI only loosely: sep and pad were single
% characters rather than sets, pad was stripped from the front of a
% field but not the back, empty fields were dropped, and a nil argument
% reached C_STR() as the atom name "[]" - so the common SWI idiom
% split_string(S, "", " \n", [Trimmed]) split S on '[' and ']'.
%
% Expected output is SWI's, checked against it on these cases.

:- initialization(main).

canon(F, Cs) :- ( is_list(F) -> Cs = F ; atom_chars(F, Cs) ).

go(S, Sep, Pad) :-
	split_string(S, Sep, Pad, R),
	maplist(canon, R, Cs),
	format("~q~n", [Cs]).

main :-
	go("  x  ", "", " "),			% pad stripped at both ends
	go("a[b]c", "", ""),			% "" is not the set {[,]}
	go("", "", ""),				% always at least one field
	go("  ", "", " "),
	go("/home//jan///nice/path", "/", ""),	% empty fields survive
	go("a,b;c", ",;", ""),			% sep is a set
	go("a, b ; c ", ",;", " "),
	go("abc", "", "cba"),			% all padding
	go(".a.b.c.", ".", "."),		% sep and pad overlap
	go(",,,", ",", ""),
	go("xxaxx", "a", "x"),
	go("héllo·wörld", "·", ""),		% multibyte sep
	go("  héllo  ", "", " ").
