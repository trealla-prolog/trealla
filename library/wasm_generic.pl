:- module(wasm_generic, [host_rpc]).
:- use_module(library(wasm)).
:- use_module(library(pseudojson)).

host_call(Expr, Cs) :-
	'$host_call'(Expr, Cs), !
	; yield, '$host_resume'(Cs).

host_rpc(Goal) :-
	unique_variable_names(Goal, VNs),
	wasm:term_json(VNs, Goal, ArgValue),
	json_value(JSON, ArgValue),
	json_chars(JSON, Req),
	host_call(Req, Resp),
	read_term_from_chars(Resp, Reply, [variable_names(VNs1)]),
	merge_vars(VNs, VNs1),
	( Reply = throw(Ball) -> throw(Ball) ; true),
	( Reply = fail -> fail ; true),
	!,
	Goal = Reply.

merge_vars(L0, L2) :-
	maplist(merge_vars_(L2), L0).
merge_vars_(L, Name=Var) :-
	(  memberchk(Name=V1, L)
	-> Var = V1 
	;  true
	).

% from format/charsio:

unique_variable_names(Term, VNs) :-
	term_variables(Term, Vs),
	foldl(var_name, Vs, VNs, 0, _).

var_name(V, Name=V, Num0, Num) :-
	fabricate_var_name(numbervars, Name, Num0),
	Num is Num0 + 1.

fabricate_var_name(VarType, VarName, N) :-
	char_code('A', AC),
	LN is N mod 26 + AC,
	char_code(LC, LN),
	NN is N // 26,
	(  NN =:= 0 ->
	( VarType == fabricated ->
		atom_chars(VarName, ['_', LC])
	; VarType == numbervars ->
		atom_chars(VarName, [LC])
	)
	;  number_chars(NN, NNChars),
	( VarType == fabricated ->
		atom_chars(VarName, ['_', LC | NNChars])
	; VarType == numbervars ->
		atom_chars(VarName, [LC | NNChars])
	)
	).
