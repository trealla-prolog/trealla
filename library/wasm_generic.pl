:- module(wasm_generic, [host_rpc/1]).
:- use_module(library(wasm)).
:- use_module(library(pseudojson)).
:- use_module(library(lists)).

host_call(Expr, Cs) :-
	'$host_call'(Expr, Cs), !
	; yield, '$host_resume'(Cs).

host_rpc(Goal) :-
	unique_variable_names(Goal, Vars0),
	wasm:term_json(Vars0, Goal, ArgValue),
	json_value(JSON, ArgValue),
	json_chars(JSON, Req),
	host_call(Req, Resp),
	read_term_from_chars(Resp, Reply, [variable_names(Vars1)]),
	!,
	host_rpc_eval(Reply, Goal, Vars0, Vars1).

host_rpc_eval(throw(Ball), _, _, _) :- !, throw(Ball).
host_rpc_eval(true, _, _, _) :- !.
host_rpc_eval(fail, _, _, _) :- !, fail.
host_rpc_eval(false, _, _, _) :- !, fail.
host_rpc_eval(call(G), _, Vs0, Vs1) :-
	merge_vars(Vs1, Vs0),
	call(G).
host_rpc_eval(G1, G0, Vs0, Vs1) :-
	G1 \= call(_),
	merge_vars(Vs0, Vs1),
	G0 = G1.

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
