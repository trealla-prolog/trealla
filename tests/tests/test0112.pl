:- initialization(main).

main :-
	load_text(":- module(op_scope_helper, [op(1199, fx, attribute)]).", [module(op_scope_holder)]),
	(   current_op(_, _, attribute)
	->  writeln(leaked)
	;   writeln(scoped)
	),
	read_term_from_atom("{answer-A, attribute-B, value-C}.", _, []),
	writeln(parsed).
