:- module(uri, [
	uri_components/2,
	uri_data/3,
	uri_data/4,
	uri_normalized/2,
	uri_normalized/3,
	iri_normalized/2,
	iri_normalized/3,
	uri_normalized_iri/2,
	uri_normalized_iri/3,
	uri_is_global/1,
	uri_resolve/3,
	uri_query_components/2,
	uri_authority_components/2,
	uri_authority_data/3,
	uri_encoded/3,
	uri_iri/2,
	uri_file_name/2,
	uri_edit/3
	]).

% Process URIs, after SWI-Prolog's library(uri).
%
% The RFC-3986 string handling lives in C ('$uri_parse' and friends);
% what is left here is term shuffling. Components come back exactly as
% they appeared in the URI - still percent-encoded - because only the
% caller knows which component it is holding and so which character set
% applies to it.

% uri_components(?URI, ?Components)
%
% A URN gets its own functor rather than a fifth reading of
% uri_components/5: its second and third arguments are a namespace id
% and a namespace-specific string, which have nothing to do with an
% authority and a path.

uri_components(URI, Components) :-
	var(Components),
	!,
	'$uri_parse'(URI, Scheme, Auth, Path, Search, Fragment),
	(	urn_parts(Scheme, Path, NID, NSS)
	->	Components = urn_components(Scheme, NID, NSS, Search, Fragment)
	;	Components = uri_components(Scheme, Auth, Path, Search, Fragment)
	).
uri_components(URI, Components) :-
	components_parts(Components, Scheme, Auth, Path, Search, Fragment),
	'$uri_build'(URI, Scheme, Auth, Path, Search, Fragment).

urn_parts(Scheme, Path, NID, NSS) :-
	nonvar(Scheme),
	Scheme == urn,
	nonvar(Path),
	split_at(Path, ':', NID, NSS).

components_parts(uri_components(S,A,P,Q,F), S, A, P, Q, F).
components_parts(urn_components(S,NID,NSS,Q,F), S, _, Path, Q, F) :-
	(	nonvar(NID),
		nonvar(NSS)
	->	atomic_list_concat([NID,':',NSS], Path)
	;	true
	).

% uri_data(?Field, +Components, ?Data)
%
% Left nondet on Field so the fields of a component term can be
% enumerated. A URN has no authority or path to report, and asking for
% one fails rather than binding a variable that means something else.

uri_data(Field, Components, Data) :-
	uri_field(Field, Components, Data).

uri_field(scheme,    uri_components(S,_,_,_,_), S).
uri_field(authority, uri_components(_,A,_,_,_), A).
uri_field(path,      uri_components(_,_,P,_,_), P).
uri_field(search,    uri_components(_,_,_,Q,_), Q).
uri_field(fragment,  uri_components(_,_,_,_,F), F).
uri_field(scheme,    urn_components(S,_,_,_,_), S).
uri_field(nid,       urn_components(_,N,_,_,_), N).
uri_field(nss,       urn_components(_,_,N,_,_), N).
uri_field(search,    urn_components(_,_,_,Q,_), Q).
uri_field(fragment,  urn_components(_,_,_,_,F), F).

% uri_data(?Field, +Components, +Data, -NewComponents)

uri_data(scheme,    uri_components(_,A,P,Q,F), S, uri_components(S,A,P,Q,F)).
uri_data(authority, uri_components(S,_,P,Q,F), A, uri_components(S,A,P,Q,F)).
uri_data(path,      uri_components(S,A,_,Q,F), P, uri_components(S,A,P,Q,F)).
uri_data(search,    uri_components(S,A,P,_,F), Q, uri_components(S,A,P,Q,F)).
uri_data(fragment,  uri_components(S,A,P,Q,_), F, uri_components(S,A,P,Q,F)).
uri_data(scheme,    urn_components(_,N,X,Q,F), S, urn_components(S,N,X,Q,F)).
uri_data(nid,       urn_components(S,_,X,Q,F), N, urn_components(S,N,X,Q,F)).
uri_data(nss,       urn_components(S,N,_,Q,F), X, urn_components(S,N,X,Q,F)).
uri_data(search,    urn_components(S,N,X,_,F), Q, urn_components(S,N,X,Q,F)).
uri_data(fragment,  urn_components(S,N,X,Q,_), F, urn_components(S,N,X,Q,F)).

% uri_normalized(+URI, -Normalized) and the resolving three-argument
% forms. iri_normalized/2 leaves every escape exactly as it found it;
% uri_normalized_iri/2 goes the other way and spells out as much as a
% reader can see.

uri_normalized(URI, Normalized) :-
	'$uri_normalize'(URI, uri, Normalized).

uri_normalized(URI, Base, Normalized) :-
	uri_resolve(URI, Base, Global),
	uri_normalized(Global, Normalized).

iri_normalized(IRI, Normalized) :-
	'$uri_normalize'(IRI, iri, Normalized).

iri_normalized(IRI, Base, Normalized) :-
	uri_resolve(IRI, Base, Global),
	iri_normalized(Global, Normalized).

uri_normalized_iri(URI, Normalized) :-
	uri_normalized(URI, Normalized0),
	uri_iri(Normalized0, Normalized).

uri_normalized_iri(URI, Base, Normalized) :-
	uri_resolve(URI, Base, Global),
	uri_normalized_iri(Global, Normalized).

% uri_is_global(+URI)
%
% More than one character of scheme. A single letter is how a Windows
% drive turns up ("c:/tmp"), and reading that as a scheme would send a
% local path off to a protocol handler.

uri_is_global(URI) :-
	'$uri_parse'(URI, Scheme, _, _, _, _),
	nonvar(Scheme),
	atom_length(Scheme, Len),
	Len > 1.

uri_resolve(URI, Base, Global) :-
	'$uri_resolve'(URI, Base, Global).

% uri_query_components(?String, ?Query)
%
% When building, a pair may be written Name=Value, Name-Value or
% Name(Value). When taking apart, the name runs to the next '=' and the
% value from there to the next '&' - which is why "a&b=1" yields the
% single pair 'a&b'='1' rather than an error about "a".

uri_query_components(String, Query) :-
	var(String),
	!,
	query_pairs(Query, Parts),
	atomic_list_concat(Parts, String).
uri_query_components(String, Query) :-
	parse_query(String, Query).

query_pairs([], ['']).
query_pairs([H|T], [Pair|Rest]) :-
	query_pair(H, Name, Value),
	'$uri_encode'(query_value, Name, EncName),
	value_text(Value, ValueText),
	'$uri_encode'(query_value, ValueText, EncValue),
	atomic_list_concat([EncName,'=',EncValue], Pair),
	(	T == []
	->	Rest = []
	;	Rest = ['&'|Rest0],
		query_pairs(T, Rest0)
	).

query_pair(Name=Value, Name, Value) :- !.
query_pair(Name-Value, Name, Value) :- !.
query_pair(Term, Name, Value) :-
	compound(Term),
	Term =.. [Name,Value].

value_text(Value, Text) :-
	(	number(Value)
	->	number_codes(Value, Codes),
		atom_codes(Text, Codes)
	;	Text = Value
	).

parse_query(String, Query) :-
	(	String == ''
	->	Query = []
	;	split_at(String, '=', RawName, Rest)
	->	'$uri_decode'(query_value, RawName, Name),
		(	split_at(Rest, '&', RawValue, More)
		->	parse_query(More, Tail)
		;	RawValue = Rest,
			Tail = []
		),
		'$uri_decode'(query_value, RawValue, Value),
		Query = [Name=Value|Tail]
	;	throw(error(syntax_error(illegal_uri_query), uri_query_components/2))
	).

% uri_authority_components(?Authority, ?Components)

uri_authority_components(Authority, Components) :-
	var(Components),
	!,
	'$uri_authority_parse'(Authority, User, Password, Host, Port),
	Components = uri_authority(User, Password, Host, Port).
uri_authority_components(Authority, uri_authority(User, Password, Host, Port)) :-
	'$uri_authority_build'(Authority, User, Password, Host, Port).

uri_authority_data(user,     uri_authority(U,_,_,_), U).
uri_authority_data(password, uri_authority(_,P,_,_), P).
uri_authority_data(host,     uri_authority(_,_,H,_), H).
uri_authority_data(port,     uri_authority(_,_,_,N), N).

% uri_encoded(+Component, ?Value, ?Encoded)

uri_encoded(Component, Value, Encoded) :-
	(	nonvar(Value)
	->	'$uri_encode'(Component, Value, Encoded0),
		Encoded = Encoded0
	;	'$uri_decode'(Component, Encoded, Value)
	).

% uri_iri(?URI, ?IRI)

uri_iri(URI, IRI) :-
	(	nonvar(URI)
	->	'$uri_iri'(URI, IRI)
	;	'$iri_uri'(IRI, URI)
	).

% uri_file_name(?URI, ?FileName)
%
% RFC-1738 file URIs, which take an absolute path. Anything that is not
% a file: URI simply is not a file name, so this fails rather than
% raising.

uri_file_name(URI, FileName) :-
	nonvar(URI),
	!,
	'$uri_parse'(URI, Scheme, _, Path, _, _),
	Scheme == file,
	'$uri_decode'(path, Path, FileName).
uri_file_name(URI, FileName) :-
	'$uri_encode'(path, FileName, EncPath),
	atom_concat('file://', EncPath, URI).

% uri_edit(+Actions, +URI0, -URI)
%
% An action with an unbound argument removes that component. A relative
% path extends the existing one instead of replacing it, so
% path(rel) against "http://h/a/b" gives "http://h/a/rel".

uri_edit(Actions, URI0, URI) :-
	(	is_list(Actions)
	->	List = Actions
	;	List = [Actions]
	),
	uri_components(URI0, Components0),
	edit_all(List, URI0, Components0, Components),
	uri_components(URI, Components).

edit_all([], _, Components, Components).
edit_all([Action|T], URI0, Components0, Components) :-
	edit_one(Action, URI0, Components0, Components1),
	edit_all(T, URI0, Components1, Components).

edit_one(Action, URI0, Components0, Components) :-
	Action =.. [Field,Value],
	(	authority_field(Field)
	->	edit_authority(Field, Value, Components0, Components)
	;	Field == path
	->	edit_path(Value, URI0, Components0, Components)
	;	Field == search
	->	search_text(Value, Text),
		uri_data(search, Components0, Text, Components)
	;	uri_data(Field, Components0, Value, Components)
	).

authority_field(user).
authority_field(password).
authority_field(host).
authority_field(port).

edit_authority(Field, Value, Components0, Components) :-
	uri_data(authority, Components0, Authority0),
	(	nonvar(Authority0)
	->	uri_authority_components(Authority0, Auth0)
	;	Auth0 = uri_authority(_,_,_,_)
	),
	set_authority(Field, Value, Auth0, Auth),
	uri_authority_components(Authority, Auth),
	uri_data(authority, Components0, Authority, Components).

set_authority(user,     U, uri_authority(_,P,H,N), uri_authority(U,P,H,N)).
set_authority(password, P, uri_authority(U,_,H,N), uri_authority(U,P,H,N)).
set_authority(host,     H, uri_authority(U,P,_,N), uri_authority(U,P,H,N)).
set_authority(port,     N, uri_authority(U,P,H,_), uri_authority(U,P,H,N)).

edit_path(Value, URI0, Components0, Components) :-
	(	var(Value)
	->	uri_data(path, Components0, _, Components)
	;	sub_atom(Value, 0, 1, _, '/')
	->	uri_data(path, Components0, Value, Components)
	;	% A relative path extends the one already there, which is
		% exactly what resolving it against the original URI does.
		'$uri_resolve'(Value, URI0, Resolved),
		'$uri_parse'(Resolved, _, _, Path, _, _),
		uri_data(path, Components0, Path, Components)
	).

search_text(Value, Text) :-
	(	is_list(Value)
	->	uri_query_components(Text, Value)
	;	Text = Value
	).

% Split at the first occurrence of a one-character separator.

split_at(Atom, Sep, Before, After) :-
	sub_atom(Atom, B, 1, A, Sep),
	!,
	sub_atom(Atom, 0, B, _, Before),
	sub_atom(Atom, _, A, 0, After).
