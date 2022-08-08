:- use_module(library(http)).

handle(C, "GET", "/", Ver, Hdrs) :-
	format(C, "HTTP/~w 200 Ok\r\nConnection: close\r\nContent-Type: text/html\r\n\r\n", [Ver]),
	format(C, "<html><body><h1>Home</h1><h2>~w</h2></body></html>\n", [Hdrs]).
handle(C, _, _, Ver, _) :-
	format(C, "HTTP/~w 500 Server Error\r\nConnection: close\r\nContent-Type: text/html\r\n\r\n", [Ver]),
	bwrite(C, "<html><body><h1>500 Server Error</h1></body></html>\n").

main :-
	fork,
	server(":8080", S, []),
	accept(S, C),
		fork,
		http_request(C, Method, Path, Ver, Hdrs),
		writeq([Method, Path, Ver, Hdrs]), nl,
		handle(C, Method, Path, Ver, Hdrs),
		close(C).

main :-
	wait.
