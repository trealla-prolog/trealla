:- use_module(library(curl)).

cget(Url, Filename) :-
	curlopt(xCURLOPT_WRITEDATA, _, CURLOPT_WRITEDATA),
	curlopt(xCURLOPT_URL, _, CURLOPT_URL),

	curl_easy_init(Curl),
	setup_call_cleanup(
		(
			open(Filename, write, S, [])
		),
		(
			stream_property(S, file(F)),
			curl_easy_setopt(Curl, CURLOPT_WRITEDATA, F, _),
			curl_easy_setopt(Curl, CURLOPT_URL, Url, _),
			curl_easy_perform(Curl, Res),
			Res =:= 0
		),
		(
			close(S)
		)
	),
	curl_easy_cleanup(Curl).

cput(Url, Filename) :-
	curlopt(xCURLOPT_READDATA, _, CURLOPT_READDATA),
	curlopt(xCURLOPT_URL, _, CURLOPT_URL),
	curlopt(xCURLOPT_PUT, _, CURLOPT_PUT),

	curl_easy_init(Curl),
	setup_call_cleanup(
		(
			open(Filename, read, S, [])
		),
		(
			stream_property(S, file(F)),
			curl_easy_setopt(Curl, CURLOPT_READDATA, F, _),
			curl_easy_setopt(Curl, CURLOPT_URL, Url, _),
			curl_easy_setopt(Curl, CURLOPT_PUT, Url, _),
			curl_easy_perform(Curl, Res),
			Res =:= 0
		),
		(
			close(S)
		)
	),
	curl_easy_cleanup(Curl).
