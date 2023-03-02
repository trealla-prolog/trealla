:- use_module(library(curl)).

run :-
	setup_call_cleanup(
		curl_easy_init(Curl),
		(
			once(curlopt(xCURLOPT_URL, _, Opt)),
			curl_easy_setopt(Curl, Opt, "https://example.com", Res1),
			Res1 =:= 0,
			curl_easy_perform(Curl, Res2),
			Res2 =:= 0
		),
		curl_easy_cleanup(Curl)
	).

