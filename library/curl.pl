:- module(curl, [
	curl_easy_init/1,
	curl_easy_setopt/4,
	curl_easy_perform/2,
	curl_easy_cleanup/1
	]).


:- use_foreign_module('libcurl.so', [
	curl_easy_init([], ptr),
	curl_easy_setopt([ptr,uint,cstr], sint),
	curl_easy_perform([ptr], sint),
	curl_easy_cleanup([ptr], void)
	]).
