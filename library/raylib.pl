:- module(raylib, [
	'InitWindow'/3,
	'BeginDrawing'/0,
	'ClearBackground'/1,
	'DrawText'/5,
	'EndDrawing'/0,
	'CloseWindow'/0
	]).

:- foreign_struct(color, [sint,sint,sint,sint]).

:- use_foreign_module('libraylib.so', [
	'InitWindow'([sint,sint,cstr], void),
	'BeginDrawing'([], void),
	'ClearBackground'([color], void),
	'DrawText'([cstr,sint,sint,sint,color], void),
	'EndDrawing'([], void),
	'CloseWindow'([], void)
	]).

