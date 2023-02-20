:- module(raylib, [
	'InitWindow'/3,
	'WindowShouldClose'/1,
	'SetTargetFPS'/1,
	'BeginDrawing'/0,
	'ClearBackground'/1,
	'DrawText'/5,
	'EndDrawing'/0,
	'CloseWindow'/0
	]).

:- foreign_struct(color, [uint8,uint8,uint8,uint8]).

:- use_foreign_module('libraylib.so', [
	'InitWindow'([sint,sint,cstr], void),
	'SetTargetFPS'([sint], void),
	'WindowShouldClose'([], bool),
	'BeginDrawing'([], void),
	'ClearBackground'([color], void),
	'DrawText'([cstr,sint,sint,sint,color], void),
	'EndDrawing'([], void),
	'CloseWindow'([], void)
	]).

