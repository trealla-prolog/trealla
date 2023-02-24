:- module(raylib, [
	'InitWindow'/3,
	'WindowShouldClose'/1,
	'SetTargetFPS'/1,
	'GetWindowPosition'/1,
	'BeginDrawing'/0,
	'ClearBackground'/1,
	'DrawText'/5,
	'EndDrawing'/0,
	'CloseWindow'/0
	]).

% NOTE: only implementing a few basic calls. Some others
% would involve complex data types (like pointers to structs
% which is not yet handled).

:- foreign_struct(color, [uint8,uint8,uint8,uint8]).
:- foreign_struct(vector2, [float,float]).
:- foreign_struct(vector3, [float,float,float]).
:- foreign_struct(vector4, [float,float,float,float]).

:- use_foreign_module('libraylib.so', [
	'InitWindow'([sint,sint,cstr], void),
	'SetTargetFPS'([sint], void),
	'WindowShouldClose'([], bool),
	'GetWindowPosition'([], vector2),
	'BeginDrawing'([], void),
	'ClearBackground'([color], void),
	'DrawText'([cstr,sint,sint,sint,color], void),
	'EndDrawing'([], void),
	'CloseWindow'([], void)
	]).

