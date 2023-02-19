:- use_module(library(raylib)).

run :-
	'InitWindow'(800, 450, "Trealla Prolog Raylib"),
	loop.

loop :-
	(
		Close is 'WindowShouldClose',
		Close =\= 0 ->
			'CloseWindow'
	;	(
			'BeginDrawing',
			'ClearBackground'([color,245,245,245,255]),
			'DrawText'("Congrats! You created your first window!", 198, 200, 20, [color,200,200,200,255]),
			'EndDrawing',
			delay(10),
			loop
		)
	).
