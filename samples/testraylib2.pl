:- use_module(library(raylib)).

run :-
	ScreenWidth = 800,
	ScreenHeight = 450,
	'InitWindow'(ScreenWidth, ScreenHeight, "Trealla Prolog Raylib"),

	X is ScreenWidth / 2.0,
	Y is ScreenHeight / 2.0,
    BallRadius = 20,

	'SetTargetFPS'(60),
	loop(X, Y, 5.0, 4.0, BallRadius).

loop(PosX, PosY, SpeedX, SpeedY, BallRadius) :-
	'WindowShouldClose'(Close),
	(
		Close =\= 0
	-> 'CloseWindow'
	;	(
		X is PosX + SpeedX,
		Y is PosY + SpeedY,

		% Check walls collision for bouncing

		'GetScreenWidth'(ScreenWidth),
		'GetScreenHeight'(ScreenHeight),

		(
			X >= (ScreenWidth - BallRadius) -> Speed2X is SpeedX * -1.0 ;
			X =< BallRadius -> Speed2X is SpeedX * -1.0 ;
			Speed2X is SpeedX
		),

		(
			Y >= (ScreenHeight - BallRadius) -> Speed2Y is SpeedY * -1.0 ;
			Y =< BallRadius -> Speed2Y is SpeedY * -1.0 ;
			Speed2Y is SpeedY
		),

		BallPosition2 = [vector2,X,Y],
		BallRadius2 is float(BallRadius),

		RAYWHITE = [color,245,245,245,245],
		MAROON = [color,190,33,55,255],

		NewX is floor(X),
		NewY is floor(Y),

		'BeginDrawing',
		'ClearBackground'(RAYWHITE),
		'DrawCircle'(NewX, NewY, BallRadius2, MAROON),
		%'DrawCircleV'(BallPosition2, BallRadius2, MAROON),
		'DrawFPS'(10, 10),
		'EndDrawing',

		loop(X, Y, Speed2X, Speed2Y, BallRadius)
		)
	).
