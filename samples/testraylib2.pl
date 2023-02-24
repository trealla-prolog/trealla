:- use_module(library(raylib)).

run :-
	ScreenWidth = 800,
	ScreenHeight = 450,
	'InitWindow'(ScreenWidth, ScreenHeight, "Trealla Prolog Raylib"),

	P1 is ScreenWidth / 2.0,
	P2 is ScreenHeight / 2.0,
    BallPosition = [P1,P2],

    BallSpeed = [5.0,4.0],
    BallRadius = 20.0,

	'SetTargetFPS'(60),
	loop(BallPosition, BallSpeed, BallRadius).

loop(BallPosition, BallSpeed, BallRadius) :-
	(
		'WindowShouldClose'(Close),
		Close =\= 0 ->
		'CloseWindow'
	;	(
		'GetScreenWidth'(ScreenWidth),
		'GetScreenHeight'(ScreenHeight),

		BallPosition = [PosX,PosY],
		BallSpeed = [SpeedX,SpeedY],

		X is PosX + SpeedX,
		Y is PosY + SpeedY,

		% Check walls collision for bouncing

		(
			X >= (ScreenWidth - BallRadius) -> Speed2X is SpeedX * -1.0 ;
			X =< BallRadius -> Speed2X is X * -1.0 ;
			Speed2X is X
		),

		(
			Y >= (ScreenHeight - BallRadius) -> Speed2Y is SpeedY * -1.0 ;
			Y =< BallRadius -> Speed2Y is Y * -1.0 ;
			Speed2Y is Y
		),

		BallPosition2a = [X,Y],
		BallPosition2b = [vector2,X,Y],
		BallSpeed2 = [Speed2X,Speed2Y],

		'BeginDrawing',
		RAYWHITE = [color,245,245,245,245],
		'ClearBackground'(RAYWHITE),

		MAROON = [color,190,33,55,255],
		'DrawCircleV'(BallPosition2b, BallRadius, MAROON),

		'DrawFPS'(10, 10),
		'EndDrawing',

		loop(BallPosition2a, BallSpeed2, BallRadius)
		)
	).
