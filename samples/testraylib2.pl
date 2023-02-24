:- use_module(library(raylib)).

run :-
	ScreenWidth = 800,
	ScreenHeight = 450,
	'InitWindow'(ScreenWidth, ScreenHeight, "Trealla Prolog Raylib"),

	P1 is ScreenWidth / 2.0,
	P2 is ScreenHeight / 2.0,
    BallPosition = [P1,P2],

    BallSpeed = [5.0,4.0],
    BallRadius = 20,

	'SetTargetFPS'(60),
	loop(BallPosition, BallSpeed, BallRadius).

loop(BallPosition, BallSpeed, BallRadius) :-
	(
		'WindowShouldClose'(Close),
		Close =\= 0 ->
		'CloseWindow'
	;	(
		BallPosition = [PosX,PosY],
		BallSpeed = [SpeedX,SpeedY],

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

		BallPosition2 = [X,Y],
		BallPosition2Vec2D = [vector2,X,Y],
		BallSpeed2 = [Speed2X,Speed2Y],

		'BeginDrawing',
		RAYWHITE = [color,245,245,245,245],
		'ClearBackground'(RAYWHITE),

		MAROON = [color,190,33,55,255],
		BallRadius2 is float(BallRadius),
		'DrawCircleV'(BallPosition2Vec2D, BallRadius2, MAROON),

		'DrawFPS'(10, 10),
		'EndDrawing',

		loop(BallPosition2, BallSpeed2, BallRadius)
		)
	).
