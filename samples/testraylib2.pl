:- use_module(library(raylib)).

run :-
	ScreenWidth = 800,
	ScreenHeight = 450,
	'InitWindow'(ScreenWidth, ScreenHeight, "Trealla Prolog Raylib"),

	X is ScreenWidth / 2.0,
	Y is ScreenHeight / 2.0,

	'SetTargetFPS'(60),
	loop(X, Y, 5.0, 4.0, 20, 0).

loop(PosX, PosY, SpeedX, SpeedY, BallRadius, Paused) :-
	'WindowShouldClose'(Close),

	( Close =\= 0
	-> 'CloseWindow'
	; (
		KEY_SPACE = 32,
		'IsKeyPressed'(KEY_SPACE, Pressed),

		( Pressed =\= 0 -> Paused2 is \Paused ; Paused2 is Paused ),

		(
			Paused2 =\= 0
			-> ( X = PosX, Y = PosY, Speed2X = SpeedX, Speed2Y = SpeedY )
			; (
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
				)
			)
		),

		BallRadius2 is float(BallRadius),
		RAYWHITE = [color,245,245,245,245],
		MAROON = [color,190,33,55,255],

		'BeginDrawing',
		'ClearBackground'(RAYWHITE),

		'DrawCircleV'([vector2,X,Y], BallRadius2, MAROON),

		'DrawFPS'(10, 10),
		'EndDrawing',

		loop(X, Y, Speed2X, Speed2Y, BallRadius, Paused2)
	 )
	).
