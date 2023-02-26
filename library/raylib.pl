:- module(raylib, [
	'InitWindow'/3,
	'WindowShouldClose'/1,
	'SetTargetFPS'/1,
	'GetWindowPosition'/1,
	'GetScreenWidth'/1,
	'GetScreenHeight'/1,
	'IsKeyPressed'/2,
	'BeginDrawing'/0,
	'ClearBackground'/1,
	'DrawText'/5,
	'DrawFPS'/2,
	'DrawCircle'/4,
	'DrawCircleV'/3,
	'EndDrawing'/0,
	'CloseWindow'/0
	]).

:- foreign_struct(float2, [float,float]).
:- foreign_struct(float4, [float,float,float,float]).
:- foreign_struct(matrix2, [matrix,matrix]).
:- foreign_struct(char32, [uint64,uint64,uint64,uint64]).

:- foreign_struct(color, [uint8,uint8,uint8,uint8]).
:- foreign_struct(vector2, [float,float]).
:- foreign_struct(vector3, [float,float,float]).
:- foreign_struct(vector4, [float,float,float,float]).
:- foreign_struct(quaternion, [float,float,float,float]).
:- foreign_struct(matrix, [
		float,float,float,float,
		float,float,float,float,
		float,float,float,float,
		float,float,float,float
		]).
:- foreign_struct(rectangle, [float,float,float,float]).
:- foreign_struct(image, [ptr,sint,sint,sint,sint]).
:- foreign_struct(texture, [uint,sint,sint,sint,sint]).
:- foreign_struct(texture2d, [uint,sint,sint,sint,sint]).
:- foreign_struct(texturecubemap, [uint,sint,sint,sint,sint]).
:- foreign_struct(rendertexture, [uint,texture,texture]).
:- foreign_struct(rendertexture2d, [uint,texture,texture]).
:- foreign_struct(npatchinfo, [rectangle,sint,sint,sint,sint,sint]).
:- foreign_struct(glyphinfo, [sint,sint,sint,sint,image]).
:- foreign_struct(font, [sint,sint,sint,texture2d,ptr,ptr]).
:- foreign_struct(camera3d, [vector3,vector3,vector3,float,sint]).
:- foreign_struct(camera, [vector3,vector3,vector3,float,sint]).
:- foreign_struct(camera2d, [vector2,vector2,float,float]).
:- foreign_struct(mesh, [sint,sint,ptr,ptr,ptr,ptr,ptr,ptr,ptr,ptr,ptr,ptr,ptr,uint,ptr]).
:- foreign_struct(shader, [uint,ptr]).
:- foreign_struct(materialmap, [texture2d,color,float]).
:- foreign_struct(material, [shader,ptr,float4]).
:- foreign_struct(transform, [vector3,quaternion,vector3]).
:- foreign_struct(boneinfo, [char32,sint]).
:- foreign_struct(model, [matrix,sint,sint,ptr,ptr,ptr,sint,ptr,ptr]).
:- foreign_struct(modelaninimation, [sint,sint,ptr]).
:- foreign_struct(ray, [vector3,vector3]).
:- foreign_struct(raycollision, [bool,float,vector,vector3]).
:- foreign_struct(boundingbox, [vector3,vector3]).
:- foreign_struct(wave, [uint,uint,uint,uint,ptr]).
:- foreign_struct(audiostream, [ptr,ptr,uint,uint,uint]).
:- foreign_struct(sound, [audiosound,uint]).
:- foreign_struct(music, [audiostream,uint,bool,sint,ptr]).
:- foreign_struct(vrdeviceinfo, [sint,sint,float,float,float,float,float,float,float4,float4]).
:- foreign_struct(vrdevicestereoconfig, [matrix2,matrix2,float2,float2,float2,float2,float2,float2]).
:- foreign_struct(filepathlist, [uint,uint,ptr]).

:- use_foreign_module('libraylib.so', [
	'InitWindow'([sint,sint,cstr], void),
	'SetTargetFPS'([sint], void),
	'WindowShouldClose'([], bool),
	'GetWindowPosition'([], vector2),
	'GetScreenWidth'([], sint),
	'GetScreenHeight'([], sint),
	'IsKeyPressed'([sint], bool),
	'BeginDrawing'([], void),
	'ClearBackground'([color], void),
	'DrawText'([cstr,sint,sint,sint,color], void),
	'DrawFPS'([sint,sint], void),
	'DrawCircle'([sint,sint,float,color], void),
	'DrawCircleV'([vector2,float,color], void),
	'EndDrawing'([], void),
	'CloseWindow'([], void)
	]).
