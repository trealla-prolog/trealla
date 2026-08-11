:- use_module(library(clpz)).

% AI Escargot is a well-known difficult 9-by-9 Sudoku. The model is entirely
% declarative: every row, column, and 3-by-3 block is all-distinct, while
% first-fail labeling chooses among the remaining finite-domain cells.

%% goal: sudoku9_solution(X0)

sudoku9_solution(Rows) :-
  Rows = [
    [1, _, _, _, _, 7, _, 9, _],
    [_, 3, _, _, 2, _, _, _, 8],
    [_, _, 9, 6, _, _, 5, _, _],
    [_, _, 5, 3, _, _, 9, _, _],
    [_, 1, _, _, 8, _, _, _, 2],
    [6, _, _, _, _, 4, _, _, _],
    [3, _, _, _, _, _, _, 1, _],
    [_, 4, _, _, _, _, _, _, 7],
    [_, _, 7, _, _, _, 3, _, _]
  ],
  sudoku9_rows(Rows),
  sudoku9_transpose(Rows, Columns),
  sudoku9_rows_distinct(Columns),
  sudoku9_blocks(Rows),
  sudoku9_flatten(Rows, Cells),
  labeling([ff], Cells).

sudoku9_rows([]).
sudoku9_rows([Row|Rows]) :-
  Row ins 1..9,
  all_distinct(Row),
  sudoku9_rows(Rows).

sudoku9_rows_distinct([]).
sudoku9_rows_distinct([Row|Rows]) :-
  all_distinct(Row),
  sudoku9_rows_distinct(Rows).

sudoku9_transpose([[]|_], []).
sudoku9_transpose(Rows, [Column|Columns]) :-
  sudoku9_heads_tails(Rows, Column, Tails),
  sudoku9_transpose(Tails, Columns).

sudoku9_heads_tails([], [], []).
sudoku9_heads_tails([[Head|Tail]|Rows], [Head|Heads], [Tail|Tails]) :-
  sudoku9_heads_tails(Rows, Heads, Tails).

sudoku9_blocks([]).
sudoku9_blocks([A, B, C|Rows]) :-
  sudoku9_block_row(A, B, C),
  sudoku9_blocks(Rows).

sudoku9_block_row([], [], []).
sudoku9_block_row([A, B, C|As], [D, E, F|Bs], [G, H, I|Cs]) :-
  all_distinct([A, B, C, D, E, F, G, H, I]),
  sudoku9_block_row(As, Bs, Cs).

sudoku9_flatten([], []).
sudoku9_flatten([Row|Rows], Cells) :-
  sudoku9_append(Row, Rest, Cells),
  sudoku9_flatten(Rows, Rest).

sudoku9_append([], Ys, Ys).
sudoku9_append([X|Xs], Ys, [X|Zs]) :- sudoku9_append(Xs, Ys, Zs).
