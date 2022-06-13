:- module(utils, [
    between_unordered/3,
    in_board/2, % tests if the cell is in board, can be also used for iterating over all cells.
    lighted/2,
    valid/2, % valid for placing a light
    adjacent_cell/4,
    adjacent_cells/3,
    
    reachable/4,
    % reach_dir(R,C, RT,CT).
    % - [R,C] for the cell you are at.
    % - [RT,CT] is the result cell.
    % examples:
    reach_up/4, % - reach_up(5,5, 4,5).
    reach_down/4, % - reach_down(5,5, 6,5).
    reach_left/4, % - reach_left(5,5, 5,4).
    reach_right/4, % - reach_right(5,5, 5,6).

    reach_horizontal/4,
    reach_vertical/4
]).

:- use_module(board).

% complexity: O(1).
%
% same of between/2, but doesn't need it's parameters ordered.
between_unordered(A, B, V):- between(A, B, V);between(B, A, V).

% complexity: O(1).
%
% checks or generates cells in-board.
in_board(R, C):-
    size(Columns, Rows),
    between(1, Rows, R),
    between(1, Columns, C).

% complexity: O(N) where N is the board dimension.
reach_up(R,C, RT,C):- RT is R-1, RT > 0, \+ wall(RT,C).
reach_up(R,C, RT,C):- R2 is R-1, R2 > 1, \+ wall(R2,C), reach_up(R2,C, RT,C).

% complexity: O(N) where N is the board dimension.
reach_left(R,C, R,CT):- CT is C-1, CT > 0, \+ wall(R,CT).
reach_left(R,C, R,CT):- C2 is C-1, C2 > 1, \+ wall(R,C2), reach_left(R,C2, R,CT).

% complexity: O(N) where N is the board dimension.
reach_down(R,C, RT,C):- size(MAX,_), RT is R+1, RT =< MAX, \+ wall(RT,C).
reach_down(R,C, RT,C):- size(MAX,_), R2 is R+1, R2 < MAX, \+ wall(R2,C), reach_down(R2,C, RT,C).

% complexity: O(N) where N is the board dimension.
reach_right(R,C, R,CT):- size(_,MAX), CT is C+1, CT =< MAX, \+ wall(R,CT).
reach_right(R,C, R,CT):- size(_,MAX), C2 is C+1, C2 < MAX, \+ wall(R,C2), reach_right(R,C2, R,CT).

% complexity: O(N) where N is the board dimension.
reach_horizontal(R,C, RT,CT):- in_board(R,C),(reach_left(R,C,RT,CT);reach_right(R,C,RT,CT)).
% complexity: O(N) where N is the board dimension.
reach_vertical(R,C, RT,CT):- in_board(R,C),(reach_up(R,C,RT,CT);reach_down(R,C,RT,CT)).

/* complexity:

O(N) when the 2 cells are pinned.
O(N) when a cell is pinned.
O(N^2) when no cells are pinned.

where N is the board dimension.
*/

% checks or searches for cells that have a clear line of sight.
% so if one of them was a light, it's ray would reach the other cell.
reachable(R1,C1, R2,C2):-  in_board(R1,C1),(reach_horizontal(R1,C1, R2,C2);reach_vertical(R1,C1, R2,C2)).

% complexity: O(N^2).
%
% checks if a cell is lighted by any light.
% walls, lights and out-of-bound cells are considered lighted.
lighted(R, C):- wall(R,C);light(R,C);\+in_board(R,C).
lighted(R, C):-
    light(RL, CL),
    reachable(R,C, RL, CL).

% complexity: O(N^2) when the parameters are pinned.
%
% checks if it's valid to place a light in this cell,
% verifies that the cell is not lighted.
valid(R, C):-
    in_board(R,C),
    \+ (restricted(R,C);wall(R,C);lighted(R,C)).

% complexity: O(1)
adjacent_cell(R1,C1, R2,C2):- R2 is R1 + 1, C2 is C1.
adjacent_cell(R1,C1, R2,C2):- R2 is R1 - 1, C2 is C1.
adjacent_cell(R1,C1, R2,C2):- R2 is R1, C2 is C1 + 1.
adjacent_cell(R1,C1, R2,C2):- R2 is R1, C2 is C1 - 1.

% complexity: O(1)
adjacent_cells(R,C, Cells):-
    findall([RA,CA], adjacent_cell(R,C, RA,CA), Cells).
