:- module(utils, [
    between_unordered/3,
    in_board/2, % tests if the cell is in board, can be also used for iterating over all cells.
    reachable/4,
    lighted/2,
    valid/2, % valid for placing a light
    adjacent_cell/4,
    adjacent_cells/3
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

/* complexity:

O(N) when the 2 cells are pinned.
O(N^2) when a cell is pinned.
O(N^3) when no cells are pinned.

where N is the board dimension.
*/

% checks or searches for cells that have a clear line of sight.
% so if one of them was a light, it's ray would reach the other cell.
reachable(R1,C1, R2,C2):-
    (R1 = R2; C1 = C2),

    in_board(R1, C1),
    in_board(R2, C2),

    \+ (
        between_unordered(R1, R2, R),
        between_unordered(C1, C2, C),

        wall(R, C)
    ).

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