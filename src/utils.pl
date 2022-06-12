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

between_unordered(A, B, V):- between(A, B, V);between(B, A, V).

in_board(R, C):-
    size(Columns, Rows),
    between(1, Rows, R),
    between(1, Columns, C).

reachable(R1,C1, R2,C2):-
    (R1 = R2; C1 = C2),

    in_board(R1, C1),
    in_board(R2, C2),

    \+ (
        between_unordered(R1, R2, R),
        between_unordered(C1, C2, C),

        wall(R, C)
    ).

lighted(R, C):- wall(R,C);light(R,C);\+in_board(R,C).
lighted(R, C):-
    light(RL, CL),
    reachable(R,C, RL, CL).

valid(R, C):-
    in_board(R,C),
    \+ (lighted(R,C);restricted(R,C);wall(R,C)).

adjacent_cell(R1,C1, R2,C2):- R2 is R1 + 1, C2 is C1.
adjacent_cell(R1,C1, R2,C2):- R2 is R1 - 1, C2 is C1.
adjacent_cell(R1,C1, R2,C2):- R2 is R1, C2 is C1 + 1.
adjacent_cell(R1,C1, R2,C2):- R2 is R1, C2 is C1 - 1.

adjacent_cells(R,C, Cells):-
    findall([RA,CA], adjacent_cell(R,C, RA,CA), Cells).