:- module(utils, [
    create_light/2,
    refresh_lighted/0, % used to properly load puzzle's solutions.
    between_unordered/3,
    in_board/2, % tests if the cell is in board, can be also used for iterating over all cells.
    lighted/2,
    valid/2, % valid for placing a light
    adjacent_cell/4,
    adjacent_cells/3,

    lightness/3, % lightness(R,C, Lightness) - calculates the lightness of a cell.
    satisfied/2, % checks if a cell is satisfied.
    
    reachable/4, % alias for reachable(any, R1,C1, R2,C2).
    reachable/5
    % reachable(direction, R,C, RT,CT).
    % - [R,C] for the cell you are at.
    % - [RT,CT] is the result cell.
    % - direction can be: up, down, left, right, row, column, any.
    % examples:
    % - reach(up, 5,5, 4,5).
    % - reach(down, 5,5, 6,5).
    % - reach(left, 5,5, 5,4).
    % - reach(right, 5,5, 5,6).
    % - reach(row, 5,5, 5,6).
    % - reach(column, 5,5, 6,5).
    % - reach(any, 5,5, 6,5).
]).

:- use_module(board).

refresh_lighted:-
    retractall(really_lighted(_,_)),
    forall((light(R,C),\+ really_lighted(R,C)), spread_light(R,C)).

create_really_lighted([R,C]):-
    asserta(really_lighted(R,C), Ref),
    (true;erase(Ref),fail),
    true.

spread_light(R,C):-
    create_really_lighted([R,C]),
    findall([RL,CL], (
        reachable(R,C, RL,CL), \+ really_lighted(RL,CL)
    ), List),
    maplist(create_really_lighted, List).

create_light(R,C):-
    assertz(light(R,C), Ref),
    spread_light(R,C),
    (true;erase(Ref),fail),
    true.

% --- --- --- --- --- %

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
reachable(up, R,C, RT,C):- RT is R-1, RT > 0, \+ wall(RT,C).
reachable(up, R,C, RT,C):- R2 is R-1, R2 > 1, \+ wall(R2,C), reachable(up, R2,C, RT,C).

% complexity: O(N) where N is the board dimension.
reachable(left, R,C, R,CT):- CT is C-1, CT > 0, \+ wall(R,CT).
reachable(left, R,C, R,CT):- C2 is C-1, C2 > 1, \+ wall(R,C2), reachable(left, R,C2, R,CT).

% complexity: O(N) where N is the board dimension.
reachable(down, R,C, RT,C):- size(MAX,_), RT is R+1, RT =< MAX, \+ wall(RT,C).
reachable(down, R,C, RT,C):- size(MAX,_), R2 is R+1, R2 < MAX, \+ wall(R2,C), reachable(down, R2,C, RT,C).

% complexity: O(N) where N is the board dimension.
reachable(right, R,C, R,CT):- size(_,MAX), CT is C+1, CT =< MAX, \+ wall(R,CT).
reachable(right, R,C, R,CT):- size(_,MAX), C2 is C+1, C2 < MAX, \+ wall(R,C2), reachable(right, R,C2, R,CT).

% complexity: O(N) where N is the board dimension.
reachable(row, R,C, RT,CT):- in_board(R,C),(reachable(left, R,C,RT,CT);reachable(right, R,C,RT,CT)).
% complexity: O(N) where N is the board dimension.
reachable(column, R,C, RT,CT):- in_board(R,C),(reachable(up, R,C,RT,CT);reachable(down, R,C,RT,CT)).

/* complexity:
O(N) when first cell is pinned.
O(N^2) when no cells are pinned.

where N is the board dimension.
*/

% checks or searches for cells that have a clear line of sight.
% so if one of them was a light, it's ray would reach the other cell.
reachable(any, R1,C1, R2,C2):- in_board(R1,C1),(reachable(row ,R1,C1, R2,C2);reachable(column, R1,C1, R2,C2)).
reachable(R1,C1, R2,C2):- reachable(any, R1,C1, R2,C2).

% checks if a cell is lighted by any light.
% walls, lights and out-of-bound cells are considered lighted.
lighted(R, C):- really_lighted(R, C).
lighted(R, C):- wall(R,C);light(R,C);\+in_board(R,C).

% complexity: O(N^2) when the parameters are pinned.
%
% checks if it's valid to place a light in this cell,
% verifies that the cell is not lighted.
valid(R, C):-
    in_board(R,C),
    \+ (restricted(R,C);wall(R,C);lighted(R,C)).

lightness(R,C, L):-
    findall(_,(adjacent_cell(R,C,RA,CA),light(RA,CA)),Lights),
    length(Lights, L).

satisfied(R,C):- wall_num(R,C, N), lightness(R,C, N).

% complexity: O(1)
adjacent_cell(R1,C1, R2,C2):- R2 is R1 + 1, C2 is C1.
adjacent_cell(R1,C1, R2,C2):- R2 is R1 - 1, C2 is C1.
adjacent_cell(R1,C1, R2,C2):- R2 is R1, C2 is C1 + 1.
adjacent_cell(R1,C1, R2,C2):- R2 is R1, C2 is C1 - 1.

% complexity: O(1)
adjacent_cells(R,C, Cells):-
    findall([RA,CA], adjacent_cell(R,C, RA,CA), Cells).
