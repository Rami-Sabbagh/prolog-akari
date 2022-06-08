:- module(validation, [
    solved/0
]).

:- use_module(board).

solved:-
    valid_board,
    all_cells_lighted,
    no_double_light,
    light_count_correct.

%-------------%
% valid_board %
%-------------%

valid_board:-
    \+ invalid_board.

invalid_board:-
    detached_wall_num;light_over_wall;cell_out_of_bound.

detached_wall_num:-
    wall_num(R,C,_),
    \+ wall(R,C).

light_over_wall:-
    light(R,C),wall(R,C).

cell_out_of_bound:-
    size(Rows, Columns),
    (light(R,C);wall(R,C);wall_num(R,C,_)),
    \+ (between(1,Rows,R),between(1,Columns,C)).

%-------------------%
% all_cells_lighted %
%-------------------%

all_cells_lighted:-
    size(Rows, Columns),
    forall((between(1,Rows,R),between(1,Columns,C)), lighted(R,C)).

lighted(R, C):- wall(R,C);light(R,C).
lighted(R, C):-
    light(RL, CL),
    reachable(R,C, RL, CL).

%-----------------%
% no_double_light %
%-----------------%

no_double_light:-
    \+ double_light(_,_,_,_).

double_light(R1, C1, R2, C2):-
    light(R1, C1),
    light(R2, C2),
    (C1 \= C2; R1 \= R2),
    reachable(R1, C1, R2, C2).

%---------------------%
% light_count_correct %
%---------------------%

light_count_correct:-
    size(Rows, Columns),
    \+ (
        wall_num(R, C, N),
        findall([RA,CA],(
            adjacent_cell(R,C, RA,CA),
            light(RA,CA),
            between(1, Rows, RA),
            between(1, Columns, CA)
        ), Adj),
        sort(Adj, AdjSet), % remove duplicates (make it a set)
        \+ length(AdjSet, N)
    ).

adjacent_cell(R1,C1, R2,C2):- R2 is R1 + 1, C2 is C1.
adjacent_cell(R1,C1, R2,C2):- R2 is R1 - 1, C2 is C1.
adjacent_cell(R1,C1, R2,C2):- R2 is R1, C2 is C1 + 1.
adjacent_cell(R1,C1, R2,C2):- R2 is R1, C2 is C1 - 1.

%-----------%
% Utilities %
%-----------%

between_unorded(A, B, V):- between(A, B, V);between(B, A, V).

reachable(R1,C1, R2,C2):-
    size(Rows, Columns),
    (R1 = R2; C1 = C2),

    \+ (
        between(1, Rows, R1),
        between(1, Columns, C1),

        between(1, Rows, R2),
        between(1, Columns, C2),
        between_unorded(R1, R2, R),
        between_unorded(C1, C2, C),

        wall(R, C)
    ).


