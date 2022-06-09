:- module(validation, [
    solved/0
]).

:- use_module(board).
:- use_module(utils).

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
    \+ (
        wall_num(R, C, N),
        findall([RA,CA],(
            adjacent_cell(R,C, RA,CA),
            light(RA,CA)
        ), Adj),
        sort(Adj, AdjSet), % remove duplicates (make it a set)
        \+ length(AdjSet, N)
    ).

adjacent_cell(R1,C1, R2,C2):- R2 is R1 + 1, C2 is C1.
adjacent_cell(R1,C1, R2,C2):- R2 is R1 - 1, C2 is C1.
adjacent_cell(R1,C1, R2,C2):- R2 is R1, C2 is C1 + 1.
adjacent_cell(R1,C1, R2,C2):- R2 is R1, C2 is C1 - 1.
