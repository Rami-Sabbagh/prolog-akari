:- module(validation, [
    solved/0,
    light_count_correct/0
]).

:- use_module(board).
:- use_module(utils).

% complexity: O(N^4) where N is the board dimension.
solved:-
    valid_board,
    all_cells_lighted,
    no_double_light,
    light_count_correct.

%-------------%
% valid_board %
%-------------%

% complexity: O(N^2).
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
    (light(R,C);wall(R,C);wall_num(R,C,_)),
    \+ (in_board(R,C)).

%-------------------%
% all_cells_lighted %
%-------------------%

% complexity: O(N^4).
all_cells_lighted:-
    forall((in_board(R,C),\+wall(R,C),\+light(R,C)), lighted(R,C)).

%-----------------%
% no_double_light %
%-----------------%

% complexity: O(N^2) where N is the lights count.
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

% complexity: O(N) where N is the numbered walls count.
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
