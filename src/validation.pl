% :- module(validation, [

% ]).

:- use_module(board).
:- use_module(board_utils).
:- use_module(dataset).

:- reset_level,assert_nth0_puzzle(0).

% solved:-
%     all_cells_lighted,
%     no_double_light,
%     light_count_correct.

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


%---------------------%
% light_count_correct %
%---------------------%

%-----------%
% Utilities %
%-----------%

between_unorded(A, B, V):- between(A, B, V);between(B, A, V).

% unreachable(R1,C1, R2,C2):-
%     size(Rows, Columns),

%     (R1 = R2; C1 = C2),

%     between(1, Rows, R1),
%     between(1, Columns, C1),

%     between(1, Rows, R2),
%     between(1, Columns, C2),

%     between_unorded(R1, R2, R),
%     between_unorded(C1, C2, C),
%     wall(R, C).

reachable(R1,C1, R2,C2):-
    size(Rows, Columns),

    (R1 = R2; C1 = C2),

    between(1, Rows, R1),
    between(1, Columns, C1),

    between(1, Rows, R2),
    between(1, Columns, C2),

    between_unorded(R1, R2, R),
    between_unorded(C1, C2, C),
    \+ wall(R, C).


