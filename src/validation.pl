% :- module(validation, [

% ]).

:- use_module(board).

% solved:-
%     all_cells_lighted,
%     no_double_light,
%     light_count_correct.

%-------------------%
% all_cells_lighted %
%-------------------%


%-----------------%
% no_double_light %
%-----------------%


%---------------------%
% light_count_correct %
%---------------------%

%

between_unorded(A, B, V):- between(A, B, V);between(B, A, V).

unreachable(R1,C1, R2,C2):-
    size(Rows, Columns),

    (R1 =:= R2; C1 =:= C2),

    between(1, Rows, R1),
    between(1, Columns, C1),

    between(1, Rows, R2),
    between(1, Columns, C2),

    wall(R, C),
    between_unorded(R1, R2, R),
    between_unorded(C1, C2, C).

reachable(R1,C1, R2,C2):-
    \+ unreachable(R1,C1, R2,C2).


