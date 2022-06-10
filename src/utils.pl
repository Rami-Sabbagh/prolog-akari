:- module(utils, [
    between_unordered/3,
    reachable/4,
    lighted/2
]).

:- use_module(board).

between_unordered(A, B, V):- between(A, B, V);between(B, A, V).

reachable(R1,C1, R2,C2):-
    size(Rows, Columns),
    (R1 = R2; C1 = C2),

    \+ (
        between(1, Rows, R1),
        between(1, Columns, C1),

        between(1, Rows, R2),
        between(1, Columns, C2),
        between_unordered(R1, R2, R),
        between_unordered(C1, C2, C),

        wall(R, C)
    ).

lighted(R, C):- wall(R,C);light(R,C).
lighted(R, C):-
    light(RL, CL),
    reachable(R,C, RL, CL).
