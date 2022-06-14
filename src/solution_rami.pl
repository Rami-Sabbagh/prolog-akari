:- module(solution, [
    solve/0
]).

:- use_module(board).
:- use_module(utils).
:- use_module(validation).

:- use_module(io_utils).

point:- !.
% point:- nl,print_grid,nl,trace.
% point:- nl,print_grid,nl.

solve:-
    restrict,
    light_all_trivials,!, point,
    light_with_backtrack, 
    seal_satisfied_cells,
    \+ dead_restricted, point,
    light_count_correct, point,
    light_unlighted, point,
    solved,
    !.

% --- --- --- --- --- %

restrict:-
    restrict_0_cells.

restrict_0_cells:-
    forall((wall_num(R,C,0), adjacent_cell(R,C, RA,CA)),(
        in_board(RA,CA),
        \+ (wall(RA,CA);restricted(RA,CA)),
        mark_restricted(RA,CA)
    ;true)),!.

% --- --- --- --- --- %

count_lights(Cells, Count):-
    findall(_, (member([RA,CA],Cells),light(RA,CA)), Lights),
    length(Lights,Count).

valid_light_placement(Cells, ValidCells):-
    findall([R,C], (member([R,C], Cells),valid(R,C)), ValidCells).

create_light([RL, CL]):- create_light(RL, CL).

% backtracks correctly
light_all_trivials:-
    (
        wall_num(R,C,N),
        adjacent_cells(R,C, Adj),
        count_lights(Adj, LightsCount),
        valid_light_placement(Adj, ValidCells),
        NeededLights is N - LightsCount,
        NeededLights > 0,
        length(ValidCells, NeededLights)
    ) -> 
    (
        maplist(create_light, ValidCells),
        light_all_trivials
    );
    (
        true
    ).

% --- --- --- --- --- %

% up, right, down, left.
combination(3, n,y,y,y).
combination(3, y,n,y,y).
combination(3, y,y,n,y).
combination(3, y,y,y,n).

combination(2, y,y,n,n).
combination(2, n,y,y,n).
combination(2, n,n,y,y).
combination(2, y,n,n,y).
combination(2, y,n,y,n).
combination(2, n,y,n,y).

combination(1, y,n,n,n).
combination(1, n,y,n,n).
combination(1, n,n,y,n).
combination(1, n,n,n,y).

try_light(R,C):- light(R,C).
try_light(R,C):- valid(R,C),create_light(R,C).

not_light(R,C):- restricted(R,C).
not_light(R,C):- wall(R,C).
not_light(R,C):- \+ light(R,C),mark_restricted(R,C).

try_combination(R,C):-
    wall_num(R,C, N),
    combination(N, Up, Right, Down, Left),

    RP is R-1, RN is R+1, CP is C-1, CN is C+1,
    (Up = y    -> try_light(RP,C); not_light(RP,C)),
    (Down = y  -> try_light(RN,C); not_light(RN,C)),
    (Left = y  -> try_light(R,CP); not_light(R,CP)),
    (Right = y -> try_light(R,CN); not_light(R,CN)).

prioritized_wall_num(R,C,3):- wall_num(R,C,3).
prioritized_wall_num(R,C,1):- wall_num(R,C,1).
prioritized_wall_num(R,C,2):- wall_num(R,C,2).

% backtracks correctly.
light_with_backtrack:-
    light_all_trivials,
    seal_satisfied_cells,
    light_isolated_restricted,
    
    (
        (prioritized_wall_num(R,C,_),\+ satisfied(R,C)) ->
        (
            % point,
            try_combination(R,C),
            % point,
            light_with_backtrack
        );
        (
            true
        )
    ).

% --- --- --- --- --- %

mark_restricted([R,C]):- mark_restricted(R,C).

seal_cell([R,C]):-
    findall([RA,CA], (
        adjacent_cell(R,C, RA,CA),
        in_board(RA,CA),
        \+ (wall(RA,CA);light(RA,CA);restricted(RA,CA))
    ), L),
    maplist(mark_restricted, L).

% backtracks correctly.
seal_satisfied_cells:-
    findall([R,C], satisfied(R,C), L),
    maplist(seal_cell, L).

% --- --- --- --- --- %

dead_restricted:-
    restricted(R,C), \+ lighted(R, C),
    \+ (reachable(R,C,RA,CA), valid(RA,CA)).

light_unlighted:- \+ valid(_,_).
light_unlighted:-
    valid(R,C), create_light(R,C),
    light_isolated_restricted,
    light_unlighted.

% backtracks correctly.
light_isolated_restricted:-
    findall([RL,CL], (
        restricted(R,C),
        \+ lighted(R,C),
        reachable(R,C,RL,CL),
        valid(RL,CL),
        \+ (
            reachable(R,C,R2,C2), valid(R2,C2),
            (R2 \= RL; C2 \= CL)
        )
    ), Lights),
    sort(Lights, LightsSet), % remove duplicates because they are possible here!
    maplist(create_light, LightsSet).
