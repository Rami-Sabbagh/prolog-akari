:- module(solution, [
    solve/0
]).

:- use_module(board).
:- use_module(utils).
:- use_module(validation).

solve:-
    restrict,
    light_all_trivials,!,
    light_with_backtrack,
    light_count_correct,
    seal_satisfied_cells, % seal them
    % nl,print_grid,nl,trace,
    light_unlighted,
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

light_all_trivials:-
    light_a_trivial_cell,light_all_trivials;true.

light_a_trivial_cell:-
    wall_num(R,C,N),
    adjacent_cells(R,C, Adj),
    count_lights(Adj, LightsCount),
    valid_light_placement(Adj, ValidCells),
    NeededLights is N - LightsCount,
    NeededLights > 0,
    length(ValidCells, NeededLights),
    forall(member([RL,CL], ValidCells), create_light(RL,CL)).

% --- --- --- --- --- %

light_with_backtrack:-
    light_all_trivials,
    seal_satisfied_cells,
    light_isolated_restricted,
    
    wall_num(R,C,N),

    adjacent_cells(R,C, Adj),
    count_lights(Adj, LightsCount),
    valid_light_placement(Adj, ValidCells),
    NeededLights is N - LightsCount,
    NeededLights =\= 0,

    member([RL,CL], ValidCells),
    create_light(RL,CL),

    light_with_backtrack;
    true.

% --- --- --- --- --- %

seal_satisfied_cells:-
    wall_num(R,C,N),
    length(Cells,N),
    findall([RA,CA],(adjacent_cell(R,C,RA,CA),light(RA,CA)),Cells),
    adjacent_cell(R,C,RA,CA),
    in_board(RA,CA),
    \+ (light(RA,CA);wall(RA,CA);restricted(RA,CA)),
    mark_restricted(RA,CA),
    seal_satisfied_cells
    ;true.

% restrict_around_numbered_cells:-
%     wall_num(R,C,_),
%     adjacent_cell(R,C, RA,CA),
%     valid(RA, CA),
%     mark_restricted(RA,CA),
%     restrict_around_numbered_cells
%     ;true.

% --- --- --- --- --- %

light_unlighted:-
    light_isolated_restricted,
    valid(R,C), create_light(R,C),
    light_unlighted
    ;true.

light_isolated_restricted:-
    restricted(R,C),
    \+ lighted(R,C),
    findall([RA,CA],(
        reachable(R,C,RA,CA),
        valid(RA,CA)
    ),Cells),
    sort(Cells,[[RL,CL]]),
    create_light(RL, CL),
    light_isolated_restricted
    ;true.

