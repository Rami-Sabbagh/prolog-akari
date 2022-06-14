:- module(templates, [
    apply_templates/0
]).

:- use_module(board).
:- use_module(utils).

:- use_module(io_utils).

point:- !.
% point:- nl,print_grid,nl,trace.

apply_template([R,C,Template]):-
    template(Template,R,C), point.

apply_templates(Exclude):-
    findall([R,C,Template], (
        matcher(R,C,Template),
        \+ member([R,C,Template], Exclude)
    ), MatchesUnsorted), point,

    sort(MatchesUnsorted, Matches),
    maplist(apply_template, Matches),
    length(Matches, Count),
    append(Matches, Exclude, NewExclude),
    Count =\= 0 -> apply_templates(NewExclude); true.

apply_templates:- apply_templates([]).

% --- --- --- %

should_restrict([R,C]):- should_restrict(R,C).

should_restrict(R,C):- \+ in_board(R,C).
should_restrict(R,C):- wall(R,C).
should_restrict(R,C):- restricted(R,C).
should_restrict(R,C):- mark_restricted(R,C).

should_light(R,C):- light(R,C).
should_light(R,C):- in_board(R,C), create_light(R,C).

should_be_empty(R,C):- \+ (wall(R,C);light(R,C)).

can_be_light(R,C):- light(R,C).
can_be_light(R,C):- valid(R,C).

light_if_possible(R,C):- valid(R,C), create_light(R,C); true.

should_be_invalid(R,C):- \+ in_board(R,C). % (should be invalid)
should_be_invalid(R,C):- wall(R,C).
should_be_invalid(R,C):- lighted(R,C), \+ light(R,C).


% matcher(R,C, Template).

matcher(R,C, restrict_3):-
    wall_num(R,C,3).

% --- --- --- %

matcher(R,C, restrict_2_up):-
    wall_num(R,C,2), R2 is R-1, should_be_invalid(R2,C).

matcher(R,C, restrict_2_down):-
    wall_num(R,C,2), R2 is R+1, should_be_invalid(R2,C).

matcher(R,C, restrict_2_left):-
    wall_num(R,C,2), C2 is C-1, should_be_invalid(R,C2).

matcher(R,C, restrict_2_right):-
    wall_num(R,C,2), C2 is C+1, should_be_invalid(R,C2).

% --- --- --- %

matcher(R,C, restrict_1_up_left):-
    wall_num(R,C,1), R2 is R-1, C2 is C-1,
    should_be_invalid(R2,C), should_be_invalid(R,C2).

matcher(R,C, restrict_1_up_right):-
    wall_num(R,C,1), R2 is R-1, C2 is C+1,
    should_be_invalid(R2,C), should_be_invalid(R,C2).

matcher(R,C, restrict_1_down_left):-
    wall_num(R,C,1), R2 is R+1, C2 is C-1,
    should_be_invalid(R2,C), should_be_invalid(R,C2).

matcher(R,C, restrict_1_down_right):-
    wall_num(R,C,1), R2 is R+1, C2 is C+1,
    should_be_invalid(R2,C), should_be_invalid(R,C2).

% --- --- --- %

matcher(R,C, shared_3_1_normal):-
    wall_num(R,C,3), R2 is R+1, C2 is C+1, wall_num(R2,C2, 1).

matcher(R,C, shared_3_1_flip_h):-
    wall_num(R,C,3), R2 is R+1, C2 is C-1, wall_num(R2,C2, 1).

matcher(R,C, shared_3_1_flip_v):-
    wall_num(R,C,3), R2 is R-1, C2 is C+1, wall_num(R2,C2, 1).

matcher(R,C, shared_3_1_flip_vh):-
    wall_num(R,C,3), R2 is R-1, C2 is C-1, wall_num(R2,C2, 1).

% --- --- --- %

matcher(R,C, shared_2_1_normal):-
    wall_num(R,C,2),
    RO is R+1, CO is C+1, % the other wall num
    RL is R-1, CL is C-1, % possible for light/wall
    wall_num(RO,CO, 1),
    (
        should_be_invalid(RL,C),can_be_light(R,CL);
        should_be_invalid(R,CL),can_be_light(RL,C)
    ).

matcher(R,C, shared_2_1_flip_h):-
    wall_num(R,C,2),
    RO is R+1, CO is C-1, % the other wall num
    RL is R-1, CL is C+1, % possible for light/wall
    wall_num(RO,CO, 1),
    (
        should_be_invalid(RL,C),can_be_light(R,CL);
        should_be_invalid(R,CL),can_be_light(RL,C)
    ).

matcher(R,C, shared_2_1_flip_v):-
    wall_num(R,C,2),
    RO is R-1, CO is C+1, % the other wall num
    RL is R+1, CL is C-1, % possible for light/wall
    wall_num(RO,CO, 1),
    (
        should_be_invalid(RL,C),can_be_light(R,CL);
        should_be_invalid(R,CL),can_be_light(RL,C)
    ).

matcher(R,C, shared_2_1_flip_vh):-
    wall_num(R,C,2),
    RO is R-1, CO is C-1, % the other wall num
    RL is R+1, CL is C+1, % possible for light/wall
    wall_num(RO,CO, 1),
    (
        should_be_invalid(RL,C),can_be_light(R,CL);
        should_be_invalid(R,CL),can_be_light(RL,C)
    ).

% --- --- --- %

matcher(R,C, line_3_h):-
    wall_num(R,CL,3), C is CL + 1, CR is C + 1,
    wall_num(R,CR,3), RP is R - 1, RN is R + 1,
    can_be_light(R,C), should_be_empty(RP,C), should_be_empty(RN,C).

matcher(R,C, line_3_v):-
    wall_num(RP,C,3), R is RP + 1, RN is R + 1,
    wall_num(RN,C,3), CP is C - 1, CN is C + 1,
    can_be_light(R,C), should_be_empty(R,CP), should_be_empty(R,CN).

% --- --- --- %

% template(Template, R,C).

template(restrict_3, R,C):-
    RP is R-1, RN is R+1, CP is C-1, CN is C+1,
    should_restrict(RP,CP), should_restrict(RP,CN),
    should_restrict(RN,CP), should_restrict(RN,CN).

% --- --- --- %

template(restrict_2_up, R,C):-
    RN is R+1, CP is C-1, CN is C+1,
    should_restrict(RN,CP), should_restrict(RN,CN).

template(restrict_2_down, R,C):-
    RP is R-1, CP is C-1, CN is C+1,
    should_restrict(RP,CP), should_restrict(RP,CN).

template(restrict_2_left, R,C):-
    RP is R-1, RN is R+1, CN is C+1,
    should_restrict(RP,CN), should_restrict(RN,CN).

template(restrict_2_right, R,C):-
    RP is R-1, RN is R+1, CP is C-1,
    should_restrict(RP,CP), should_restrict(RN,CP).

% --- --- --- %

template(restrict_1_up_left, R,C):-
    RN is R+1, CN is C+1,
    should_restrict(RN,CN).

template(restrict_1_up_right, R,C):-
    RN is R+1, CP is C-1,
    should_restrict(RN,CP).

template(restrict_1_down_left, R,C):-
    RP is R-1, CN is C+1,
    should_restrict(RP,CN).

template(restrict_1_down_right, R,C):-
    RP is R-1, CP is C-1,
    should_restrict(RP,CP).

% --- --- --- %

template(shared_3_1_normal, R,C):-
    RL is R-1, CL is C-1, R1 is R+1, R2 is R+2, C1 is C+1, C2 is C+2,
    should_light(RL,C), should_light(R,CL),
    should_restrict(R2,C1), should_restrict(R1, C2).

template(shared_3_1_flip_v, R,C):-
    RL is R+1, CL is C-1, R1 is R-1, R2 is R-2, C1 is C+1, C2 is C+2,
    should_light(RL,C), should_light(R,CL),
    should_restrict(R2,C1), should_restrict(R1, C2).

template(shared_3_1_flip_h, R,C):-
    RL is R-1, CL is C+1, R1 is R+1, R2 is R+2, C1 is C-1, C2 is C-2,
    should_light(RL,C), should_light(R,CL),
    should_restrict(R2,C1), should_restrict(R1, C2).

template(shared_3_1_flip_vh, R,C):-
    RL is R+1, CL is C+1, R1 is R-1, R2 is R-2, C1 is C-1, C2 is C-2,
    should_light(RL,C), should_light(R,CL),
    should_restrict(R2,C1), should_restrict(R1, C2).

% --- --- --- %

template(shared_2_1_normal, R,C):-
    RL is R-1, CL is C-1, R1 is R+1, R2 is R+2, C1 is C+1, C2 is C+2,
    light_if_possible(RL,C), light_if_possible(R,CL),
    should_restrict(R2,C1), should_restrict(R1, C2).

template(shared_2_1_flip_v, R,C):-
    RL is R+1, CL is C-1, R1 is R-1, R2 is R-2, C1 is C+1, C2 is C+2,
    light_if_possible(RL,C), light_if_possible(R,CL),
    should_restrict(R2,C1), should_restrict(R1, C2).

template(shared_2_1_flip_h, R,C):-
    RL is R-1, CL is C+1, R1 is R+1, R2 is R+2, C1 is C-1, C2 is C-2,
    light_if_possible(RL,C), light_if_possible(R,CL),
    should_restrict(R2,C1), should_restrict(R1, C2).

template(shared_2_1_flip_vh, R,C):-
    RL is R+1, CL is C+1, R1 is R-1, R2 is R-2, C1 is C-1, C2 is C-2,
    light_if_possible(RL,C), light_if_possible(R,CL),
    should_restrict(R2,C1), should_restrict(R1, C2).

% --- --- --- %

template(line_3_h, R,C):-
    CL1 is C-2, CL2 is C+2,
    should_light(R,C), should_light(R,CL1), should_light(R,CL2),
    RP is R-1, RN is R+1,
    CP is C-1, CN is C+1,
    findall([RT,CT], (
        should_be_empty(RP,CP),reachable(left,  RP,CP, RT,CT);
        should_be_empty(RN,CP),reachable(left,  RN,CP, RT,CT);
        should_be_empty(RP,CN),reachable(right, RP,CN, RT,CT);
        should_be_empty(RN,CN),reachable(right, RN,CN, RT,CT)
    ), SpotsUnsorted),
    sort(SpotsUnsorted, Spots),
    maplist(should_restrict, Spots).

template(line_3_v, R,C):-
    RL1 is R-2, RL2 is R+2,
    should_light(R,C), should_light(RL1,C), should_light(RL2,C),
    RP is R-1, RN is R+1,
    CP is C-1, CN is C+1,
    findall([RT,CT], (
        should_be_empty(RP,CP),reachable(up,   RP,CP, RT,CT);
        should_be_empty(RP,CN),reachable(up,   RP,CN, RT,CT);
        should_be_empty(RN,CP),reachable(down, RN,CP, RT,CT);
        should_be_empty(RN,CN),reachable(down, RN,CN, RT,CT)
    ), SpotsUnsorted),
    sort(SpotsUnsorted, Spots),
    maplist(should_restrict, Spots).

% --- --- --- %
