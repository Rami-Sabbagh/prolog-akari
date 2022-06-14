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


should_restrict(R,C):- \+ in_board(R,C).
should_restrict(R,C):- wall(R,C).
should_restrict(R,C):- restricted(R,C).
should_restrict(R,C):- mark_restricted(R,C).

should_light(R,C):- light(R,C).
should_light(R,C):- in_board(R,C), create_light(R,C).

should_be_empty(R,C):- \+ wall(R,C).

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
