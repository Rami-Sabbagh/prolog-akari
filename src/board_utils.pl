:- module(board_utils, [
    unsolve/0, % unsolves the loaded level
    reset_level/0 % unloads the whole level
]).

:- use_module(board).

unsolve:-
    retractall(light(_,_)),
    retractall(really_lighted(_,_)),
    retractall(restricted(_,_)).

reset_level:-
    unsolve,
    retractall(wall(_,_)),
    retractall(wall_num(_,_,_)),
    retractall(title(_)),
    retractall(size(_,_)).
