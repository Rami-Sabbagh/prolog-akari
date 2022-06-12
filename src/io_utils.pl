:- encoding(utf8).

:- module(io_utils,[
	choose_nth1_puzzle/1,
	print_grid/0
]).

:- use_module(utils).
:- use_module(board).
:- use_module(board_utils).
:- use_module(dataset).

choose_nth1_puzzle(Index):-
	reset_level,
	assert_nth1_puzzle(Index),
	clear_grid.

print_grid:-
	forall(in_board(R,C),print_cell(R,C)).

print_cell(R,C):-
	(C =:= 1 -> nl; true),
	wall_num(R,C,Z),ansi_format([bg(white),fg(black)],Z,[]);
	wall(R,C),ansi_format([bg(white),fg(white)],'#',[]);
	light(R,C),ansi_format([fg(yellow),bold],'*',[]);
	lighted(R,C),ansi_format([fg(black)],'•',[]);
	restricted(R,C),ansi_format([fg(magenta)],'•',[]);
	ansi_format([fg(cyan)],'•',[]).

clear_grid:-
	unsolve,
	print_grid.
