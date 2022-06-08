choose_nth1_puzzle(Index):-
	reset_level,
	assert_nth1_puzzle(Index),
	clear_grid.

print_grid:-
	size(Columns, Rows),
	forall((between(1, Rows, R),between(1, Columns, C)),print_cell(R,C)).

print_cell(R,C):-
	(C =:= 1 -> nl; true),
	wall_num(R,C,Z),ansi_format([bg(white),fg(black)],Z,[]);
	wall(R,C),ansi_format([bg(white),fg(white)],'#',[]);
	light(R,C),ansi_format([fg(yellow),bold],'*',[]);
	restricted(R,C),ansi_format([fg(magenta)],'•',[]);
	is_lighted(cell(R,C)),ansi_format([fg(black)],'•',[]);
	ansi_format([fg(cyan)],'•',[]).

clear_grid:-
	unsolve,
	print_grid.

