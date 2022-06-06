:- encoding(utf8).
:- use_module(dataset).
:- dynamic not_light/2.

% Testing the solution
test_puzzle :-
	size(Xmax,Ymax),
	loop1_test_puzzle(Xmax,Ymax).

loop1_test_puzzle(0,_):-!.
loop1_test_puzzle(Row,Column):-
	Row1 is Row-1,
	loop1_test_puzzle(Row1,Column),
	loop2_test_puzzle(Row,Column).
	
loop2_test_puzzle(_,0):-!.
loop2_test_puzzle(Row,Column):-
	Column1 is Column-1,
	loop2_test_puzzle(Row,Column1),
	test_cell(Row,Column).

test_cell(X,Y):-
	(wall(X,Y);light(X,Y))->!;(wall_num(X,Y,_)->
		is_wall_num_satisfied(cell(X,Y));is_lighted(cell(X,Y))).

% Choosing the puzzle
choose_nth1_puzzle(Index):-
	assert_nth1_puzzle(Index),
	clear_grid.

print_grid:-
	size(Columns, Rows),
	between(1, Rows, Y),
	between(1, Columns, X),
	(X =:= 1 -> nl; true),
	\+ print_cell(X,Y); true.

print_cell(X,Y):-
	wall_num(X,Y,Z),ansi_format([bg(white),fg(black)],Z,[]);
	wall(X,Y),ansi_format([bg(white),fg(white)],'#',[]);
	light(X,Y),ansi_format([fg(yellow),bold],'*',[]);
	not_light(X,Y),ansi_format([fg(magenta)],'.',[]);
	ansi_format([fg(cyan)],'•',[]).

%get adjacent cells of a given cell
adjacent_cells(cell(Row, Col), Cells4) :-
	Cells0 = [],
	size(X, Y),
	(Row < X -> Row1 is Row+1, append(Cells0, [cell(Row1, Col)], Cells1); Cells1 = Cells0),
	(Row > 1 -> Row2 is Row-1, append(Cells1, [cell(Row2, Col)], Cells2); Cells2 = Cells1),
	(Col < Y -> Col1 is Col+1, append(Cells2, [cell(Row, Col1)], Cells3); Cells3 = Cells2),
	(Col > 1 -> Col2 is Col-1, append(Cells3, [cell(Row, Col2)], Cells4); Cells4 = Cells3).


% Get all cells on the right and left of the given cell,
% until a wall, or the edge of the board is reached.
row_cells_until_wall(cell(Row, Col), Cells) :-
	right_cells_until_wall(cell(Row, Col), Right),
	left_cells_until_wall(cell(Row, Col), Left),
	append(Right, Left, Cells).

right_cells_until_wall(cell(_, Col), []) :- size(_, Y), Col >= Y,!.
right_cells_until_wall(cell(Row, Col), Cells) :-
	Col2 is Col+1,
	right_cells_until_wall(cell(Row, Col2), Cells2),
	(wall(Row, Col2) -> Cells = []; Cells = [cell(Row,Col2) | Cells2]).

left_cells_until_wall(cell(_, Col), []) :- Col =< 1,!.
left_cells_until_wall(cell(Row, Col), Cells) :-
	Col2 is Col-1,
	left_cells_until_wall(cell(Row, Col2), Cells2),
	(wall(Row, Col2) -> Cells = []; Cells = [cell(Row,Col2) | Cells2]).


% Get all cells on the top and bottom of the given cell,
% until a wall, or the edge of the board is reached.
column_cells_until_wall(cell(Row, Col), Cells) :-
	up_cells_until_wall(cell(Row, Col), Up),
	down_cells_until_wall(cell(Row, Col), Down),
	append(Up, Down, Cells).

down_cells_until_wall(cell(Row, _), []) :- size(X, _), Row >= X,!.
down_cells_until_wall(cell(Row, Col), Cells) :-
	Row2 is Row+1,
	down_cells_until_wall(cell(Row2, Col), Cells2),
	(wall(Row2, Col) -> Cells = []; Cells = [cell(Row2,Col) | Cells2]).

up_cells_until_wall(cell(Row, _), []) :- Row =< 1,!.
up_cells_until_wall(cell(Row, Col), Cells) :-
	Row2 is Row-1,
	up_cells_until_wall(cell(Row2, Col), Cells2),
	(wall(Row2, Col) -> Cells = []; Cells = [cell(Row2,Col) | Cells2]).

count_lights([], 0).
count_lights([cell(X,Y)|Cells], Cnt) :-
	light(X, Y) -> count_lights(Cells, Cnt2), Cnt is Cnt2 + 1; count_lights(Cells, Cnt).


is_lighted(cell(X,Y)) :-
	column_cells_until_wall(cell(X,Y), Column),
	row_cells_until_wall(cell(X,Y), Row),
	append(Column, Row, Cells),
	count_lights(Cells, Cnt),
	!,
	Cnt > 0.

% Check if the given wall num has the right light count around it
is_wall_num_satisfied(cell(X,Y)) :-
	adjacent_cells(cell(X,Y), Cells),
	count_lights(Cells, Cnt),
	wall_num(X, Y, Num),
	Cnt =:= Num.

% Check the number of valid adjacent cells of a wall with number
valid_adjacent_cells(cell(X,Y), Cells4) :-
	Cells0 = [],
	adjacent_cells(cell(X,Y), Cells),
	length(Cells,L),
	nth1(1, Cells, cell(X1,Y1)),
	((wall(X1, Y1);is_lighted(cell(X1,Y1))) -> Cells1 = Cells0; append(Cells0, [cell(X1, Y1)], Cells1)),
	nth1(2, Cells, cell(X2,Y2)),
	((wall(X2, Y2);is_lighted(cell(X2,Y2))) -> Cells2 = Cells1; append(Cells1, [cell(X2, Y2)], Cells2)),
	(L>2->
		(nth1(3, Cells, cell(X3,Y3)),
		((wall(X3, Y3);is_lighted(cell(X3,Y3))) -> Cells3 = Cells2; append(Cells2, [cell(X3, Y3)], Cells3)));
		Cells3 = Cells2),
	(L>3->
		(nth1(4, Cells, cell(X4,Y4)),
		((wall(X4, Y4);is_lighted(cell(X4,Y4))) -> Cells4 = Cells3; append(Cells3, [cell(X4, Y4)], Cells4)));
		Cells4 = Cells3).

% Solve the grid
solve :-
	findall(cell(X,Y),(wall_num(X, Y, _),\+is_wall_num_satisfied(cell(X,Y))), Cells),
	Cells \= [] -> (set_light(Cells),solve);(print_grid,!).

% Set the light to the satisfied wall num
set_light([]):-!.
set_light([cell(X,Y)|Rest]) :-
	wall_num(X,Y,N),
	valid_adjacent_cells(cell(X,Y),Valid_adjacent_cells),
	length(Valid_adjacent_cells,L),
	(L = N -> (assert_adjacent_light(Valid_adjacent_cells),set_light(Rest));set_light(Rest)).

assert_adjacent_light([]):-!.
assert_adjacent_light([cell(X,Y)|Rest]):-
	assert(light(X,Y)),
	assert_adjacent_light(Rest).

clear_grid :-
	unsolve,
	print_grid.