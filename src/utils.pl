:- encoding(utf8).
% :- use_module(library(theme/dark)).

:- use_module(board).
:- use_module(board_utils).
:- use_module(dataset).

:- include(io_utils).

% Check double agents
% (check if an empty cell is lighted by 2 lights or more).
no_double_light:-
	size(Columns, Rows),
	forall((between(1, Rows, R),between(1, Columns, C)),check_double(R,C)).

check_double(X,Y):- wall(X,Y);light(X,Y).

check_double(X,Y):-
	(column_cells_until_wall(cell(X,Y), Column),
	row_cells_until_wall(cell(X,Y), Row),
	count_lights(Column, Cnt1),
	count_lights(Row, Cnt2),
	!,
	Cnt1 =< 1,
	Cnt2 =< 1).

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
	((wall(X1, Y1);is_lighted(cell(X1,Y1));light(X1,Y1);restricted(X1,Y1)) -> Cells1 = Cells0; append(Cells0, [cell(X1, Y1)], Cells1)),
	nth1(2, Cells, cell(X2,Y2)),
	((wall(X2, Y2);is_lighted(cell(X2,Y2));light(X2,Y2);restricted(X2,Y2)) -> Cells2 = Cells1; append(Cells1, [cell(X2, Y2)], Cells2)),
	(L>2->
		(nth1(3, Cells, cell(X3,Y3)),
		((wall(X3, Y3);is_lighted(cell(X3,Y3));light(X3,Y3);restricted(X3,Y3)) -> Cells3 = Cells2; append(Cells2, [cell(X3, Y3)], Cells3)));
		Cells3 = Cells2),
	(L>3->
		(nth1(4, Cells, cell(X4,Y4)),
		((wall(X4, Y4);is_lighted(cell(X4,Y4));light(X4,Y4);restricted(X4,Y4)) -> Cells4 = Cells3; append(Cells3, [cell(X4, Y4)], Cells4)));
		Cells4 = Cells3).

% Solve the grid
solve :-
	iterate_solve(5),
	light_rest.

iterate_solve(0).
iterate_solve(Cnt):-
	Cnt > 0,
	satisfy_wall_nums(5),
	block_satisfied_wall_nums,
	Cnt1 is Cnt-1,
	iterate_solve(Cnt1).

light_rest:-
	size(Columns, Rows),
	forall((between(1, Rows, R),between(1, Columns, C)),check_cell(R,C)).

check_cell(X,Y):-
	(is_lighted(cell(X,Y));wall(X,Y);restricted(X,Y))->true;assert(light(X,Y)).

satisfy_wall_nums(Cnt) :-
	findall(cell(X,Y),(wall_num(X, Y, _),\+is_wall_num_satisfied(cell(X,Y))), Cells),
	(Cells \= [] , Cnt>0) -> (set_light(Cells),Cnt1 is Cnt -1,satisfy_wall_nums(Cnt1));!.

% Set the light to the satisfied wall num
set_light([]):-!.
set_light([cell(X,Y)|Rest]) :-
	wall_num(X,Y,N),
	adjacent_cells(cell(X,Y),Adjacent_cells),
	valid_adjacent_cells(cell(X,Y),Valid_adjacent_cells),
	length(Valid_adjacent_cells,L),
	count_lights(Adjacent_cells,Cnt),
	N1 is N-Cnt,
	(L = N1 -> (assert_adjacent_light(Valid_adjacent_cells),set_light(Rest));set_light(Rest)).

assert_adjacent_light([]):-!.
assert_adjacent_light([cell(X,Y)|Rest]):-
	assert(light(X,Y)),
	assert_adjacent_light(Rest).

block_satisfied_wall_nums :-
	findall(cell(X,Y),(wall_num(X, Y, _),is_wall_num_satisfied(cell(X,Y))), Cells),
	(Cells \= []) -> block_light(Cells);!.

block_light([]):-!.
block_light([cell(X,Y)|Rest]) :-
	valid_adjacent_cells(cell(X,Y),Valid_adjacent_cells),
	assert_adjacent_restricted(Valid_adjacent_cells),
	block_light(Rest).

assert_adjacent_restricted([]):-!.
assert_adjacent_restricted([cell(X,Y)|Rest]):-
	assert(restricted(X,Y)),
	assert_adjacent_restricted(Rest).

% Validating the solution

solved :-
	no_double_light,
	size(Columns, Rows),
	forall((between(1, Rows, R),between(1, Columns, C)),cell_solved(R,C)).

cell_solved(X,Y):-
	wall(X,Y),!;
	light(X,Y),!;
	wall_num(X,Y,_),is_wall_num_satisfied(cell(X,Y));
	is_lighted(cell(X,Y)).
