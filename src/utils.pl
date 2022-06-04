:- ensure_loaded(board).
loop1_print_grid(0,_):-!.
loop1_print_grid(Row,Column):-
	Row1 is Row-1,
	loop1_print_grid(Row1,Column),
	loop2_print_grid(Row,Column),
	nl.
	
loop2_print_grid(_,0):-!.
loop2_print_grid(Row,Column):-
	Column1 is Column-1,
	loop2_print_grid(Row,Column1),
	print_cell(Row,Column).

print_grid :-
	size(Xmax,Ymax),
	loop1_print_grid(Xmax,Ymax).

print_cell(X,Y):-
	wall_num(X,Y,Z)->format('~w',[Z]);(
		wall(X,Y)->write('W');(
			light(X,Y)->write('L');write('*')
		)
	).

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
count_valid_adjacent_cells(cell(X,Y), Cnt) :-
	adjacent_cells(cell(X,Y), Cells),
	length(Cells,L),
	nth1(1, Cells, cell(X1,Y1)),
	(wall(X1, Y1) -> Cnt1 = 0; Cnt1 = 1),
	nth1(2, Cells, cell(X2,Y2)),
	(wall(X2, Y2) -> Cnt2 = 0; Cnt2 = 1),
	(L>2->
		(nth1(3, Cells, cell(X3,Y3)),
		(wall(X3, Y3) -> Cnt3 = 0; Cnt3 = 1));
		Cnt3 = 0),
	(L>3->
		(nth1(4, Cells, cell(X4,Y4)),
		(wall(X4, Y4) -> Cnt4 = 0; Cnt4 = 1));
		Cnt4 = 0),
	Cnt is Cnt1 + Cnt2 + Cnt3 + Cnt4.



% Get all the wall_num cell
wall_num_cells(Cells) :-
	findall((X,Y), wall_num(X, Y, _), Cells).
	% set_light(Cells).

% set the light of the cells
