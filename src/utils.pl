:- ensure_loaded(board).

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

right_cells_until_wall(cell(_, Col), []) :- size(_, Y), Col >= Y.
right_cells_until_wall(cell(Row, Col), Cells) :-
	Col2 is Col+1,
	right_cells_until_wall(cell(Row, Col2), Cells2),
	(wall(Row, Col2) -> Cells = []; Cells = [cell(Row,Col2) | Cells2]).

left_cells_until_wall(cell(_, Col), []) :- Col =< 1.
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

down_cells_until_wall(cell(Row, _), []) :- size(X, _), Row >= X.
down_cells_until_wall(cell(Row, Col), Cells) :-
	Row2 is Row+1,
	down_cells_until_wall(cell(Row2, Col), Cells2),
	(wall(Row2, Col) -> Cells = []; Cells = [cell(Row2,Col) | Cells2]).

up_cells_until_wall(cell(Row, _), []) :- Row =< 1.
up_cells_until_wall(cell(Row, Col), Cells) :-
	Row2 is Row-1,
	up_cells_until_wall(cell(Row2, Col), Cells2),
	(wall(Row2, Col) -> Cells = []; Cells = [cell(Row2,Col) | Cells2]).