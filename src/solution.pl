:- module(solution, [
    solve/0
]).

:- use_module(board).
:- use_module(utils).

% Get adjacent cells of a given cell
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

% Count lights in the given list of cells
count_lights([], 0).
count_lights([cell(X,Y)|Cells], Cnt) :-
	light(X, Y) -> count_lights(Cells, Cnt2), Cnt is Cnt2 + 1; count_lights(Cells, Cnt).

% Check if the given wall num has the right light count around it
is_wall_num_satisfied(cell(X,Y)) :-
	adjacent_cells(cell(X,Y), Cells),
	count_lights(Cells, Cnt),
	wall_num(X, Y, Num),
	Cnt =:= Num.

% Check the number of valid adjacent cells of a wall with number
valid_adjacent_cells(cell(R,C), Cells) :-
	findall(cell(RA,CA), (
		adjacent_cell(R,C, RA, CA),in_board(RA, CA),
		\+ (wall(RA,CA);lighted(RA,CA);light(RA,CA);restricted(RA,CA))
	), CellsList),
	sort(CellsList, Cells).

% Solve the grid
solve :-
	light_3_diagonal,
	iterate_solve(5),
	light_restricted,
	light_rest.

/* Light the other two nieghbors of a 3 that is diagonal with a 1
e.g.
before:
•••
•3•
••1
after:
•*•
*3•
••1
*/
light_3_diagonal:-
	forall(wall_num(X,Y,3),light_3_diagonal(cell(X,Y))).

light_3_diagonal(cell(R,C)):-
	R1 is R+1, C1 is C+1,
	R2 is R-1, C2 is C-1,
	(wall_num(R1,C1,1) -> (assert(light(R,C2)),assert(light(R2,C)));true),
	(wall_num(R1,C2,1) -> (assert(light(R2,C)),assert(light(R,C1)));true),
	(wall_num(R2,C1,1) -> (assert(light(R,C2)),assert(light(R1,C)));true),
	(wall_num(R2,C2,1) -> (assert(light(R,C1)),assert(light(R1,C)));true).

% light the restricted cells with the first valid
% cell in their row or column
light_restricted:-
	forall((restricted(X,Y),\+lighted(X,Y)),random_light(X,Y)).

random_light(R,C):-
	row_cells_until_wall(cell(R,C), Rows),
	column_cells_until_wall(cell(R,C), Columns),
	append(Rows,Columns,Cells),
	assert_random_light(Cells).

assert_random_light([]):-!.
assert_random_light([cell(X,Y)|Rest]):-
	(restricted(X,Y);lighted(X,Y))->assert_random_light(Rest);assert(light(X,Y)).

% The repeated part of the solution
iterate_solve(0).
iterate_solve(Cnt):-
	Cnt > 0,
	block_numbers,
	satisfy_wall_nums(5),
	block_satisfied_wall_nums,
	light_restricted_isolated,
	light_isolated,
	Cnt1 is Cnt-1,
	iterate_solve(Cnt1).

% Block the corners of the numbers
block_numbers:-
	findall(wall_num(X,Y,N),(wall_num(X,Y,N),\+is_wall_num_satisfied(cell(X,Y))),Walls),
	block_numbers(Walls).

block_numbers([]):-!.
block_numbers([wall_num(X,Y,N)|Rest]):-
	valid_adjacent_cells(cell(X,Y),Valid_adjacent_cells),
	length(Valid_adjacent_cells,L),
	adjacent_cells(cell(X,Y),Adjacent_cells),
	count_lights(Adjacent_cells,Cnt),
	N1 is N-Cnt+1,
	(N1 = L -> block_corners(Valid_adjacent_cells);true),
	block_numbers(Rest).

block_corners([]):-!.
block_corners([cell(X,Y)|Rest]):-
	X1 is X+1, Y1 is Y+1,
	X2 is X-1, Y2 is Y-1,
	((member(cell(X1,Y1),Rest))->
		(assert(restricted(X,Y1)),assert(restricted(X1,Y)));true),
	((member(cell(X1,Y2),Rest))->
		(assert(restricted(X,Y2)),assert(restricted(X1,Y)));true),
	((member(cell(X2,Y1),Rest))->
		(assert(restricted(X,Y1)),assert(restricted(X2,Y)));true),
	((member(cell(X2,Y2),Rest))->
		(assert(restricted(X,Y2)),assert(restricted(X2,Y)));true),
	block_corners(Rest).

% Light isolated cells that can't be lighted unless they are light
light_isolated:-
	forall((in_board(R,C),\+wall(R,C),\+light(R,C),\+lighted(R,C))
		,check_isolated(cell(R,C))).

check_isolated(cell(R,C)):-
	row_cells_until_wall(cell(R,C), Rows),
	column_cells_until_wall(cell(R,C), Columns),
	append(Rows,Columns,Cells),
	count_not_lighted(Cells,Cnt),
	!,
	Cnt = 0 -> assert(light(R,C));true.

% Count not lighted cells in the given list of cells
count_not_lighted([],0):-!.
count_not_lighted([cell(X,Y)|Rest],Cnt):-
	count_not_lighted(Rest,Cnt1),
	((lighted(X,Y);restricted(X,Y)) -> Cnt is Cnt1 ; Cnt is Cnt1 + 1).

% Light restricted cells that have only one not lighted cell in their row and column
light_restricted_isolated:-
	forall((in_board(R,C),restricted(R,C),\+lighted(R,C))
		,check_restricted_isolated(cell(R,C))).

check_restricted_isolated(cell(R,C)):-
	row_cells_until_wall(cell(R,C), Rows),
	column_cells_until_wall(cell(R,C), Columns),
	append(Rows,Columns,Cells),
	count_not_lighted(Cells,Cnt),
	!,
	Cnt = 1 -> forall((member(cell(X,Y),Cells),\+ lighted(X,Y),\+ restricted(X,Y)),assert(light(X,Y)));true.

% Light every not lighted cell in the board
light_rest:-
	forall((in_board(R,C),\+wall(R,C),\+light(R,C),\+lighted(R,C),\+restricted(R,C)),
		assert(light(R,C))).

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

% Block the not lighted cells in the adjacent of a satisfied wall num
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
