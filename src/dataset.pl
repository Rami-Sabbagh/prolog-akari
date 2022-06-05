:- module(dataset, [
    dataset/1,
    assert_puzzle/3,
    assert_solution/3
]).

dataset(Handle):-
    new_table('packed-data.csv', [
        title(string),
        volume(integer),
        book(integer),
        level(integer),
        width(integer),
        height(integer),
        work(integer),
        puzzle(string),
        solution(string)
    ], [
        field_separator(0',), 
        encoding(utf8)
    ], Handle).

% --------------------

assert_puzzle(Puzzle, Width, Height):-
    assertz(size(Width, Height)),
    assert_puzzle(Puzzle, Width, Height, 0).
    
assert_puzzle(_, Width, Height, Offset):- Offset >= (Width * Height).
assert_puzzle(Puzzle, Width, Height, Offset):-
    X is Offset mod Width + 1,
    Y is Offset // Width + 1,
    CodeIndex is Offset + 1,
    NextOffset is Offset + 1,
    string_code(CodeIndex, Puzzle, Code),
    assert_puzzle_cell(Code, X, Y),
    assert_puzzle(Puzzle, Width, Height, NextOffset).

assert_puzzle_cell(0'., _, _).
assert_puzzle_cell(0'#, X, Y):- assertz(wall(X, Y)).
assert_puzzle_cell(0'1, X, Y):- assertz(wall_num(X, Y, 1)).
assert_puzzle_cell(0'2, X, Y):- assertz(wall_num(X, Y, 2)).
assert_puzzle_cell(0'3, X, Y):- assertz(wall_num(X, Y, 3)).
assert_puzzle_cell(0'4, X, Y):- assertz(wall_num(X, Y, 4)).

% --------------------

assert_solution(Solution, Width, Height):-
    assert_solution(Solution, Width, Height, 0).
    
assert_solution(_, Width, Height, Offset):- Offset >= (Width * Height).
assert_solution(Solution, Width, Height, Offset):-
    X is Offset mod Width + 1,
    Y is Offset // Width + 1,
    CodeIndex is Offset + 1,
    NextOffset is Offset + 1,
    string_code(CodeIndex, Solution, Code),
    assert_solution_cell(Code, X, Y),
    assert_solution(Solution, Width, Height, NextOffset).

assert_solution_cell(0'0, _, _).
assert_solution_cell(0'1, X, Y):- assertz(light(X, Y)).

% --------------------

load_first_puzzle:-
    dataset(Handle),
    read_table_fields(Handle, 0, _, [
        width(Width),
        height(Height),
        puzzle(Puzzle)
    ]),
    assert_puzzle(Puzzle, Width, Height), !.

% read_table_fields(D, 0, N, [width(W),puzzle(P)]).

