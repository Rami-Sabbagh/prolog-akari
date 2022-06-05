:- module(dataset, [
    assert_nth0_puzzle/1, % assert_nth0_puzzle(Index)
    assert_nth1_puzzle/1, % assert_nth1_puzzle(Index)
    assert_indexed_puzzle/4, %assert_indexed_puzzle(Size, Volume, Book, No)
    puzzle_exists/4, % puzzle_exists(Size, Volume, Book, No)

    wall/2,
    wall_num/3,
    light/2, % use retractall(light) to unload the predefined solution

    title/1,
    size/2
]).

:- dynamic(wall/2),dynamic(wall_num/3),dynamic(light/2),dynamic(title/1),dynamic(size/2).

% ------------------------------------- %
% BELOW THIS RESIDES THE IMPLEMENTATION %
% ------------------------------------- %

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

assert_puzzle_at(Handle, Pos):-
    read_table_fields(Handle, Pos, _, [
        title(Title),
        width(Width),
        height(Height),
        puzzle(Puzzle),
        solution(Solution)
    ]),
    assertz(title(Title)),
    assert_puzzle(Puzzle, Width, Height),
    assert_solution(Solution, Width, Height).

% --------------------

assert_nth1_puzzle(Index):-
    ActualIndex is Index - 1,
    assert_nth0_puzzle(ActualIndex).

assert_nth0_puzzle(Index):-
    dataset(Handle),
    assert_nth0_puzzle(Index, Handle, 0),!.

assert_nth0_puzzle(0, Handle, Offset):-
    assert_puzzle_at(Handle, Offset).

assert_nth0_puzzle(Index, Handle, Offset):-
    read_table_record_data(Handle, Offset, Next, _),
    NextIndex is Index - 1,
    assert_nth0_puzzle(NextIndex, Handle, Next).

% --------------------

puzzle_exists(Size, Volume, Book, No):-
    dataset(Handle),
    in_table(Handle, [
        volume(Volume),
        book(Book),
        level(No),
        width(Size),
        height(Size)
    ], _).

assert_indexed_puzzle(Size, Volume, Book, No):-
    dataset(Handle),
    in_table(Handle, [
        volume(Volume),
        book(Book),
        level(No),
        width(Size),
        height(Size)
    ], Pos),
    assert_puzzle_at(Handle, Pos).
