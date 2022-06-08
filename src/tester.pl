% Quickstart:

% ?- [tester].
% ?- run_tests.

:- begin_tests(utils).

:- use_module(dataset).
:- use_module(solution).
:- use_module(validation).

:- use_module(board_utils).

test_puzzle:-
    solved,
    unsolve,
    \+ solved,
    solve,
    solved.

% test_puzzle:-
%     nl,print_grid,nl,nl,
%     solved.

% -- ENABLE ONE AT A TIME -- %

% :- include('tests_picked'). % handpicked tests
% :- include('tests_partial'). % 60 tests
:- include('tests_full'). % 2,756 tests
% :- include('tests_failed'). % few of the failed tests

:- end_tests(utils).