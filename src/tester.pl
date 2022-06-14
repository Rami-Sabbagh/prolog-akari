% Quickstart:

% ?- [tester].
% ?- run_tests.

:- begin_tests(krazydad).

:- use_module(dataset).
:- use_module(validation).
% :- use_module(solution).
:- use_module(solution_rami).

:- use_module(board_utils).

test_puzzle_untimed:-
    solved,
    unsolve,
    \+ solved,
    solve,
    solved.

% 1 seconds limit
test_puzzle:- call_with_time_limit(1, test_puzzle_untimed).

% -- ENABLE ONE AT A TIME -- %

% :- include('tests_picked'). % handpicked tests
% :- include('tests_partial'). % 60 tests
:- include('tests_full'). % 2,756 tests
% :- include('tests_failed'). % few of the failed tests

:- end_tests(krazydad).