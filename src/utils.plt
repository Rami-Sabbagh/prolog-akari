% Quickstart:

% ?- [utils].
% ?- load_test_files([]).
% ?- run_tests.

:- begin_tests(utils).
:- use_module(dataset).

:- include(utils).

test_puzzle:-
    size(Dimension, Dimension).

% -- ENABLE ONE AT A TIME -- %

:- include('tests_picked'). % handpicked tests
% :- include('tests_partial'). % 60 tests
% :- include('tests_full'). % 2,756 tests

:- end_tests(utils).