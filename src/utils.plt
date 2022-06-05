:- begin_tests(utils).
:- use_module(dataset).

test_puzzle:-
    size(Dimension, Dimension).

% -- ENABLE ONE AT A TIME -- %

% :- include('tests_picked'). % handpicked tests
% :- include('tests_partial'). % 30 tests
% :- include('tests_full'). % 2,756 tests

:- end_tests(utils).