%% https://github.com/atomvm/AtomVM/pull/776#issuecomment-1714363197
-module(pi_test).
-export([run/0]).

run() ->
    pi(4, 3, -1, 1000000).

pi(Value, _N, _Sign, 0) ->
    Value;
pi(Value, N, Sign, Iterations) ->
    pi(Value + Sign * (4 / N), N + 2, -Sign, Iterations - 1).
