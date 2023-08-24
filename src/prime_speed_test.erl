% Copyright 2022-2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(prime_speed_test).

-export([run/0]).

run() ->
    iteration(10).

iteration(0) ->
    ok;
iteration(N) ->
    Pid1 = spawn(fun calculate_primes/0),
    Pid2 = spawn(fun calculate_primes/0),
    Pid3 = spawn(fun calculate_primes/0),
    Pid4 = spawn(fun calculate_primes/0),
    Pid1 ! {self(), go},
    Pid2 ! {self(), go},
    Pid3 ! {self(), go},
    Pid4 ! {self(), go},
    receive
        {Pid1, ok} -> ok
    end,
    receive
        {Pid2, ok} -> ok
    end,
    receive
        {Pid3, ok} -> ok
    end,
    receive
        {Pid4, ok} -> ok
    end,
    iteration(N - 1).

is_prime(Num) ->
    test_prime(Num, 2).

test_prime(Num, I) when Num == I ->
    true;
test_prime(Num, I) ->
    if
        Num rem I == 0 ->
            false;
        true ->
            test_prime(Num, I + 1)
    end.

calculate_primes() ->
    receive
        {Parent, go} ->
            calculate_list(1, 1000),
            Parent ! {self(), ok}
    end.

calculate_list(First, Last) when First < 2 ->
    calculate_list(First + 1, Last);
calculate_list(First, Last) when First == Last ->
    case is_prime(Last) of
        true ->
            Last;
        false ->
            []
    end;
calculate_list(First, Last) ->
    case is_prime(First) of
        true ->
            [First | calculate_list(First + 1, Last)];
        false ->
            calculate_list(First + 1, Last)
    end.
