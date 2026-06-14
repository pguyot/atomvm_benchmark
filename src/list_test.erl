% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(list_test).

-export([run/0]).

%% List-heavy workload: build, reverse, sort, keyfind and foldl over
%% medium-sized lists, asserting deterministic results on each round.

run() ->
    loop(100).

loop(0) ->
    ok;
loop(N) ->
    L = make_list(1000, []),
    R = lists:reverse(L),
    %% Sort pseudo-randomly shuffled input so both VMs do a real n log n
    %% sort (a presorted or reversed list lets run-detecting sorts finish
    %% in linear time).
    S = lists:sort(shuffle(L)),
    [1 | _] = S,
    [1000 | _] = R,
    500500 = lists:foldl(fun(X, Acc) -> X + Acc end, 0, S),
    KV = make_kv(100, []),
    {50, 2500} = lists:keyfind(50, 1, KV),
    true = lists:member(999, L),
    false = lists:member(1001, L),
    loop(N - 1).

%% Builds [1, 2, ..., Count].
make_list(0, Acc) -> Acc;
make_list(N, Acc) -> make_list(N - 1, [N | Acc]).

%% Deterministic LCG-keyed decoration shuffle (same sequence on both VMs).
shuffle(L) ->
    {Decorated, _} = lists:foldl(
        fun(X, {Acc, Seed}) ->
            NextSeed = (Seed * 1103515245 + 12345) band 16#7FFFFFFF,
            {[{NextSeed, X} | Acc], NextSeed}
        end,
        {[], 42},
        L
    ),
    [X || {_, X} <- lists:sort(Decorated)].

make_kv(0, Acc) -> Acc;
make_kv(N, Acc) -> make_kv(N - 1, [{N, N * N} | Acc]).
