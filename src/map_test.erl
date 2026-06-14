% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(map_test).

-export([run/0]).

%% Map churn workload: build a map key by key, update half the keys,
%% look up every key and fold over the result, asserting deterministic
%% checksums on each round.

run() ->
    loop(100).

loop(0) ->
    ok;
loop(N) ->
    M = build(100, #{}),
    100 = maps:size(M),
    M2 = update(50, M),
    5050 = maps:fold(fun(_K, V, Acc) -> Acc + V end, 0, M),
    % keys 1..50 doubled: 5050 + (1 + .. + 50) = 5050 + 1275
    6325 = maps:fold(fun(_K, V, Acc) -> Acc + V end, 0, M2),
    77 = maps:get(77, M2),
    6325 = lookup_all(100, M2, 0),
    loop(N - 1).

build(0, M) -> M;
build(N, M) -> build(N - 1, maps:put(N, N, M)).

update(0, M) -> M;
update(N, M) -> update(N - 1, maps:put(N, 2 * N, M)).

lookup_all(0, _M, Acc) -> Acc;
lookup_all(N, M, Acc) -> lookup_all(N - 1, M, Acc + maps:get(N, M)).
