% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(binary_test).

-export([run/0]).

%% Binary workload: build a binary with the bit syntax, parse it back with
%% binary pattern matching, and exercise split/part style operations,
%% asserting deterministic checksums on each round.

run() ->
    loop(200).

loop(0) ->
    ok;
loop(N) ->
    B = build(100, <<>>),
    400 = byte_size(B),
    5050 = parse(B, 0),
    <<_:50/binary, Rest/binary>> = B,
    350 = byte_size(Rest),
    2472378 = hash(B, 0),
    loop(N - 1).

build(0, Acc) -> Acc;
build(N, Acc) -> build(N - 1, <<Acc/binary, N:32>>).

parse(<<>>, Acc) -> Acc;
parse(<<X:32, Rest/binary>>, Acc) -> parse(Rest, Acc + X).

%% Byte-wise fold to exercise per-byte binary matching.
hash(<<>>, Acc) -> Acc;
hash(<<X:8, Rest/binary>>, Acc) -> hash(Rest, (Acc * 31 + X) band 16#FFFFFF).
