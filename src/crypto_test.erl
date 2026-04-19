% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(crypto_test).
-export([run/0]).

% Latency test: chains 1000 sha256 hashes of 32-byte data.
% Platforms with hardware SHA256 acceleration (e.g. ESP32) will show
% significantly lower times here than software-only platforms (e.g. RP2040).
run() ->
    Seed = crypto:hash(sha256, <<"atomvm_benchmark">>),
    hash_chain(Seed, 1000).

hash_chain(Data, 0) ->
    Data;
hash_chain(Data, N) ->
    hash_chain(crypto:hash(sha256, Data), N - 1).
