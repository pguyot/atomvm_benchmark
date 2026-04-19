% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(crypto_bulk_test).
-export([run/0]).

% Throughput test: hashes a 1 KB buffer 100 times.
% Hardware SHA256 acceleration (e.g. ESP32) yields much better throughput
% on larger blocks than the latency-focused crypto_test.
run() ->
    Data = binary:copy(<<16#A5>>, 1024),
    hash_bulk(Data, 100).

hash_bulk(_Data, 0) ->
    ok;
hash_bulk(Data, N) ->
    crypto:hash(sha256, Data),
    hash_bulk(Data, N - 1).
