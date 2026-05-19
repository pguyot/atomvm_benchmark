% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(crypto_test).

-export([run/0]).

% Exercises crypto:hash/2 (SHA-256) and crypto:crypto_one_time/5 (AES-256-CBC).
% Both inner loops are deterministic and chained so the final values double as
% self-checks on AtomVM vs BEAM.

-define(SHA256_ITERATIONS, 10000).
-define(SHA256_EXPECTED,
    <<82, 229, 228, 9, 207, 11, 252, 118, 235, 27, 13, 44, 75, 164, 54, 106, 253, 126, 193, 14, 54,
        32, 188, 119, 81, 120, 47, 45, 222, 206, 161, 159>>
).

-define(AES_ITERATIONS, 1000).
-define(AES_KEY, <<1:256>>).
-define(AES_IV, <<2:128>>).
-define(AES_EXPECTED,
    <<99, 243, 95, 109, 47, 62, 84, 11, 207, 32, 119, 7, 143, 45, 142, 80>>
).

run() ->
    ?SHA256_EXPECTED = sha256_chain(?SHA256_ITERATIONS, <<0:256>>),
    ?AES_EXPECTED = aes_chain(?AES_ITERATIONS, <<0:128>>),
    ok.

% Repeatedly hash the previous digest (similar to PBKDF iteration).
sha256_chain(0, H) ->
    H;
sha256_chain(N, H) ->
    sha256_chain(N - 1, crypto:hash(sha256, H)).

% Repeatedly AES-256-CBC encrypt the previous ciphertext with no padding.
aes_chain(0, B) ->
    B;
aes_chain(N, B) ->
    aes_chain(N - 1, crypto:crypto_one_time(aes_256_cbc, ?AES_KEY, ?AES_IV, B, true)).
