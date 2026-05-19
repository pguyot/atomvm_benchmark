% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(bigint_test).

-export([run/0]).

% AtomVM caps integers at 256 bits (signed magnitude). To keep Base*Base below
% that limit, the modulus is the 127-bit Mersenne prime 2^127 - 1; intermediate
% squarings stay under 254 bits.

% Mersenne prime: 2^127 - 1
-define(P127, 16#7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).

run() ->
    iteration(40).

iteration(0) ->
    ok;
iteration(N) ->
    P = ?P127,
    % Fermat's little theorem: g^(p-1) mod p = 1 for g coprime to p
    1 = pow_mod(7, P - 1, P),
    1 = pow_mod(13, P - 1, P),
    % 2^128 mod (2^127 - 1) = 2
    2 = pow_mod(2, 128, P),
    10810968933129975378600013865352026249 = pow_mod(3, 200, P),
    iteration(N - 1).

pow_mod(_Base, 0, _Mod) ->
    1;
pow_mod(Base, Exp, Mod) ->
    B = Base rem Mod,
    pow_mod_iter(B, Exp, Mod, 1).

pow_mod_iter(_Base, 0, _Mod, Acc) ->
    Acc;
pow_mod_iter(Base, Exp, Mod, Acc) ->
    NewAcc =
        case Exp band 1 of
            1 -> (Acc * Base) rem Mod;
            0 -> Acc
        end,
    pow_mod_iter((Base * Base) rem Mod, Exp bsr 1, Mod, NewAcc).
