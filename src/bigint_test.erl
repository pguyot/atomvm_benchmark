% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(bigint_test).
-export([run/0]).

% Exercises bigint arithmetic via modular exponentiation.
% Uses Mersenne prime 2^127-1; verifies Fermat's little theorem: a^(p-1) ≡ 1 (mod p).
run() ->
    P = 170141183460469231731687303715884105727,
    pow_mod_loop([2, 3, 5, 7, 11, 13, 17, 19, 23, 29], P).

pow_mod_loop([], _P) ->
    ok;
pow_mod_loop([Base | Rest], P) ->
    1 = pow_mod(Base, P - 1, P),
    pow_mod_loop(Rest, P).

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
            1 -> Base * Acc rem Mod;
            0 -> Acc
        end,
    pow_mod_iter(Base * Base rem Mod, Exp bsr 1, Mod, NewAcc).
