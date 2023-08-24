% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(prng_test).

-export([run/0, lcg/1]).

% Deterministic PRNG that yield same values with AtomVM and BEAM
% (it has to be =< 64 bits)
-define(LCG_MULTIPLIER, 1103515245).
-define(LCG_INCREMENT, 12345).
-define(LCG_MODULUS_MASK, ((1 bsl 31) - 1)).

lcg(Uniform) ->
    PrevValue =
        case get(rand_state) of
            undefined -> 0;
            Val -> Val
        end,
    NextValue = (PrevValue * ?LCG_MULTIPLIER + ?LCG_INCREMENT) band ?LCG_MODULUS_MASK,
    put(rand_state, NextValue),
    RandomBits = NextValue bsr 16,
    % biased
    1 + (RandomBits rem Uniform).

run() ->
    erase(rand_state),
    _ = lists:foldl(
        fun(_Ix, _Acc) ->
            lcg(9)
        end,
        0,
        lists:seq(1, 10000)
    ),
    erase(rand_state).
