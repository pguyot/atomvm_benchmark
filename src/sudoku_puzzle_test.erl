% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(sudoku_puzzle_test).

-export([run/0]).

run() ->
    SpawnOpts =
        case erlang:system_info(machine) of
            "BEAM" -> [];
            "ATOM" -> [{heap_growth, fibonacci}]
        end,
    {Pid, Ref} = spawn_opt(
        fun() ->
            {Hints, Puzzle} = sudoku_grid:random_puzzle(fun prng_test:lcg/1),
            Hints = 24,
            0 = sudoku_grid:get(1, 1, Puzzle),
            4 = sudoku_grid:get(1, 2, Puzzle)
        end,
        [link, monitor | SpawnOpts]
    ),
    receive
        {'DOWN', Ref, process, Pid, normal} -> ok
    end,
    ok.
