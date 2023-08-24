% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(sudoku_solution_test).

-export([run/0]).

run() ->
    SpawnOpts =
        case erlang:system_info(machine) of
            "BEAM" -> [];
            "ATOM" -> [{heap_growth, fibonacci}]
        end,
    {Pid, Ref} = spawn_opt(
        fun() ->
            sudoku_grid:random_solution(fun prng_test:lcg/1)
        end,
        [link, monitor | SpawnOpts]
    ),
    receive
        {'DOWN', Ref, process, Pid, normal} -> ok
    end,
    ok.
