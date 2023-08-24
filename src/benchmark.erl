% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(benchmark).

-export([start/0, main/1]).

% Entry point for escriptize
main(_) ->
    start().

start() ->
    io:format("Running tests:\n"),
    TimeFunc = get_time_func(),
    run(TimeFunc, pingpong_speed_test),
    run(TimeFunc, prime_speed_test),
    run(TimeFunc, prng_test),
    case erlang:function_exported(lists, usort, 1) of
        true ->
            run(TimeFunc, sudoku_solution_test),
            run(TimeFunc, sudoku_puzzle_test);
        false ->
            io:format("Cannot run sudoku tests as this version of AtomVM is missing functions\n")
    end,
    try erlang:system_info(schedulers) of
        N when is_integer(N) andalso N > 1 ->
            Schedulers = erlang:system_flag(schedulers_online, 1),
            run(TimeFunc, pingpong_speed_test, " [schedulers=1]"),
            run(TimeFunc, prime_speed_test, " [schedulers=1]"),
            1 = erlang:system_flag(schedulers_online, Schedulers),
            ok;
        _ ->
            ok
    catch
        _:_ -> ok
    end,
    ok.

run(TimeFunc, TestModule) ->
    run(TimeFunc, TestModule, []).

run(TimeFunc, TestModule, Suffix) ->
    io:format("~s~s: ", [TestModule, Suffix]),
    Start = TimeFunc(),
    TestModule:run(),
    End = TimeFunc(),
    Delta = End - Start,
    io:format("~p\n", [Delta]).

% Old versions of AtomVM didn't have microsecond
get_time_func() ->
    try
        get_time_microsecond(),
        fun get_time_microsecond/0
    catch
        error:badarg ->
            io:format("Warning: microsecond is not available, will use millisecond\n"),
            fun get_time_millisecond/0
    end.

get_time_microsecond() ->
    erlang:system_time(microsecond).

get_time_millisecond() ->
    erlang:system_time(millisecond) * 1000.
