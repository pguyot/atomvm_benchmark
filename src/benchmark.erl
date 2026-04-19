% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(benchmark).

-export([start/0, main/1, exports/0]).

exports() ->
    code_server:module_info().

% Entry point for escriptize
main(_) ->
    start().

start() ->
    case erlang:system_info(machine) of
        "ATOM" ->
            Platform = atomvm:platform(),
            io:format("Platform: ~p\n", [Platform]),
            case Platform of
                esp32 ->
                    io:format("Chip info: ~p\n", [erlang:system_info(esp32_chip_info)]);
                _ ->
                    ok
            end,
            try Platform of
                esp32 ->
                    try esp:task_wdt_reconfigure({5000, 0, false}) of
                        ok ->
                            io:format("Reconfigured esp32 watchdog timer\n");
                        {error, noproc} ->
                            io:format("ESP32 watchdog timer is not running\n")
                    catch
                        error:undef ->
                            io:format(
                                "ESP32 watchdog timer is not enabled or watchdog timer support is not available with this AtomVM version\n"
                            )
                    end;
                _Other ->
                    ok
            catch
                error:undef -> ok
            end;
        "BEAM" ->
            io:format("Machine: BEAM\n")
    end,
    print_jit_status(),
    io:format("Running tests:\n"),
    TimeFunc = get_time_func(),
    run(TimeFunc, pingpong_speed_test),
    run(TimeFunc, prime_speed_test),
    run(TimeFunc, prng_test),
    run(TimeFunc, pi_test),
    run(TimeFunc, bigint_test),
    case erlang:function_exported(lists, usort, 1) of
        true ->
            run(TimeFunc, sudoku_solution_test),
            run(TimeFunc, sudoku_puzzle_test);
        false ->
            io:format("Cannot run sudoku tests as this version of AtomVM is missing functions\n")
    end,
    case erlang:function_exported(crypto, hash, 2) of
        true ->
            run(TimeFunc, crypto_test),
            run(TimeFunc, crypto_bulk_test);
        false ->
            io:format("Cannot run crypto tests as crypto module is not available\n")
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

print_jit_status() ->
    try erlang:system_info(emu_flavor) of
        jit -> io:format("JIT: enabled\n");
        emu -> io:format("JIT: disabled\n");
        _ -> ok
    catch
        _:_ -> ok
    end.

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
