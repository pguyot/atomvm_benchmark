% Copyright 2022-2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(pingpong_speed_test).

-export([run/0]).

run() ->
    iteration(10).

iteration(0) ->
    ok;
iteration(N) ->
    Pid1 = spawn(fun ping_pong/0),
    Pid2 = spawn(fun ping_pong/0),
    Pid1 ! {peer, Pid2},
    Pid2 ! {peer, Pid1},
    Pid1 ! {self(), start},
    receive
        {Pid1, done} -> ok
    end,
    iteration(N - 1).

ping_pong() ->
    receive
        {peer, Peer} ->
            ping_pong_loop(Peer, undefined, 0)
    end.

ping_pong_loop(Peer, Parent, N) when is_pid(Parent) andalso N > 10000 ->
    Peer ! {self(), done},
    Parent ! {self(), done};
ping_pong_loop(Peer, Parent, N) ->
    receive
        {NewParent, start} ->
            Peer ! {self(), ping},
            ping_pong_loop(Peer, NewParent, N + 1);
        {Peer, ping} ->
            Peer ! {self(), pong},
            ping_pong_loop(Peer, Parent, N);
        {Peer, pong} ->
            Peer ! {self(), ping},
            ping_pong_loop(Peer, Parent, N + 1);
        {_, done} ->
            ok
    end.
