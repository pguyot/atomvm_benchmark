% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(sudoku_grid).

-export([random_puzzle/1, random_solution/1, parallel_random_puzzle/3, print/1, get/3, to_list/1]).

-type index() :: {1..9, 1..9}.
-type value() :: 1..9.
-type puzzle_grid() :: #{index() => 0 | value()}.
-type work_grid() :: tuple().
-type random_generator() :: fun((N :: pos_integer()) -> X :: pos_integer()).

-export_type([puzzle_grid/0, index/0, value/0, random_generator/0]).

-spec get
    (X :: 1..9, Y :: 1..9, Grid :: puzzle_grid()) -> 0 | value();
    (X :: 1..9, Y :: 1..9, Grid :: work_grid()) -> [value()].
get(X, Y, Grid) when is_map(Grid) ->
    maps:get({X, Y}, Grid);
get(X, Y, Grid) when is_tuple(Grid) ->
    element((X - 1) * 9 + Y, Grid).

-spec put
    (X :: 1..9, Y :: 1..9, Value :: 0 | value(), Grid :: puzzle_grid()) -> puzzle_grid();
    (X :: 1..9, Y :: 1..9, Value :: [value()], Grid :: work_grid()) -> work_grid().
put(X, Y, Value, Grid) when is_map(Grid) ->
    maps:put({X, Y}, Value, Grid);
put(X, Y, Value, Grid) when is_tuple(Grid) ->
    setelement((X - 1) * 9 + Y, Grid, Value).

-spec to_list
    (Grid :: puzzle_grid()) -> [{index(), 0 | value()}];
    (Grid :: work_grid()) -> [{index(), [value()]}].
to_list(Grid) when is_map(Grid) ->
    maps:to_list(Grid);
to_list(Grid) when is_tuple(Grid) ->
    [{{X, Y}, get(X, Y, Grid)} || X <- lists:seq(1, 9), Y <- lists:seq(1, 9)].

-spec parallel_random_puzzle(random_generator(), pos_integer(), timeout()) -> puzzle_grid().
parallel_random_puzzle(RandomGenerator, MaxHint, Timeout) ->
    Parent = self(),
    Start = erlang:system_time(millisecond),
    SpawnOpts =
        case erlang:system_info(machine) of
            "BEAM" -> [];
            "ATOM" -> [{heap_growth, fibonacci}]
        end,
    Workers = [
        spawn_opt(
            fun() -> parallel_random_puzzle_worker_loop(RandomGenerator, Parent, infinity) end, [
                monitor | SpawnOpts
            ]
        )
     || _ <- lists:seq(1, erlang:system_info(schedulers_online))
    ],
    parallel_random_puzzle_loop(Start, MaxHint, Workers, Timeout, infinity, undefined).

parallel_random_puzzle_worker_loop(RandomGenerator, Parent, BestCandidate) ->
    {Hints, Puzzle} = random_puzzle(RandomGenerator),
    if
        Hints < BestCandidate ->
            Parent ! {self(), Hints, Puzzle},
            parallel_random_puzzle_worker_loop(RandomGenerator, Parent, Hints);
        true ->
            parallel_random_puzzle_worker_loop(RandomGenerator, Parent, BestCandidate)
    end.

parallel_random_puzzle_loop(Start, MaxHint, Workers, Timeout, BestPuzzleHints, BestPuzzleGrid) ->
    Wait =
        case {Timeout, BestPuzzleGrid} of
            {infinity, _} -> infinity;
            {_, undefined} -> infinity;
            _ -> max(0, Timeout + Start - erlang:system_time(millisecond))
        end,
    receive
        {_Worker, Hints, Puzzle} ->
            if
                Hints =< MaxHint ->
                    stop_workers(Workers),
                    Puzzle;
                Hints < BestPuzzleHints ->
                    parallel_random_puzzle_loop(Start, MaxHint, Workers, Timeout, Hints, Puzzle);
                true ->
                    parallel_random_puzzle_loop(
                        Start, MaxHint, Workers, Timeout, BestPuzzleHints, BestPuzzleGrid
                    )
            end
    after Wait ->
        stop_workers(Workers),
        BestPuzzleGrid
    end.

stop_workers([]) ->
    ok;
stop_workers([{Worker, Monitor} | Tail]) ->
    exit(Worker, kill),
    demonitor(Monitor, [flush]),
    flush_solutions(Worker),
    stop_workers(Tail).

flush_solutions(Worker) ->
    receive
        {Worker, _, _} -> flush_solutions(Worker)
    after 0 -> ok
    end.

-spec random_puzzle(random_generator()) -> {pos_integer(), puzzle_grid()}.
random_puzzle(RandomGenerator) ->
    Grid = random_solution(RandomGenerator),
    AllCells = [{X, Y} || X <- lists:seq(1, 9), Y <- lists:seq(1, 9)],
    ShuffledCells = shuffle(RandomGenerator, AllCells),
    remove_values_until_multiple_solutions(Grid, ShuffledCells, []).

-spec shuffle(random_generator(), [any()]) -> [any()].
shuffle(RandomGenerator, List) ->
    [Val || {_, Val} <- lists:sort([{RandomGenerator(16#10000), Val} || Val <- List])].

remove_values_until_multiple_solutions(Solution, [], HintCells) ->
    {length(HintCells), hints_to_puzzle_grid(Solution, HintCells)};
remove_values_until_multiple_solutions(Solution, [Cell | Tail], AccHintCells) ->
    Candidate = remove_cells_and_propagate(Solution, Tail ++ AccHintCells),
    case has_a_unique_solution(Candidate) of
        true ->
            remove_values_until_multiple_solutions(Solution, Tail, AccHintCells);
        false ->
            remove_values_until_multiple_solutions(Solution, Tail, [Cell | AccHintCells])
    end.

-spec empty_work_grid() -> work_grid().
empty_work_grid() ->
    AllValues = lists:seq(1, 9),
    list_to_tuple(lists:duplicate(81, AllValues)).

-spec random_solution(random_generator()) -> work_grid().
random_solution(RandomGenerator) ->
    EmptyGrid = empty_work_grid(),
    fill_random_grid(RandomGenerator, EmptyGrid).

-spec fill_random_grid(random_generator(), work_grid()) -> work_grid().
fill_random_grid(RandomGenerator, EmptyGrid) ->
    RandomValues = random_values(RandomGenerator, 17),
    CandidateGrid = fill_cells(RandomGenerator, EmptyGrid, RandomValues),
    case find_a_solution(CandidateGrid) of
        {value, Solution} -> Solution;
        none -> fill_random_grid(RandomGenerator, EmptyGrid)
    end.

-spec random_values(RandomGenerator :: random_generator(), Count :: pos_integer()) -> [value()].
random_values(RandomGenerator, Count) ->
    RandomValues = [RandomGenerator(9) || _ <- lists:seq(1, Count)],
    % We need at least 8 different values
    case length(lists:usort(RandomValues)) < 8 of
        true -> random_values(RandomGenerator, Count);
        false -> RandomValues
    end.

%% Randomly add values in a grid, ensuring the grid is still valid.
-spec fill_cells(RandomGenerator :: random_generator(), Grid :: work_grid(), [value()]) ->
    work_grid().
fill_cells(_RandomGenerator, Grid, []) ->
    Grid;
fill_cells(RandomGenerator, Grid, [Value | Tail]) ->
    RandomCellX = RandomGenerator(9),
    RandomCellY = RandomGenerator(9),
    case get(RandomCellX, RandomCellY, Grid) of
        [SingleValue] when SingleValue =/= Value ->
            fill_cells(RandomGenerator, Grid, [Value | Tail]);
        _ ->
            NewGrid = set_grid_value_and_propagate(Grid, {RandomCellX, RandomCellY}, Value),
            case NewGrid of
                invalid ->
                    fill_cells(RandomGenerator, Grid, [Value | Tail]);
                _ ->
                    fill_cells(RandomGenerator, NewGrid, Tail)
            end
    end.

-spec set_grid_value_and_propagate(Grid :: work_grid(), Index :: index(), Value :: value()) ->
    work_grid() | invalid.
set_grid_value_and_propagate(Grid0, {X, Y}, Value) ->
    set_grid_values_and_propagate(Grid0, [{{X, Y}, Value}]).

-spec set_grid_values_and_propagate(
    Grid :: work_grid(), Updates :: [{Index :: index(), Value :: value()}]
) -> work_grid() | invalid.
set_grid_values_and_propagate(Grid0, [{{X, Y}, Value} | Tail]) ->
    Result = lists:foldl(
        fun({CellX, CellY}, Acc) ->
            CellValues = get(CellX, CellY, Grid0),
            case Acc of
                invalid ->
                    invalid;
                {AccGrid, AccList} ->
                    if
                        CellX =:= X andalso CellY =:= Y ->
                            {put(X, Y, [Value], AccGrid), AccList};
                        CellX =:= X ->
                            set_grid_value_and_propagate_update_cell(
                                {CellX, CellY}, CellValues, Value, AccGrid, AccList
                            );
                        CellY =:= Y ->
                            set_grid_value_and_propagate_update_cell(
                                {CellX, CellY}, CellValues, Value, AccGrid, AccList
                            );
                        (CellX - 1) div 3 =:= (X - 1) div 3 andalso
                            (CellY - 1) div 3 =:= (Y - 1) div 3 ->
                            set_grid_value_and_propagate_update_cell(
                                {CellX, CellY}, CellValues, Value, AccGrid, AccList
                            );
                        true ->
                            {AccGrid, AccList}
                    end
            end
        % deterministic, as opposed to maps:fold
        end,
        {Grid0, Tail},
        [{IX, IY} || IX <- lists:seq(1, 9), IY <- lists:seq(1, 9)]
    ),
    case Result of
        invalid -> invalid;
        {NewGrid, []} -> NewGrid;
        {NewGrid, NewList} -> set_grid_values_and_propagate(NewGrid, NewList)
    end.

-spec set_grid_value_and_propagate_update_cell(
    index(), [value()], value(), work_grid(), Updates :: [{Index :: index(), Value :: value()}]
) -> {NewGrid :: work_grid(), NewUpdates :: [{Index :: index(), Value :: value()}]}.
set_grid_value_and_propagate_update_cell({CellX, CellY}, CellValues, Value, AccGrid, AccList) ->
    case lists:member(Value, CellValues) of
        true ->
            NewCellValues = lists:delete(Value, CellValues),
            case NewCellValues of
                [] -> invalid;
                [SingleValue] -> {AccGrid, [{{CellX, CellY}, SingleValue} | AccList]};
                _ -> {put(CellX, CellY, NewCellValues, AccGrid), AccList}
            end;
        false ->
            {AccGrid, AccList}
    end.

-spec hints_to_puzzle_grid(Grid :: work_grid(), HintCells :: [index()]) -> puzzle_grid().
hints_to_puzzle_grid(Grid, HintCells) ->
    EmptyGrid = maps:from_list([{{X, Y}, 0} || X <- lists:seq(1, 9), Y <- lists:seq(1, 9)]),
    lists:foldl(
        fun({IndexX, IndexY} = Index, Map) ->
            [Value] = get(IndexX, IndexY, Grid),
            maps:put(Index, Value, Map)
        end,
        EmptyGrid,
        HintCells
    ).

-spec remove_cells_and_propagate(Grid :: work_grid(), HintCells :: [index()]) -> work_grid().
remove_cells_and_propagate(Grid, HintCells) ->
    EmptyGrid = empty_work_grid(),
    lists:foldl(
        fun({IndexX, IndexY} = Index, Map) ->
            [Value] = get(IndexX, IndexY, Grid),
            set_grid_value_and_propagate(Map, Index, Value)
        end,
        EmptyGrid,
        HintCells
    ).

-spec find_a_solution(Grid :: work_grid()) -> {value, work_grid()} | none.
find_a_solution(Grid) ->
    case find_solutions(Grid, 1, []) of
        {value, [Solution]} -> {value, Solution};
        {incomplete, []} -> none
    end.

-spec has_a_unique_solution(Grid :: work_grid()) -> boolean().
has_a_unique_solution(Grid) ->
    case find_solutions(Grid, 2, []) of
        {value, [_Solution1, _Solution2]} -> false;
        {incomplete, [_Solution]} -> true;
        {incomplete, []} -> false
    end.

-spec find_solutions(Grid :: work_grid(), Count :: pos_integer(), AccSolution :: [work_grid()]) ->
    {value, [work_grid()]} | {incomplete, [work_grid()]}.
find_solutions(Grid, Count, AccSolutions) ->
    case get_undecided_cell(Grid) of
        complete when length(AccSolutions) + 1 =:= Count ->
            {value, [Grid | AccSolutions]};
        complete ->
            {incomplete, [Grid | AccSolutions]};
        {incomplete, {X, Y}, Values} ->
            test_solution(Grid, {X, Y}, Values, Count, AccSolutions)
    end.

%% @doc Recursively (depth-first) test values for a given cell.
-spec test_solution(
    Grid :: work_grid(),
    Cell :: index(),
    Values :: [value()],
    Count :: pos_integer(),
    AccSolutions :: [work_grid()]
) -> {value, [work_grid()]} | {incomplete, [work_grid()]}.
test_solution(_Grid, {_X, _Y}, [], _Count, AccSolutions) ->
    {incomplete, AccSolutions};
test_solution(Grid, {X, Y}, [Value | Tail], Count, AccSolutions) ->
    Grid1 = set_grid_value_and_propagate(Grid, {X, Y}, Value),
    case Grid1 of
        invalid ->
            test_solution(Grid, {X, Y}, Tail, Count, AccSolutions);
        _ ->
            case find_solutions(Grid1, Count, AccSolutions) of
                {value, AllSolutions} ->
                    {value, AllSolutions};
                {incomplete, NewAccSolutions} ->
                    test_solution(Grid, {X, Y}, Tail, Count, NewAccSolutions)
            end
    end.

%% @doc Pick up a cell which is still undecided
-spec get_undecided_cell(Grid :: work_grid()) -> complete | {incomplete, index(), [value()]}.
get_undecided_cell(Grid) ->
    lists:foldl(
        fun({X, Y}, Acc) ->
            case {get(X, Y, Grid), Acc} of
                {[_V1, _V2 | _] = List, complete} ->
                    {incomplete, {X, Y}, List};
                {[_V1, _V2 | _] = List, {incomplete, {_OtherX, _OtherY}, OtherList}} when
                    length(List) < length(OtherList)
                ->
                    {incomplete, {X, Y}, List};
                {_, Acc} ->
                    Acc
            end
        % deterministic, as opposed to maps:fold
        end,
        complete,
        [{X, Y} || X <- lists:seq(1, 9), Y <- lists:seq(1, 9)]
    ).

-spec print(puzzle_grid() | work_grid()) -> ok.
print(Grid) ->
    lists:foreach(
        fun(X) ->
            lists:foreach(
                fun(Y) ->
                    Val = get(X, Y, Grid),
                    case Val of
                        0 -> io:format(" ");
                        [SingleVal] when is_integer(SingleVal) -> io:format("~B", [SingleVal]);
                        Val when is_integer(Val) -> io:format("~B", [Val]);
                        [] -> io:format("X");
                        _ when is_list(Val) -> io:format(".")
                    end
                end,
                lists:seq(1, 9)
            ),
            io:format("\n")
        end,
        lists:seq(1, 9)
    ).
