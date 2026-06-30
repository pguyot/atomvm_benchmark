# atomvm_benchmark

This project generates binaries to run speed tests for AtomVM. These tests are compatible with Erlang/OTP.

The following 7 tests are implemented:
- `pingpong_speed_test`: two processes are sending each other a lot of messages
- `prime_speed_test`: four processes compute prime numbers in parallel
- `prng_test`: generation of random numbers using a simple linear congruential generator
- `pi_test`: computation of an approximation of pi with the Leibniz formula
- `estone_test`: the EStone benchmark adapted from the Erlang/OTP test suite (`erts/emulator/test/estone_SUITE.erl`), running a series of micro benchmarks (list manipulation, message passing, pattern matching, ets, binary handling, arithmetic, function calls, timers, links, ...); the `port_io` micro of the original benchmark is omitted as it requires an external port program
- `sudoku_solution_test`: generation of a full Sudoku grid using a PRNG
- `sudoku_puzzle_test`: generation of a Sudoku puzzle with a unique solution, starting from a solution grid and removing hints randomly until the puzzle has more than one solution

The first four tests are known to work with AtomVM 0.5.0.
The `estone_test` micros that rely on functions unavailable on the running machine (e.g. an older AtomVM) are gracefully skipped.
The two sudoku tests require new functions not yet available.

On ESP32, the watchdog should better be disabled when the tests are running.

If the emulator supports SMP, the two first tests (which are highly dependent on SMP) are additionally run with schedulers online set to 1.

The project builds the following binaries:
- `benchmark` escript compiled with Erlang/OTP 21, suitable for desktop
- `benchmark` escript compiled with Erlang/OTP 26, suitable for desktop
- `benchmark.avm` package compiled with Erlang/OTP 21, suitable for AtomVM 0.5.0
- `benchmark.avm` package compiled with Erlang/OTP 26, requiring AtomVM 0.6.0 alpha
