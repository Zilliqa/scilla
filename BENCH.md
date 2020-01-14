# Performance regression benchmarks

## Rationale

The performance regression micro-benchmarks have the design goal
of ensuring that any change to Scilla evaluator/typechecker and
their underlying libraries do not add significant performance
penalty. Performance regression testing is an effective way to
reveal such issues in early stages. Usually, when performance
regression issue is spotted at a certain point, multiple commits
might have been merged already. When performance regression is
exposed, developers have to spend extra efforts bisecting which
commit among the recently committed changes causes the problem.
An effective way to combat performance regression is to employ
systematic, continuous performance regression testing. Ideally,
the testing should be carried out as comprehensively and
intensively as possible, i.e., on every source commit basis.
This can eliminate the “last-minute surprise” wherein
performance issues are exposed too late. More importantly, this
would avoid the tedious and lengthy diagnosis process to figure
out which commit is responsible for the observed performance
regression.

In the next sections we describe the usage of the benchmark
framework and it’s design.

## Usage

There two ways to use the benchmark suite:

- Run the `make bench` command that executes the `run_benchmark.sh`
  shell script which implements the comparison workflow.
- Use the benchmark suite executable (`./bin/scilla_bench`)
  directly.

### make bench

The `make bench` has two **optional** parameters:

- `base` – Base branch or commit hash (defaults to `master` branch)
- `compare` – Branch or commit hash to compare with the `base`

Running the `make bench` locally without giving it the `compare`
parameters means that we want to compare `base` with the current
state of the repository.

Note that you should commit your changes or stash them before
running the benchmark suite because it needs to checkout the
`base` commit/branch.

When running on CI the `compare` parameter defaults to the
`TRAVIS_COMMIT** environment variable which points to the commit
that the current build is testing.

**Examples**:

Compare `1b4f594b` with the `master` branch:

```sh
make bench base=master compare=1b4f594b
```

Compare the current state of the repo with the `master` branch:

```sh
make bench
```

Please note that the `compare=your-branch-or-commit` parameter
takes precedence over the `TRAVIS_BRANCH` environment variable.
For example, here

```sh
TRAVIS_COMMIT=1b4f594b make bench compare=your-branch-or-commit
```

the `COMPARE` with will be set to `your-branch-or-commit`, not
to the `1b4f594b`.

There is an option to trace the parameters, environment
variables and OCaml version used in the `run_benchmarks.sh`
shell script. For example, the following command

```sh
BENCH_DEBUG=1 make bench
```

Will print

```sh
DEBUG: ocaml -version = your-ocaml-version
DEBUG: TRAVIS_COMMIT =
DEBUG: BASE = master
DEBUG: COMPARE =
```

### scilla-bench executable

It is possible to specify a few command line options. For
example, the quota allowed per test. This quota may be a number
of runs (e.g. 1000x or 1e6x) or a time span (e.g. 10s or 500ms).
The more data samples the program can collect, the better its
estimates will be. The default is to run each benchmark 30
times, which is usually enough to achieve a decent precision.

Here is list of all possible options:

- `-suite` – Type of the benchmark suite to run (can be
  specified multiple times).
- `-quota` – Quota allowed per test.
- `-matching` – Run only benchmarks matching the given regular
  expression.
- `-list` – List benchmark names without running them.
- `-save` – Whether we want to save benchmark results (default is `true`)
- `-compare` – Compare benchmark results and output the
  difference (default is `true`).
- `-timestamp` – Timestamp of benchmark results to compare with.
  If it is not given then the previous results will be loaded by
  finding the directory named after the latest timestamp. And if
  there is no previous results at all just the current timings
  (and percentage) will be displayed.
- `-threshold` – Time per run delta threshold value (in percentage).
- `-ci` – Exit with non-zero code if any *time per run delta*
  exceeds the threshold.
- `-ipcaddress` – Address for IPC communication with blockchain
  for state access.

## Design

We use the micro-benchmarking library for OCaml named
[core_bench](https://github.com/janestreet/core_bench) under the
hood. It is similar to Haskell’s micro-benchmarking library,
[Criterion](https://hackage.haskell.org/package/criterion), in
that it serves the same overall purpose, but instead employs a
different approach to estimating costs that basically yields
better results.

Currently 2 kinds of benchmarks are supported:

- Benchmarks for standalone closed expressions.
- Contract (and transition) benchmarks – high-level performance
  regression tests that invoke the `scilla-runner` executable.

Contract transition benchmarks can be run in 2 modes:

- **IPC** – Uses external server for state management.
- **Local** – Where full state is provided as an input to the
  `scilla-runner` executable.

Benchmarks are described using the simple JSON format and
organized as follows:

- Standalone (expression) and contract benchmarks are listed in
  the `config.json` file
- Each contract benchmark is kept in a separate directory which
  contains the `bench.json` file that serves as a benchmark
  specification (see the `config.atd` for details)
- Transitions should be placed in the "transitions" directory
  inside the corresponding benchmark directory. See the "ipc"
  benchmark for example.

The benchmark suite executable will continually persist the
results each time the benchmark suite is run. Currently we keep
a history of previous results in the file system. The results
for each run is serialized (as an S-expressions) and saved to a
file with `sexp` extension in the directory named after the
current timestamp. Also there is a parameter named `-timestamp`
which can be used to specify the results we want to compare with
(more on this in the [Usage][next section]). If there is nothing
to compare with (which means that we have no previous results in
the file system), the comparison is skipped and only the current
results are displayed. Otherwise the following columns are
printed: first one containing the previous results, next one
with the current results and deltas with percentage.

## References

* [Original PR](https://github.com/Zilliqa/scilla/pull/673)
