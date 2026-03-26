# Bench Suite

The `bench/` directory keeps the current throughput-focused benchmarks and adds a small support layer for extra observability.

Structured artifacts are written to `bench/results/` by default as both JSON and CSV. Override that location with `PARSEFF_BENCH_RESULTS_DIR=/path/to/results`.

## What is measured now

- `Gc.quick_stat` batch deltas in the JSON, CSV, and arithmetic benchmarks
- `Gc.stat` plus Linux RSS/HWM in `bench_streaming.ml`
- the original `Benchmark.latencyN` throughput tables remain unchanged

## Useful targets

- `make bench`
- `make bench-json`
- `make bench-csv`
- `make bench-arithmetic`
- `make bench-streaming`

Each run writes stable files such as `bench/results/bench_json.json` and `bench/results/bench_json.csv`, so you can diff or archive them across runs.

## Reading the new GC tables

### `Gc.quick_stat`

The `GC Quick Stats (single batch)` table adds:

- `Minor MiB`: short-lived allocation churn in the minor heap
- `Major MiB`: direct major-heap allocation during the batch
- `Promo MiB`: data promoted out of the minor heap
- `minGC` / `majGC`: collection counts during the batch
- `Heap MiB` / `Top MiB`: heap size after the run and heap high-water mark

This is the cheapest extra signal and is good for comparing parsers on the same workload.

### `Gc.stat` + RSS

`bench_streaming.ml` uses a fuller measurement pass:

- `Live MiB`: live heap after a post-run full major GC
- `Heap MiB` / `Top MiB`: current heap and high-water mark
- `RSS MiB`: resident memory after the run, when `/proc/self/status` is available
- `HWM MiB`: resident-set high-water mark reported by Linux

This is most useful for streaming and large-input experiments.

## Streaming benchmark knobs

`bench_streaming.ml` reads a few environment variables:

- `PARSEFF_STREAM_LINES` default `100000`
- `PARSEFF_STREAM_CHUNK` default `65536`
- `PARSEFF_STREAM_RUNS` default `1`
- `PARSEFF_STREAM_WARMUP` default `0`

Examples:

```bash
PARSEFF_STREAM_CHUNK=64 make bench-streaming
PARSEFF_STREAM_LINES=1000000 PARSEFF_STREAM_CHUNK=4096 make bench-streaming
```

## `perf stat`

Use this first when throughput changes and you want to know why.

```bash
opam exec -- dune build bench/bench_json.exe --profile=release
perf stat -r 5 -d -d -- taskset -c 2 _build/default/bench/bench_json.exe
```

Good counters to watch:

- `cycles`
- `instructions`
- `branches`
- `branch-misses`
- `cache-misses`
- `LLC-load-misses`
- `dTLB-load-misses`

Rules of thumb:

- lower cycles with similar instructions usually means a real CPU win
- lower IPC plus more cache misses usually means memory pressure
- more branch misses often means heavier control-flow/backtracking cost

## `perf record`

Use this to find hot functions.

```bash
perf record -g -- taskset -c 2 _build/default/bench/bench_json.exe
perf report --stdio
```

## `perf mem`

Use this when the question is where the loads and stores are coming from in the memory hierarchy.

```bash
sudo perf mem record -- taskset -c 2 _build/default/bench/bench_json.exe
perf mem report --stdio
```

This can show whether samples hit in L1/L2/L3 or spill out to RAM, plus memory-access latency and TLB information.

## `memtrace`

Use `memtrace` to study allocation sites and object lifetimes when time numbers are not enough:

```bash
opam install memtrace memtrace_viewer
```

Then verify whether live tracing is supported in your OCaml 5 switch. Some current multicore setups still report that tracing the running process is not available, so treat `memtrace` as an experimental local workflow until that support is confirmed.

If your switch supports it, the intended workflow is:

```bash
MEMTRACE=bench-json.ctf _build/default/bench/bench_json.exe
memtrace-viewer bench-json.ctf
```

Use it to answer questions like:

- which functions allocate the most
- whether allocations are short-lived or promoted
- whether conversion code dominates parser scanning
- whether a change reduced total allocation or only shifted it elsewhere
