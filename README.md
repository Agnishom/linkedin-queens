# Haskell Solver for LinkedIn Queens

[Blog Post](https://imiron.io/post/linkedin-queens/)

> [!NOTE]
> This repository accompanies the blogpost [Solving LinkedIn Queens with Haskell](https://imiron.io/post/linkedin-queens/). It contains backtracking based solutions to the problem,
> with progressively complex optimizations. We explore different heuristics for generating and ranking candidates, and pruning the search space.

To run benchmarks:

```
cabal bench -O2 --benchmark-options '--timeout 60'
```

## Time for a Selection of Problems

| Problem       | Attempt 0 | Attempt 1 | Attempt 2  | Attempt 3 | Attempt 4 | Attempt 5 (Heap) | Attempt 5 (Set)  | Attempt 6        | SMT Solver (Z3) |
| ------------- | --------- | --------- | ---------- | --------- | --------- | ---------------- | ---------------- | ---------------- | --------------- |
| **Problem 1** | 17.6 ms   | 19.1 μs   | 21.7 μs    | 20.0 μs   | 20.5 μs   | 22.3 μs          | 11.7 μs          | 10.8 μs          | 5.18 ms         |
| **Problem 2** | -         | 17.057 s  | 193 ms     | 172 ms    | 65.5 μs   | 101 μs           | 52.2 μs          | 48.6 μs          | 13.4 ms         |
| **Problem 3** | 2.131 s   | 16.8 μs   | 32.8 μs    | 27.7 μs   | 31.3 μs   | 43.6 μs          | 20.1 μs          | 17.9 μs          | 5.20 ms         |
| **Problem 4** | -         | 10.9 ms   | 817 μs     | 768 μs    | 44.6 μs   | 66.5 μs          | 29.7 μs          | 27.2 μs          | 8.67 ms         |
| **Problem 5** | -         | 4.620 s   | 118 ms     | 105 ms    | 396 μs    | 616 μs           | 304  μs          | 251  μs          | 29.8 ms         |

## Average Time for Solving Official Levels

| Size                   | Attempt 1 | Attempt 2 | Attempt 3 | Attempt 4 | Attempt 5 (Heap) | Attempt 5 (Set) | Attempt 6       | SMT Solver (Z3) |
| ---------------------- | --------- | --------- | --------- | --------- | ---------------- |---------------- | --------------- | --------------- |
| **7** (48 problems)    | 269.66 ms | 5.15 ms   | 4.75 ms   | 189 μs    | 214 μs           | 168 μs          | 159 μs          | 11.02 ms        |
| **8** (145 problems)   | -         | 81.11 ms  | 72.12 ms  | 197 μs    | 269 μs           | 166 μs          | 152 μs          | 20.62 ms        |
| **9** (137 problems)   | -         | -         | -         | 310 μs    | 448 μs           | 275 μs          | 243 μs          | 32.03 ms        |
| **10** (60 problems)   | -         | -         | -         | 463 μs    | 664 μs           | 430 μs          | 376 μs          | 51 ms           |
| **11**  (17 problems)  | -         | -         | -         | 682 μs    | 900 μs           | 652 μs          | 605 μs          | 67 ms           |