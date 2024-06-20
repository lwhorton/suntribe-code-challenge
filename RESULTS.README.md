### The Sun Tribe Code Challenge

The code itself is fairly thoroughly documented. 

To compare execution speed of a performant and naive version of the diffing
function, run the `:performance` alias (which uses criterium):

```
clj -X:performance
```

Here's a few samples from a Macbook Pro 2023 @ MacOS 14.1, M3, 18GB Ram:

```
Running tests in #{"test"}

Testing challenge-performance-test

benchmarking #object[challenge.core$difference_performant 0x3513d214 challenge.core$difference_performant@3513d214]
Evaluation count : 150990 in 6 samples of 25165 calls.
             Execution time mean : 4.160919 µs
    Execution time std-deviation : 135.826669 ns
   Execution time lower quantile : 4.023611 µs ( 2.5%)
   Execution time upper quantile : 4.323715 µs (97.5%)
                   Overhead used : 1.564368 ns

benchmarking #object[challenge.core$difference 0x60fc7f43 challenge.core$difference@60fc7f43]
Evaluation count : 66 in 6 samples of 11 calls.
             Execution time mean : 9.714654 ms
    Execution time std-deviation : 220.643131 µs
   Execution time lower quantile : 9.515449 ms ( 2.5%)
   Execution time upper quantile : 9.957954 ms (97.5%)
                   Overhead used : 1.564368 ns

Running tests in #{"test"}

Testing challenge-performance-test

benchmarking #object[challenge.core$difference_performant 0x3513d214 challenge.core$difference_performant@3513d214]
Evaluation count : 150726 in 6 samples of 25121 calls.
             Execution time mean : 4.172963 µs
    Execution time std-deviation : 250.709768 ns
   Execution time lower quantile : 3.959260 µs ( 2.5%)
   Execution time upper quantile : 4.579061 µs (97.5%)
                   Overhead used : 1.617536 ns

Found 1 outliers in 6 samples (16.6667 %)
	low-severe	1 (16.6667 %)
 Variance from outliers : 14.6626 % Variance is moderately inflated by outliers

benchmarking #object[challenge.core$difference 0x49631cfb challenge.core$difference@49631cfb]
Evaluation count : 66 in 6 samples of 11 calls.
             Execution time mean : 9.952773 ms
    Execution time std-deviation : 784.738950 µs
   Execution time lower quantile : 9.102476 ms ( 2.5%)
   Execution time upper quantile : 10.730722 ms (97.5%)
                   Overhead used : 1.617536 ns
```

Which puts the optimized version at about a `2300x` (mean execution time)
improvement over the naive implementation.

> I chose not to spend the time handling the bonus unbounded `LocalDateRange`
> inputs. 
