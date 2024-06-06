### The Sun Tribe Code Challenge
You are challenged to implement a date-interval difference function.  The arguments to the function `difference` will be clojure sets, each
of which contains zero or more [LocalDateRange](https://www.threeten.org/threeten-extra/apidocs/org.threeten.extra/org/threeten/extra/LocalDateRange.html)
objects.  The function must return a set of LocalDateRange objects that represents the [interval difference](http://mathinschool.com/page/8.html) of the 
first set and all additional input sets.

For example, given the a set containing the single data range representing the month of January 2024 and a second set containing a date range
representing the second week of January 2024 and a date range representing the single day of New Year's Day 2024, the function should return
the set with two intervals, one representing the date range January 2-7, 2024 and the other representing the date range January 15-31, 2024. 
Stated using algebraic symbols and leveraging the ISO representation of date intervals:

```
#{"2024-01-01/P1M"} - #{"2024-08-01/P7D" "2024-01-01/P1D"} = #{"2024-01-02/P6D" "2024-01-15/P17D"}`
```

### Context
At Sun Tribe Trading we gather the energy production data of solar generators from various sources (typically APIs from the manufacturers
of solar energy equipment) and submit the data to regulatory authorities.  It's important that we accurately determine the energy data we need
to gather based on the data we already have on hand and the current reporting period.  Date interval operations are central to this
computation.

### Evaluation
Your solution will be evaluated as follows, in decreasing order of importance:
* Correctness.  Do all the tests pass?
* Readability.  Can we understand your code?  Is it well organized?  Is it well documented?  Is it idiomatic?
* Performance.  How long does it take to run the test suite?

Correctness and performance will be evaluated by running this command in the root of your submitted repository:

`time clojure -X:test`

Readability will be evaluated by your potential future colleagues looking at the entire contents of the repository.  You may organize
the code of your implementation in any way you deem appropriate, but you must not modify the `challenge-test` namespace.  You may
add additional tests in separate namespaces.  You may introduce additional dependencies and make other modifications to the `deps.edn`
file, but you must not modify the `:test` alias.

### Scale of the Challenge
We believe that a performance-optimized solution to the problem is difficult and we have not attempted to implement it.  We have 
implemented a correct solution in less than fifty lines of documented Clojure code with no additional dependencies.  It takes ~15 seconds 
to complete the test suite on a 2023 MacBook Pro with an Apple M2 Pro processor and 32G of RAM.

### Hints, maybe helpful
Our solution evolved from the observation that a LocalDateRange object can be represented (inefficiently) by a sequence of 
[LocalDate](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDate.html) objects.  That representation can be
input into a set difference operation, but the result must be "compressed" back into the minimal set of LocalDateRange 
objects.

The `LocalDateRange` class has useful class and instance methods.

Before coding, you should be comfortable with the concept of a [time interval](https://en.wikipedia.org/wiki/ISO_8601#Time_intervals), and
more specifically a (local) date interval as implemented by
the [backing Java library we use](https://www.threeten.org/threeten-extra/apidocs/org.threeten.extra/org/threeten/extra/LocalDateRange.html).

### Submitting your response
This git repository is the entire input you will need for the Sun Tribe code challenge.  Your submission should be a modified version 
of this repository (including our initial commits and the commit(s) of your solution).  You may submit either:

* a link to a public http(s) repository that can be cloned by `git clone <url>`.

-or-

* a repository compressed into a standard zip file that can be opened with `unzip <file>`.
