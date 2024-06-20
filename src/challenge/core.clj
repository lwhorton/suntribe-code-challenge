(ns challenge.core
  "this module handles the computation of differences (subtraction) in date
  ranges. this is similar to set difference { A } - { B }, but is specifically
  intended and optimized to handle ISO 8601 LocalDate and LocalDateRange
  formats.
  
  there are a few important pieces of context to understand before the following
  behavior makes sense:

  1. we make heavy use of LocalDate (java.time) and LocalDateRange
  (org.threeten.extra) java classes. these classes handle tricky computation
  around calendar representations of dates and time periods. the classes handle
  inconsistencies around 28/30/31-day months, period-equality; does 1M 30D? it
  depends on the month, leap-years, and so on. 

  2. the formats and representations of LocalDate and LocalDateRange are
  explicitly defined in ISO 8601, but the generally preferred representation for
  our purposes is: 
  
  <start date (inclusive)>/<period (inclusive)> 
  
  this means that 2024-01-01/3D includes the dates 01, 02, and 03 for a total
  duration of 3 days.
  
  an alternative representation often encountered is:

  <start date (inclusive)>/<end date (exclusive)>. 
  
  this means that 2024-01-01/2024-01-04 includes the dates 01, 02, and 03, but
  not 04, for a total duration of 3 days.
  
  we attempt to maintain consistency around inclusivity and exclusivity
  throughout this module's function signatures, return values, computations, and
  alternate formats/representations.

  3. we expose a custom clojure reader to attempt consistent rendering of
  local-date and local-date-range objects, as defined in #2.

  4. in certain computations we 'explode' a LocalDateRange into a clojure seq of
  LocalDates, one for each day in the range. we call this a 'range-seq'. a
  range-seq DOES NOT MAINTAIN inclusive, exclusive consistency. the seq
  represents exactly the exploded range [inclusive-start, ..., inclusive-end].

  5. in certain computations we leverage a tuple:
  [start (inclusive), end (exclusive)] as an alternative, more-easily computable
  representation of a LocalDateRange. we call this a 'interval'. a
  interval DOES maintain inclusive, exclusive consistency. 
  "
  (:require [clojure.set :as set]
            [challenge.readers])
  (:import (java.time LocalDate Period)
           (org.threeten.extra LocalDateRange)))

(comment
  ;; first, let's explore our domain and class object capabilities:

  ;; useful testing data
  (def week0 #st/local-date-range "2024-01-01/P7D")
  (def week1 #st/local-date-range "2024-01-08/P7D")
  (def week2 #st/local-date-range "2024-01-15/P7D")
  (def week3 #st/local-date-range "2024-01-22/P7D")
  (def extra #st/local-date-range "2024-01-29/P3D")
  (def month0 #st/local-date-range "2024-01-01/P1M")

  ;; sanity checks around LocalDate, LocalDateRange, periods and their
  ;; representations:

  ;; how does strict equality work?
  ;; - it's consistent
  (true? (= #st/local-date "2024-01-02"
            #st/local-date "2024-01-02"
            (.plusDays #st/local-date "2024-01-01" 1)))

  ;; how does equality between different representations work?
  ;; - it's consistent
  (true?
   (=
     ;; start and duration
    #st/local-date-range "2024-01-01/P3D"
     ;; start and end
    #st/local-date-range "2024-01-01/2024-01-04"
     ;; duration and end
    #st/local-date-range "P3D/2024-01-04")

    ;; @TODO - i suppose we could implement our own deftype to represent a
    ;; 'interval'. maybe come back to this?
    ;(= #st/interval ["2024-01-01" "2024-01-04"])
   )

  ;; what are these 'periods' thingies?
  ;; - alternative, equal representations
  (true?
   (=
    (.toPeriod #st/local-date-range "2024-01-01/P1M")
    (.toPeriod #st/local-date-range "2024-01-01/P31D")))

  (true?
   (=
    3
    (.lengthInDays #st/local-date-range "2024-01-01/P3D")))

  ;; do periods concern themselves with boundaries around months?
  ;; - yes! periods take their calendar month into consideration
  (true?
   (=
    #st/local-date-range "2024-01-01/P1M"
    #st/local-date-range "2024-01-01/P31D"))
  (true?
   (=
    #st/local-date-range "2024-01-02/P1M"
    #st/local-date-range "2024-01-02/P31D"))
  (false?
   (=
    #st/local-date-range "2024-02-01/P1M"
    #st/local-date-range "2024-02-01/P31D"))

  (=
   ;; jan = 31 days
   (.toPeriod #st/local-date-range "2024-01-01/P31D")
   ;; april = 30 days
   (.toPeriod #st/local-date-range "2024-04-01/P30D")
   ;; feb = 28 days (normally)
   (.toPeriod #st/local-date-range "2024-02-01/P1M")))

(defn local-date-range->range-seq
  "explode a LocalDateRange into a seq of LocalDates within that range. note
  that according to ISO 8601, a duration includes the start date but excludes
  the end date. the returned seq of local-dates DOES NOT maintain this
  include/exclude consistency. it is [inclusive inclusive]. 'ranges' of 1 day
  are represented as a seq with cardinality 1.

  e.g.

  #local-date-range 2024-01-01/P1D -> [2024-01-01]
  #local-date-range 2024-01-01/P2D -> [2024-01-01 2024-01-02]
  #local-date-range 2024-01-01/P1M -> [2024-01-01 ... 2024-01-31]
  "
  [local-date-range]
  (let [start (.getStart local-date-range)
        count-days (.lengthInDays local-date-range)]
    (for [i (range count-days)]
      (.plusDays start i))))

(comment
  (.getStart #st/local-date-range "2024-01-01/P3D")
  (.lengthInDays #st/local-date-range "2024-01-01/P3D")
  (.toPeriod #st/local-date-range "2024-01-01/P3D")
  (= [#st/local-date "2024-01-01"]
     (local-date-range->range-seq #st/local-date-range "2024-01-01/P1D"))
  (= [#st/local-date "2024-01-01"
      #st/local-date "2024-01-02"
      #st/local-date "2024-01-03"]
     (local-date-range->range-seq #st/local-date-range "2024-01-01/P3D")))

(defn interval->local-date-range
  "given a interval of LocalDates or date-strings [start-date end-date], return a
  LocalDateRange representing the start-date (inclusive) to end-date (exclusive)
  period."
  [[start end]]
  (let [start (if (instance? LocalDate start) start (LocalDate/parse start))
        end (if (instance? LocalDate end) end (LocalDate/parse end))]
    (LocalDateRange/of start end)))

(comment
  ;; this is a round trip sanity check between the reader and our 
  ;; [start end (exclusive)] range tuple representation
  (true?
   (=
    #st/local-date-range "2024-01-01/P1D"
    #st/local-date-range "2024-01-01/2024-01-02"
    (interval->local-date-range [#st/local-date "2024-01-01" #st/local-date "2024-01-02"])))

  (true?
   (=
    #st/local-date-range "2024-01-01/P2D"
    #st/local-date-range "2024-01-01/2024-01-03"
    (interval->local-date-range [#st/local-date "2024-01-01" #st/local-date "2024-01-03"])))

  (true?
   (=
    #st/local-date-range "2024-01-01/P1M"
    #st/local-date-range "2024-01-01/2024-02-01"
    (interval->local-date-range [#st/local-date "2024-01-01" #st/local-date "2024-02-01"]))))

(defn range-seq->intervals
  "given a seq of LocalDates, collapse all strictly sequential dates into a
  tuple [start-date (inclusive) end-date (exclusive)] as an alternate
  representation of the period. returns a seq of range tuples. LocalDates
  without a range are represented as a tuple with <start date>/<start date +1
  day>: [2024-01-01 2024-01-02].

  e.g. 
  [01 02 03, 05, 07 08 09 10]
  -> [[01 04] [05 06] [07 11]]
  "
  [range-seq]
  (when (seq range-seq)
    (let [to-collapse (sort range-seq)]
      (loop [remaining (rest to-collapse)
             start (first to-collapse)
             end (first to-collapse)
             ranges []]
        (if (not (seq remaining))
          ;; base case - we've reached the end of a range and the end of our
          ;; search. add 1 to maintain [start end (exclusive)] consistency.
          (conj ranges [start (.plusDays end 1)])
          (let [curr (first remaining)
                nxt (rest remaining)]
            (if (= (.plusDays end 1) curr)
              (recur nxt start curr ranges)
              ;; we've reached the end of a strictly sequential range, conj this
              ;; range [start end (exclusive)]
              (recur nxt curr curr (conj ranges [start (.plusDays end 1)])))))))))

(comment
  ;; this is a sanity check around computing tuples from seqs:
  (nil? (range-seq->intervals []))

  (=
   [[#st/local-date "2024-01-01" #st/local-date "2024-01-02"]]
   (range-seq->intervals [#st/local-date "2024-01-01"]))

  (=
   [[#st/local-date "2024-01-01" #st/local-date "2024-01-04"]]
   (range-seq->intervals [#st/local-date "2024-01-01"
                          #st/local-date "2024-01-02"
                          #st/local-date "2024-01-03"]))

  (=
   [[#st/local-date "2024-01-01" #st/local-date "2024-01-04"]
    [#st/local-date "2024-01-05" #st/local-date "2024-01-07"]]
   (range-seq->intervals [#st/local-date "2024-01-01"
                          #st/local-date "2024-01-02"
                          #st/local-date "2024-01-03"

                          #st/local-date "2024-01-05"
                          #st/local-date "2024-01-06"]))

  (= [[#st/local-date "2024-01-05" #st/local-date "2024-01-11"]]
     (range-seq->intervals [#st/local-date "2024-01-05"
                            #st/local-date "2024-01-06"
                            #st/local-date "2024-01-07"
                            #st/local-date "2024-01-08"
                            #st/local-date "2024-01-09"
                            #st/local-date "2024-01-10"])))

(defn range-seq->local-date-ranges
  "collapse a range-seq into a minimum coll of LocalDateRanges. note that
  range-seq is [inclusive inclusive]."
  [range-seq]
  (let [intervals (range-seq->intervals range-seq)
        local-date-ranges (map interval->local-date-range intervals)]
    local-date-ranges))

(comment
  ;; this is a sanity checks for collapsing an exploded range-seq back into its
  ;; minimum representation 
  (= []
     (range-seq->local-date-ranges []))

  (=
   [#st/local-date-range "2024-01-01/P3D"]
   (range-seq->local-date-ranges [#st/local-date "2024-01-01"
                                  #st/local-date "2024-01-02"
                                  #st/local-date "2024-01-03"]))

  (= [#st/local-date-range "2024-01-01/P3D" #st/local-date-range "2024-01-05/P6D"]
     (range-seq->local-date-ranges [#st/local-date "2024-01-01"
                                    #st/local-date "2024-01-02"
                                    #st/local-date "2024-01-03"

                                    #st/local-date "2024-01-05"
                                    #st/local-date "2024-01-06"
                                    #st/local-date "2024-01-07"
                                    #st/local-date "2024-01-08"
                                    #st/local-date "2024-01-09"
                                    #st/local-date "2024-01-10"])))

(defn difference* [a bs]
  ;; for a naive first attempt at computing range difference, lets convert
  ;; ranges into seqs of values as a different, more computable representation
  ;; of a range. sets of values will allow core's set/difference function to
  ;; work as-is.
  (let [;; a is the #{set} from which we subtract, but bs is a seq of sets 
        ;; [#{seta} #{setb} ...] to subtract from a. hence the slightly
        ;; odd-looking mapcat reduce.
        a (mapcat local-date-range->range-seq a)
        b (mapcat local-date-range->range-seq bs)]
    (set/difference (set a) (set b))))

(defn difference
  "At a high level, the naive concept for computing the minimal LocalDateRange
  difference follows:

  - convert each range into a series of date values
  - perform a core.set/difference to subtract all B-dates from A-dates
  - sort the difference, then compress the resulting dates back into a minimally
  representative set of LocalDateRange objects.
  "
  [& input]
  (let [[a & rest] input
        diff (difference* a (apply set/union rest))
        ;; compress the results set to a minimum representation leveraging periodic
        ;; representations (ISO 8601-1:2019) <start>/<period>
        compressed (range-seq->local-date-ranges diff)]
    (set compressed)))

(comment
  ;; just to make sure [ ,,, & x ] works the way i think it does...
  (defn &-check [& input]
    (let [[a & rest] input]
      [a rest]))

  (&-check #{} #{})
  (&-check #{:a} #{})
  (&-check #{:a} #{:b})
  (&-check #{:a :b} #{:c :d} #{:e :f})

  ;; how do we continually conj into the same set? 
  (= (apply set/union #{1 2} '(#{4 3} #{6 5}))
     (reduce set/union #{1 2} '(#{4 3} #{6 5})))

  ;; does set difference work 'out of the box' for dates?
  ;; - yes!
  (= #{#st/local-date "2024-01-01"
       #st/local-date "2024-01-02"}
     (clojure.set/difference
      #{#st/local-date "2024-01-01"
        #st/local-date "2024-01-02"
        #st/local-date "2024-01-03"}
      #{#st/local-date "2024-01-03"}))

  ;; some simple sanity checks
  (= #{week0} (difference #{week0}))
  (= #{} (difference #{week0} #{week0}))
  (= #{} (difference #{week0 week1 week2} #{week0} #{week1} #{week2}))
  (= #{#st/local-date-range "2024-01-01/P1M"} (difference #{week0 week1 week2 week3 extra})))

;; 
;; # begin optimized difference attempts
;; 
;; what follows is some exploration of better ways to better optimize our
;; difference computation.
(defn local-date-range->interval
  "convert a LocalDateRange into our [start (inclusive) end (exclusive)]
  interval representation."
  [local-date-range]
  (let [[start end] (clojure.string/split (.toString local-date-range) #"/")]
    [start end]))

(defn max-date-str
  "returns the 'maximum' of two date strings a and b. while core/max only works
  on numbers, internally our date strings are in ISO 8601 which allows
  lexicographical sorting."
  [a b]
  (if (neg? (compare a b)) b a))

(defn before?
  "returns true if a is before b, lexicographically"
  [a b]
  (neg? (compare a b)))

(defn before=?
  "returns true if a is before or equal to b, lexicographically"
  [a b]
  (let [c (compare a b)]
    (or (zero? c) (neg? c))))

(defn after?
  "returns true if a is after b, lexicographically"
  [a b]
  (pos? (compare a b)))

(defn after=?
  "returns true if a is after or equal to b, lexicographically"
  [a b]
  (let [c (compare a b)]
    (or (zero? c) (pos? c))))

(comment
  ;; sanity check the befores and afters
  (before? "2024-01-01" "2024-01-02")
  (before? "2024-01-02" "2024-01-01")
  (before=? "2024-01-01" "2024-01-01")
  (after? "2024-01-01" "2024-01-02")
  (after? "2024-01-02" "2024-01-01")
  (after=? "2024-01-01" "2024-01-01"))

(defn merge-intervals
  "given a seq of intervals, collapse them into a seq of minimally
  representative intervals.

  e.g. 
  [[1 5] [2 10]] -> [[1 10]]
  [[1 3] [5 10]] -> [[1 3] [5 10]]
  "
  [intervals]
  (let [sorted (sort-by first intervals)]
    (sort-by
     first
     (reduce
      (fn [merged interval]
        (let [[prev-start prev-end] (last merged)
              [next-start next-end] interval]
          (if (before? prev-end next-start)
            ;; the current interval starts further along than the previous' end,
            ;; so start a new interval
            (conj merged [next-start next-end])
            ;; the current interval overlaps with the previous, merge them. keep
            ;; vectors so conj is onto the tail.
            (conj (vec (butlast merged)) [prev-start (max-date-str prev-end next-end)]))))
      ;; base case - have to start with the first interval
      [(first sorted)]
      (rest sorted)))))

(comment
  ;; does merge-intervals work the way we think it does?
  (def example-range [["2024-01-01" "2024-02-01"] ;; one month
                      ["2024-01-30" "2024-02-03"] ;; with a few overlapping days
                      ["2024-03-01" "2024-03-08"] ;; an unrelated week
                      ["2024-03-10" "2024-03-17"] ;; a different week 
                      ["2024-03-13" "2024-03-20"] ;; that overlaps the previous week
                      ])
  (merge-intervals example-range)

  (let [a (local-date-range->interval #st/local-date-range "2024-01-01/P7D")
        b (local-date-range->interval #st/local-date-range "2024-01-09/P23D")
        c (local-date-range->interval #st/local-date-range "2024-02-01/P1W")
        d (local-date-range->interval #st/local-date-range "2024-02-01/P1M")
        e (local-date-range->interval #st/local-date-range "2024-02-15/P1M")]
    (and
     (= (merge-intervals [a b])
        [["2024-01-01" "2024-01-08"] ["2024-01-09" "2024-02-01"]])

     (= (merge-intervals [c d e])
        [["2024-02-01" "2024-03-15"]])))

  (let [as (map local-date-range->interval #{#st/local-date-range "2024-01-01/P2M"
                                             #st/local-date-range "2024-01-15/P1M"
                                             #st/local-date-range "2024-06-01/P1W"
                                             #st/local-date-range "2024-06-04/P1W"})

        m (merge-intervals as)]
    [:intervals as
     :reduced m]))

(defn subtract-intervals
  "subtract a seq of [start end] intervals from another seq of intervals.

  *subtraction assumes each interval is already sorted by its start date*

  subtracting intervals is pretty tricky. remember we have a seq of intervals
  from which we are subtracting another seq of intervals. we'll iterate in
  tandem the intervals in both seqs by maintaining two positions in the seqs,
  accumulating results and progressing the positions as we iterate. based on the
  overlap, or lack of overlap, we will move one pointer forward at a time to the next
  interval in the corresponding list. by the end, we'll have exhausted at least
  one list and have our results of subtracted intervals. you might want to get
  out a piece of paper; the inherent complexity of this subtraction operation is
  kind of intense.

  |-----|-----|-----|-----|-----|
  01                            25

  given intervals-a: 
  |-----|                 |-----|
  01    05                20    25
              |-----|          
              10   15

  and intervals-b:

        |-----|-----|-----|-|
        05                 21


  then a - b results in:
  |-----|                   |---|
  01    05                  22  25

  we have a bunch of possible states, but they can loosely be categorized as
  follows. see the included rich comments for details. 

  no overlap
    * A is completely before B
    * A is completely after B

  partial overlap
    * A starts before B, ends inside B
    * A starts inside B, ends after B
    * A starts before B, ends after B (B is within A)
    * A starts inside B, ends inside B (A is within B)

  complete overlap
    * A is exactly the same as B

  half-exact
    * A or B starts on the same day, but doesn't end on the same day
    * A or B ends on the same day, but doesn't start on the same day
  "
  [intervals-a intervals-b]
  (comment
    ;; we have fairly small cross product of states. rather than invent some
    ;; clever algebraic deductions, we can more easily generate all possible
    ;; states, then think through their outcomes. note how a lot of states
    ;; mirror one another. hopefully that makes this easier to grok.
    (for [x [:a :b]
          y [:b :a]
          :when (not= x y)
          relation-a [:before :inside :after :same]
          relation-b [:before :inside :after :same]]
      [[x :start relation-a y]
       [x :end relation-b y]])

    ;; a definition of the following structure:
    [['start-location 'end-location] ['overlap-condition 'range-to-include 'pointer-to-progress]]
    ;; 'range-to-include is a bit tricky, but think of it like this: {A}
    ;; intervals define a range. we're subtracing {B} intervals from that range.
    ;; if there's no overlap between two intervals, we add {A} to the result. if
    ;; there's partial or complete overlap, we add only the parts of {A} that
    ;; are not subtracted out by {B}, based on the start/stop of overlap. it
    ;; will probably help to get out pen+paper and recreate this state
    ;; conditions as a diagram. 
    ;; 
    ;; there's probably some boolean reduction to be done here- a lot of these
    ;; conditions are mirrors of one another, but we're trying to avoid being
    ;; overly clever for the moment.
    (def states
      [;; the straightforward cases - no overlap
       [[:a :start :before :b] [:a :end :before :b]] [nil [:start-a :end-a] :progress-a] ;; mirror 1
       [[:a :start :after :b] [:a :end :after :b]] [nil nil :progress-b] ;; mirror 2
       [[:b :start :before :a] [:b :end :before :a]] [nil nil :progress-b] ;; mirror 2
       [[:b :start :after :a] [:b :end :after :a]] [nil [:start-a :end-a] :progress-a] ;; mirror 1

       ;; the tricky cases - partial overlap
       [[:a :start :before :b] [:a :end :inside :b]] [:partial [:start-a :start-b] :progress-a] ;; mirror 6
       [[:a :start :inside :b] [:a :end :after :b]] [:partial [:end-b :end-a] :progress-b] ;; mirror 4
       [[:b :start :before :a] [:b :end :inside :a]] [:partial [:end-b :end-a] :progress-b] ;; mirror 4
       [[:b :start :inside :a] [:b :end :after :a]] [:partial [:start-a :start-b] :progress-a] ;; mirror 6

       ;; the less-tricky cases - complete overlap
       [[:a :start :inside :b] [:a :end :inside :b]] [:complete nil :progress-a] ;; mirror 5
       [[:a :start :before :b] [:a :end :after :b]] [:complete [[:start-a :start-b] [:end-b :end-a]] :progress-both] ;; mirror 3
       [[:b :start :inside :a] [:b :end :inside :a]] [:complete [[:start-a :start-b] [:end-b :end-a]] :progress-both] ;; mirror 3
       [[:b :start :before :a] [:b :end :after :a]] [:complete nil :progress-a] ;; mirror 5

       ;; the abnormal cases - start or end at the same point
       [[:a :start :same :b] [:a :end :inside :b]] [:partial nil :progress-a] ;; mirror 7
       [[:a :start :same :b] [:a :end :after :b]] [:partial [:end-b :end-a] :progress-b] ;; mirror 4
       [[:b :start :same :a] [:b :end :inside :a]] [:partial [:end-b :end-a] :progress-b] ;; mirror 4
       [[:b :start :same :a] [:b :end :after :a]] [:partial nil :progress-a] ;; mirror 7
       [[:a :start :before :b] [:a :end :same :b]] [:partial [:start-a :start-b] :progress-both]
       [[:b :start :before :a] [:b :end :same :a]] [:partial nil :progress-both]

       ;; the identical ranges
       [[:a :start :same :b] [:a :end :same :b]] [:identical nil :progress-both]
       [[:b :start :same :a] [:b :end :same :a]] [:identical nil :progress-both]

       ;; all the invalid cases - a or b cannot end before they start
       [[:a :start :inside :b] [:a :end :before :b]] [:invalid]
       [[:a :start :after :b] [:a :end :before :b]] [:invalid]
       [[:a :start :after :b] [:a :end :inside :b]] [:invalid]
       [[:b :start :inside :a] [:b :end :before :a]] [:invalid]
       [[:b :start :after :a] [:b :end :before :a]] [:invalid]
       [[:b :start :after :a] [:b :end :inside :a]] [:invalid]
       [[:a :start :same :b] [:a :end :before :b]] [:invalid]
       [[:b :start :same :a] [:b :end :before :a]] [:invalid]]))

  ;; TODO - if we boolean-reduce the structure above, it could theoretically be
  ;; used as a ruleset (core.match?) for driving the algorithm's behavior,
  ;; instead of this loop/recur that is fairly inelegent and repetitive...
  (loop [a intervals-a
         b intervals-b
         result []]
    (let [[a-start a-end] (first a)
          [b-start b-end] (first b)]
      #_(tap> {:a (first a)
               :b (first b)
               :result result})
      (cond
        ;; base case: 'a is empty, meaning we have no remaining ranges from
        ;; which to subtract. return whatever result we have aggregated.
        (empty? a)
        result

        ;; base case: 'b is empty, meaning we have nothing else to subtract
        ;; from 'a. aggregate all remaining intervals in 'a and return.
        (empty? b) (concat result a)

        ;; the straightforward cases - no overlap

        ;; mirror 1
        (and (before? a-start b-start) (before? a-end b-end))
        (recur (rest a) b (conj result [a-start a-end]))

        ;; mirror 2
        (and (after? a-start b-start) (after? a-end b-end))
        (recur a (rest b) result)

        ;; the tricky cases - partial overlap

        ;; mirror 6
        (and (before? a-start b-start) (before? a-end b-end))
        (recur (rest a) b (conj result [a-start b-start]))

        ;; mirror 4
        (and (after? a-start b-start) (after? a-end b-end))
        (recur a (rest b) (conj result [b-end a-end]))

        ;; the less-tricky cases - complete overlap

        ;; mirror 5
        (and (after? a-start b-start) (before? a-end b-end))
        (recur (rest a) b result)

        ;; mirror 3
        (and (before? a-start b-start) (after? a-end b-end))
        (recur (rest a) (rest b) (conj result
                                       [a-start b-start]
                                       [b-end a-end]))

        ;; the abnormal cases 

        ;; mirror 7
        (and (= a-start b-start) (before? a-end b-end))
        (recur (rest a) b result)

        ;; mirror 4
        (and (= a-start b-start) (after? a-end b-end))
        (recur a (rest b) (conj result [b-end a-end]))

        (and (before? a-start b-start) (= a-end b-end))
        (recur (rest a) (rest b) (conj result [a-start b-start]))

        (and (before? b-start a-start) (= b-end a-end))
        (recur (rest a) (rest b) result)

        ;; the identical ranges
        (and (= a-start b-start) (= a-end b-end))
        (recur (rest a) (rest b) result)

        :else
        (println "we have an issue in interval-subtraction" (first a) (first b))))))

(comment
  ;; lets try out some subtractions
  (let [a (local-date-range->interval #st/local-date-range "2024-01-01/P2M")
        b (local-date-range->interval #st/local-date-range "2024-01-15/P1M")
        [as bs] [(merge-intervals [a])
                 (merge-intervals [b])]]
    {:intervals [as bs]
     :reduced (subtract-intervals as bs)})
  
  (subtract-intervals (merge-intervals (map local-date-range->interval #{month0 week1}))
                      (merge-intervals (map local-date-range->interval #{week2 week3 extra})))

  (subtract-intervals (merge-intervals (map local-date-range->interval #{#st/local-date-range "2020-01-01/P10Y"}))
                      (merge-intervals (map local-date-range->interval #{week2 week3 extra}))))

(defn difference-performant
  "At a high level, the optimized concept for computing the minimal LocalDateRange
  difference follows...

  - don't explode each range into a seq of local-date, instead convert each
  range into an *efficient* [start end] interval
  - sort, then merge overlapping intervals to reduce the search space into sets
  of compressed intervals
  - iterate the interval sets at the same time, subtracting one from the other
  to produce a diff
  - convert the remaining intervals back to LocalDateRange objects 
  "
  [& input]
  (let [[a & rest] input
        as (merge-intervals (map local-date-range->interval a))
        bs (merge-intervals (map local-date-range->interval (apply set/union #{} rest)))

        diff (subtract-intervals as bs)
        as-ranges (map interval->local-date-range diff)]
    (set as-ranges)))

(comment
  ;; more elaborate tests are in /test/**
  (difference-performant #{#st/local-date-range "2024-01-01/P1Y"} #{week1})
  (difference-performant #{#st/local-date-range "2020-01-01/P10Y"} #{week1 week2 week3 extra})
  (difference-performant #{#st/local-date-range "2020-01-01/P10Y"}
                         #{#st/local-date-range "2024-01-01/P4D"}))
