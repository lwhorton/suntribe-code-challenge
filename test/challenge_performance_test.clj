(ns challenge-performance-test
  (:require [challenge.core :refer [difference difference-performant]]
            [challenge.readers]
            [clojure.test :refer [deftest is testing]]
            [criterium.core :as criterium])
  (:import (org.threeten.extra LocalDateRange)))

(deftest performance-simple
  (is (let [expected #{#st/local-date-range "2020-01-01/P4Y" #st/local-date-range "2024-01-05/P5Y11M27D"}
            a #{#st/local-date-range "2020-01-01/P10Y"}
            b #{#st/local-date-range "2024-01-01/P4D"}]
        (= expected
           (difference a b)
           (do
             (println "benchmarking" difference)
             (criterium/quick-bench
              (difference a b))
             expected)))))

(deftest performance-optimized
  (is
    ;; validate the test first, then run performance 
   (let [expected #{#st/local-date-range "2020-01-01/P4Y" #st/local-date-range "2024-01-05/P5Y11M27D"}
         a #{#st/local-date-range "2020-01-01/P10Y"}
         b #{#st/local-date-range "2024-01-01/P4D"}]
     (= expected
        (difference-performant a b)
        (do
          (println "benchmarking" difference-performant)
          (criterium/quick-bench
           (difference-performant a b))
          expected)))))
