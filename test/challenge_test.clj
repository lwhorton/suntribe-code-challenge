(ns challenge-test
  (:require [challenge.core :refer [difference]]
            [challenge.readers]
            [clojure.test :refer [deftest is testing]]))

(def week0 #st/local-date-range "2024-01-01/P7D")
(def week1 #st/local-date-range "2024-01-08/P7D")
(def week2 #st/local-date-range "2024-01-15/P7D")
(def week3 #st/local-date-range "2024-01-22/P7D")
(def extra #st/local-date-range "2024-01-29/P3D")
(def month0 #st/local-date-range "2024-01-01/P1M")

(deftest unary
  (is (= #{week0}
         (difference #{week0})))
  (testing "compresses results"
    (is (= #{month0}
           (difference #{week0 week1 week2 week3 extra})))))

(deftest binary
  (testing "equals"
    (is (= #{}
           (difference #{week0} #{week0})))
    (is (= #{}
           (difference #{week0 week1} #{week0 week1}))))
  (testing "overlapped by"
    (is (= #{#st/local-date-range "2024-01-01/P4D"}
           (difference #{week0}
                       #{#st/local-date-range "2024-01-05/P10D"})))
    (is (= #{#st/local-date-range "2024-01-01/P4D"}
           (difference #{week0 week1}
                       #{#st/local-date-range "2024-01-05/P10D"}))))
  (testing "met by"
    (is (= #{week0}
           (difference #{week0} #{week1}))))
  (testing "contains"
    (is (= #{week0 #st/local-date-range "2024-01-15/P17D"}
           (difference #{month0} #{week1}))))
  (testing "during"
    (is (= #{}
           (difference #{week1} #{month0})))))

(deftest variadic
  (is (= #{week0 extra}
         (difference #{month0}
                     #{week1}
                     #{week2 week3}))))

(deftest example-from-readme
  (is (= #{#st/local-date-range "2024-01-02/P6D" #st/local-date-range "2024-01-15/P17D"}
         (difference #{month0}
                     #{week1 #st/local-date-range "2024-01-01/P1D"}))))

(deftest performance
  (is (= #{#st/local-date-range "2020-01-01/P4Y" #st/local-date-range "2024-01-05/P5Y11M27D"}
         (difference #{#st/local-date-range "2020-01-01/P10Y"}
                     #{#st/local-date-range "2024-01-01/P4D"}))))
