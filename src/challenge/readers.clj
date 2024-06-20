(ns challenge.readers
  (:import (org.threeten.extra LocalDateRange)
           (java.time LocalDate)))

(defn local-date-range [s] (LocalDateRange/parse s))

(defn local-date-range-as-iso-interval
  "Print the given LocalDateRange `ldr` as an ISO 8601-1:2019 interval in
  <start>/<duration> format."
  [ldr]
  {:pre [(instance? LocalDateRange ldr)]}
  (let [start (.getStart ldr)
        period (.toPeriod ldr)]
    ;; NB: when the start date is the first day of the month and the duration is
    ;; one month, consider printing the interval as "YYYY-MM/P1M".  This has the
    ;; advantage of naturally segregating month intervals from day intervals in
    ;; lexical sorts but has the disadvantage of not being parseable by the
    ;; LocalDateRange class nor by its sibling Interval class.
    (str start "/" period)))

;; We elect to print a LocalDateRange using the ISO 8601-1:2019 <start>/<period>
;; representation.  This is a compact and standardized representation that high-
;; lights our expected use case of specific periods (one day and one month).
;; The use of a LocalDate to represent the date-only start point is not well
;; publicized, probably because the relevant ISO standard (ISO 8601-2:2019) is
;; aggressively copyrighted and the use cases are few compared to other elements
;; of the standard.
(defmethod print-dup LocalDateRange
  [o ^java.io.Writer w]
  (doto w
    (.write "#st/local-date-range \"")
    (.write (local-date-range-as-iso-interval o))
    (.write "\"")))

(defn local-date [s] (LocalDate/parse s))

(defmethod print-dup LocalDate
  [o ^java.io.Writer w]
  (doto w
    (.write "#st/local-date \"")
    (.write (.toString o))
    (.write "\"")))
