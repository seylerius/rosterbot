(ns com.seriouslyseylerius.rosterbot
  (:use loco.core
        loco.constraints)
  (:require [clj-time.core :as t]
            [clj-time.periodic :as p]
            [clj-time.predicates :as pr]
            [clj-time.format :as f])
  (:gen-class))

(defn date-range
  "Given two dates, return the inclusive range between them"
  [d1 d2]
  (take-while #(not (t/after? % d2))
              (p/periodic-seq d1 (t/days 1))))

(defn valid-shift
  "Given a day, the start of the roster, and a shift, validate the shift"
  [day shift start]
  (cond
    ;; For daily recurrence, verify that `day` is `:period` days from `start`
    (= (shift :recurrence) :daily)
    (= 0 (mod (t/in-days (t/interval start day)) (shift :period)))

    ;; For weekly recurrence, we need to verify that we're in a valid week
    ;; (by backing up `start` to the preceding Sunday, then checking `:period`)
    ;; Then we need to validate the day of the week
    (= (shift :recurrence) :weekly)
    (let [dow (mod (t/day-of-week start) 7)
          start (t/minus start (t/days dow))]
      (and
       (= 0 (mod (t/in-weeks (t/interval start day)) (shift :period)))
       (some #(= dow %) (shift :days))))))

(defn make-day
  "Generate variables and constraints for a day's shifts and employees"
  [day shifts employees start]
  (map ))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
