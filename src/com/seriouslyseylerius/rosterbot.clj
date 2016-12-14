(ns com.seriouslyseylerius.rosterbot
  (:use loco.core
        loco.constraints)
  (:require [loco.automata :as a]
            [clj-time.core :as t]
            [clj-time.periodic :as p]
            [clj-time.predicates :as pr]
            [clj-time.format :as f]
            [cuerdas.core :as s])
  (:gen-class))

(defn all-shift-vars
  "Return all valid team-member/day pairs"
  [team dates]
  (for [n (range (count team))
        d dates]
    [:shift n d]))

(defn shift-var-declarations
  "Define shift variables for each team-member/day pair"
  [team shifts dates]
  (let [shift-ids (concat [-1 -2 0] (map :id shifts))]
    (for [v (all-shift-vars team dates)]
      ($in v shift-ids))))

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
    (and (= 0 (mod (t/in-days (t/interval start day)) (shift :period)))
         (not (contains? (shift :exceptions) day)))

    ;; For weekly recurrence, we need to verify that we're in a valid week
    ;; (by backing up `start` to the preceding Sunday, then checking `:period`)
    ;; Then we need to validate the day of the week
    (= (shift :recurrence) :weekly)
    (let [dow (mod (t/day-of-week start) 7)
          start (t/minus start (t/days dow))]
      (and
       (= 0 (mod (t/in-weeks (t/interval start day)) (shift :period)))
       (some #(= dow %) (shift :days))
       (not (contains? (shift :exceptions) day))))))

(defn shift-reducer
  [acum [ks c]]
  (assoc-in acum ks (max (get-in acum ks 0) c)))

(defn shift-counter
  "Given shifts and a date-range, return days mapped to shift counts"
  [dates shifts]
  (let [day-shifts (for [day dates
                           shift shifts
                           :when (valid-shift day shift (first dates))]
                       [[day (shift :id)] (shift :count)])]
    (reduce shift-reducer {} day-shifts)))

(defn day-constraint
  "Given a team, a day and a map of shift-counts, return a cardinality constraint"
  [team day shift-counts]
  (let [column (for [n (range (count team))]
                 [:shift n day])]
    ($cardinality column shift-counts)))

(defn stringify-shifts
  [shifts]
  (let [str-shifts (s/join "|" (for [shift shifts]
                                 (str "<" shift ">")))]
       (str "(" str-shifts ")")))

(defn shift-shorthand
  [shift shifts]
  (let [shifts (conj (set shifts) 0 -1 -2)
        other (stringify-shifts (disj shifts shift))
        out (stringify-shifts [0 -1 -2])
        off (stringify-shifts [0 -1])
        shift (str "<" shift ">")]
    [shifts other out off]))

(defn shift-constraint
  "Return an automaton requiring `shift` be followed by `count` of another"
  [& {:as kwargs}]
  (let [shifts (kwargs :shifts)
        shift (kwargs :shift)
        s-counted (seq? shift)
        shift (if s-counted
                (first)
                shift)
        shift-count (if s-counted
                      (or (second shift) 1)
                      1)
        follower (kwargs :follower)
        f-counted (seq? follower)
        follower (if f-counted
                   (first follower)
                   follower)
        follower-count (if follower
                         (if f-counted
                           (or (second follower) 1)
                           1)
                         0)
        [shifts other out off shift] (shift-shorthand shift shifts)]
    (a/string->automaton
     (str other "*"
          (apply str (repeat shift-count shift))
          (cond
            (= follower :off) (apply str (repeat follower-count off))
            (= follower :out) (apply str (repeat follower-count out))
            (= follower :other) (apply str (repeat follower-count other))
            (seq? follower) (apply str
                                   (repeat follower-count
                                           (stringify-shifts
                                            (set follower)))))
          other "*"))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
