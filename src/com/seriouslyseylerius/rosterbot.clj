(ns com.seriouslyseylerius.rosterbot
  (:use loco.core
        loco.constraints)
  (:require [loco.automata :as a]
            [clj-time.core :as t]
            [clj-time.periodic :as p]
            [clj-time.predicates :as pr]
            [clj-time.format :as f]
            [cuerdas.core :as s]
            [clojure.spec :as spec]
            [clojure.set :as set])
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

(defn shift-count-cleaner
  [[ks c]]
  [ks (if (nil? c) 0 c)])

(defn shift-counter
  "Given shifts and a date-range, return days mapped to shift counts"
  [dates shifts]
  (let [day-shifts (map shift-count-cleaner
                        (for [day dates
                              shift shifts
                              :when (valid-shift day shift (first dates))]
                          [[day (shift :id)] (shift :count)]))]
    (reduce shift-reducer {} day-shifts)))

(defn day-constraint
  "Given a team, a day and a map of shift-counts, return a cardinality constraint"
  [team [day shift-counts]]
  (let [column (for [n (range (count team))]
                 [:shift n day])]
    ($cardinality column shift-counts)))

(defn day-constraints
  [dates team shifts]
  (map (partial day-constraint team) (shift-counter dates shifts)))

(defn stringify-shifts
  [shifts]
  (let [str-shifts (s/join "|" (for [shift shifts]
                                 (str "<" shift ">")))]
    (str "(" str-shifts ")")))

(defn shift-automaton
  [shifts rule]
  (a/string->automaton
   (let [shifts (set/union (set shifts) #{0 -1})
         shift (first rule)
         other (stringify-shifts (disj shifts shift))]
     (->> rule
          (map #(str "<" % ">"))
          (apply str)
          (#(str "(" other "*(" % ")*" other "*)*"))))))

(defn shift-requirements
  [dates team shifts rules]
  (let [constraints (map (partial shift-automaton shifts) rules)
        automaton (reduce a/union constraints)
        rows (partition (count dates) (for [t team d dates] [:shift t d]))]
    (map (partial $regular automaton) rows)))

(defn external-assignment-constraints
  [externals]
  (for [[team-member out-dates] externals
        day out-dates]
    ($= [:shift team-member day] -1)))

(defn personal-assignment-constraints
  [personals]
  (for [[team-member off-dates] personals
        day off-dates]
    ($= [:shift team-member day] 0)))

(defn score
  [dates team shifts rules personals]
  (let [personal-constraints (personal-assignment-constraints personals)
        personal-handles (map #(keyword (str "p" %))
                              (range (count personal-constraints)))
        constraints (map (partial shift-automaton (map :id shifts)) rules)
        automaton (reduce a/union constraints)
        rows (partition (count dates) (for [t team d dates] [:shift t d]))
        shift-constraints (map (partial $regular automaton) rows)
        row-handles (map #(keyword (str "r" %))
                         (range (count shift-constraints)))]
    (conj (mapcat (partial apply concat)
                  (for [[i row-const] (map vector
                                           row-handles shift-constraints)]
                    [($in i 0 1)
                     ($= i ($reify row-const))])
                  (for [[j p-const] (map vector
                                         personal-handles
                                         personal-constraints)]
                    [($in j 0 1)
                     ($= j ($reify p-const))]))
          (let [shift-score ($* 500 (apply $+ row-handles))
                personals-score ($* 100 (apply $+ personal-handles))]
            ($+ shift-score personals-score)))))

(defn solve-roster
  [dates team shifts rules externals personals]
  (solution (concat (shift-var-declarations team shifts dates)
                    (day-constraints dates team shifts)
                    (shift-requirements dates team (map :id shifts) (:required rules))
                    (external-assignment-constraints externals))
            ;; :maximize (score dates team shifts (:optional rules) personals)
            ))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
