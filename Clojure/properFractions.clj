(ns Clojure.properFractions
  (:require [clojure.contrib.string :as string])
  (:require [clojure.contrib.seq-utils :as sequtils])
  (:require [clojure.contrib.math :as math])
  (:require [clojure.set :as set]))

(defn getMaxSmallerThan [v d]
  (let [aux (/ (math/floor (* v d)) d)] (if (>= aux v) (- aux (/ 1 d)) aux)))
(defn getMinBiggerThan [v d]
  (let [aux (/ (math/ceil (* v d)) d)] (if (<= aux v) (+ aux (/ 1 d)) aux)))

(defn findNextTo [v d closest]
  (if (<= d 0)
    closest
    (recur v (dec d) (max closest (getMaxSmallerThan v d)))))

(defn calcBetween [a b d acc]
  (if (<= d 1)
    acc
    (let [bottom (getMinBiggerThan a d)
          top (getMaxSmallerThan b d)
          r (range bottom (+ top (/ 1 d)) (/ 1 d))
          toCount (filter #(= d (denominator %1)) r)]
      (recur a b (dec d) (+ acc (count toCount))))))
