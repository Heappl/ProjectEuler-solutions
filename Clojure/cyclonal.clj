(ns Clojure.cyclonal
  (:require [clojure.contrib.string :as string])
  (:require [clojure.contrib.seq-utils :as sequtils])
  (:require [clojure.contrib.math :as math])
  (:require [clojure.set :as set]))


;(112233445566)

(defn calcGonal [func n acc]
  (let [v (func n)]
    (if (> v 10000) acc (recur func (inc n) (if (> v 999) (conj acc v) acc)))))

(def triangle (calcGonal (fn [n] (/ (* n (inc n)) 2)) 1 []))
(def square (calcGonal (fn [n] (* n n)) 1 []))
(def pentagonal (calcGonal (fn [n] (/ (* n (dec (* 3 n))) 2)) 1 []))
(def hexagonal (calcGonal (fn [n] (* n (dec (* 2 n)))) 1 []))
(def heptagonal (calcGonal (fn [n] (/ (* n (- (* 5 n) 3)) 2)) 1 []))
(def octagonal (calcGonal (fn [n] (* n (- (* 3 n) 2))) 1 []))

(def gonal [heptagonal pentagonal hexagonal square triangle])

(defn neighCand [g c] (= (rem g 100) (unchecked-divide c 100)))

(defn nextPrefixes [prefix [h & tail] acc]
  (if h
    (if (neighCand prefix h)
      (recur prefix tail (conj acc (+ (* prefix 100) (rem h 100))))
      (recur prefix tail acc))
    acc))

(defn nextPrefixesWithLeftSet [prefix [h & setstail] setsacc acc]
  (if h
    (recur prefix setstail (conj setsacc h) (conj acc [(concat setstail setsacc) (nextPrefixes prefix h [])]))
    (filter (fn [[s c]] (not (empty? c))) acc)))

(defn findNextPrefixesForOneSets [[sets [h & tail]]  acc]
  (if h
    (recur [sets tail] (concat acc (nextPrefixesWithLeftSet h sets [] [])))
    acc))

(defn findNextPrefixes [[sets & tail] acc]
  (if sets
    (recur tail (concat acc (findNextPrefixesForOneSets sets [])))
    acc))

(defn printSets [[sets cands]] (print cands))
(defn printCands [[sets & tail]]
  (if sets
    (let [] (printSets sets) (recur tail))
    (println "end")))

(defn isCycle [[sets [cand]]]
  (= (rem cand 100) (unchecked-divide cand 1000000000000)))

(defn findCycles []
  (loop [n 0 cands [[gonal octagonal]]]
    (printCands cands)
    (if (> n 4)
      (filter isCycle cands)
      (recur (inc n) (findNextPrefixes cands [])))))

(defn sumCycle [x]
  (loop [acc 0 nextX x]
    (if (> nextX 1000)
      (recur (+ acc (rem nextX 10000)) (unchecked-divide nextX (long 100)))
      acc)))
