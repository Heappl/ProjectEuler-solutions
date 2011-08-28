(ns Clojure.randomBitWalk
  (:require [clojure.contrib.string :as string])
  (:require [clojure.contrib.seq-utils :as sequtils])
  (:require [clojure.contrib.math :as math])
  (:require [clojure.set :as set]))

(defn factorial [n] 
  (loop [k 1 acc 1]
    (if (> k n) acc (recur (inc k) (* acc k)))))

(defn newton [n k]
  (/ (factorial n) (factorial k) (factorial (- n k))))

(defn probOfKFlippedInNthStepOutOfT [k n t]
  (let [flipProb (/ 1 2)]
    (* (newton t k)
       (math/expt flipProb k)
       (math/expt (- 1 flipProb) (- t k)))))

(defn calcProb [k previous step]
  (loop [i 0 acc 0]
    (if (> i k)
      (double acc)
      (let [prevProb (nth previous i)
            newProb (* prevProb (probOfKFlippedInNthStepOutOfT (- k i) step (- 32 i)))]
        (recur (inc i) (if (< i 32) (+ acc newProb) acc))))))

(defn calcStep [previous step]
  (loop [i 0 probs previous]
    (if (> i 32)
      probs
      (let [newProb (calcProb i previous step)]
        (recur (inc i) (assoc probs i newProb))))))

(defn estimate [steps]
  (loop [step 0 acc 0 probs (vec (map (fn [x] (if (= x 0) 1 0)) (range 0 33)))]
    (if (> step steps)
      (double acc)
      (let [nextProbs (calcStep probs step)
            estElem (* (inc step) (nth nextProbs 32))
            nextEst (+ acc estElem)]
        (if (= (rem step 10) 0) (println step (double nextEst)))
        (recur (inc step) nextEst nextProbs)))))

