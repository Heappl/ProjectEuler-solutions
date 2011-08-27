(ns Clojure.primePowers
  (:require [clojure.contrib.string :as string])
  (:require [clojure.contrib.seq-utils :as sequtils])
  (:require [clojure.contrib.math :as math])
  (:require [clojure.set :as set]))

(defn isPrime [x primes]
  (let [xsqrt (int (math/sqrt x))]
	  (loop [[h & tail] primes]
	    (if (and h (<= h xsqrt)) (if (= (rem x h) 0) false (recur tail)) true))))

(defn primes [top]
  (loop [x 3 acc [2]]
    (if (> x top)
      acc
      (recur (+ x 2) (if (isPrime x acc) (conj acc x) acc)))))

(defn countPowers [soFar top primes found power]
  (loop [[h & tail] primes c 0 newFound found]
    (let [v (if h (+ soFar (math/expt h power)))]
	    (if (and h (< v top))
        (let [[nc nFound] (if (> power 2)
                            (countPowers v top primes newFound (dec power))
                            [(if (get newFound v) 0 1) (conj newFound v)])]
          (recur tail (+ nc c) nFound))
	      [c newFound]))))

(defn countPrimePowers [top]
  (let [[c found] (countPowers 0 top (primes (math/sqrt top)) #{} 4)]
    c))
