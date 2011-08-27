(ns Clojure.sortedRadicals
  (:require [clojure.contrib.string :as string])
  (:require [clojure.contrib.seq-utils :as sequtils])
  (:require [clojure.contrib.math :as math])
  (:require [clojure.set :as set])
  (:require [Clojure.primePowers :as primes]))

(defn removeFactor [x f]
  (if (= (rem x f) 0)
    (recur (/ x f) f)
    x))

(defn radical [x primes]
  (let [xsqrt (math/sqrt x)]
	  (loop [[h & tail] primes rad 1 lx x]
	    (if (and h (<= h xsqrt))
        (let [newX (removeFactor lx h)]
          (recur tail (if (< newX lx) (* rad h) rad) newX))
        (* rad lx)))))

(defn radicals [top]
  (let [primes (primes/primes (math/sqrt top))]
    (loop [i 2 acc [[1 1]]]
      (if (> i top)
        (sort acc)
        (let [rad (radical i primes)]
          (recur (inc i) (conj acc [rad i])))))))

(defn e [k top]
  (let [rads (radicals top)]
    (nth rads (dec k))))
