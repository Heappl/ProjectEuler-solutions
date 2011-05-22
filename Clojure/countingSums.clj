(ns countingSums
  (:require [clojure.contrib.string])
  (:require [clojure.contrib.seq-utils])
  (:require [clojure.contrib.math])
  (alias string clojure.contrib.string)
  (alias math clojure.contrib.math)
  (alias sequtils clojure.contrib.seq-utils))

(defn massoc [m i j newValue] (assoc m i (assoc (nth m i) j newValue)))
(defn mget [m i j] (nth (nth m i) j))

(defn prepareAcc [n]
  (loop [acc (vec (map (fn [_] (vec (map (fn [_] -1) (range (inc n))))) (range (inc n))))
         k n]
    (if (>= k 0)
      (recur (massoc (massoc (massoc acc 0 k 1) 1 k 0) k 0 (if (= k 0) 1 0)) (dec k))
      acc)))

(defn countSums [n k acc]
  (let [current (mget acc n k)]
    (if (>= current 0)
     [acc current]
     (loop [newAcc acc newCurr 0 i 0]
       (if (> i (unchecked-divide n k))
         [(massoc newAcc n k newCurr) newCurr]
         (let [[tempAcc tempCurr] (countSums (- n (* k i)) (dec k) newAcc)]
           (recur tempAcc (+ newCurr tempCurr) (inc i))))))))

(defn countPrimeSums [n k acc primes]
  (let [current (mget acc n k)]
    (if (>= current 0)
      [acc current]
      (loop [newAcc acc newCurr 0 i 0]
        (if (> i (unchecked-divide n (nth primes k)))
          [(massoc newAcc n k newCurr) newCurr]
          (let [[tempAcc tempCurr] (countPrimeSums (- n (* (nth primes k) i)) (dec k) newAcc primes)]
            (recur tempAcc (+ newCurr tempCurr) (inc i)))))
      )))

(defn primes [n]
  (reduce (fn [acc elem] (if (> elem 0) (conj acc elem) acc)) []
    (let [markNotPrime (fn [p acc]
	                       (if (nth acc p)
		                       (loop [i (* p 2) outAcc acc]
		                         (if (> i n) outAcc (recur (+ i p) (assoc outAcc i 0))))
	                        acc))]
		  (loop [i 2 acc (assoc (vec (range (inc n))) 1 0)]
		    (if (> (* i i) n)
	       acc
	       (recur (inc i) (markNotPrime i acc)))))))
