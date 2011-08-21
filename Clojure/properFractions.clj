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

(defn primes [top]
  (loop [ind 0 prs (vec (range 2 (inc top)))]
    (if (>= ind (count prs))
      prs
	    (let [n (+ ind 2)
            prime (= (nth prs ind) n)
	          newPrs (if prime
	                   (loop [i (+ ind n) currPrs prs]
	                     (if (>= i (count prs)) currPrs (recur (+ i n) (assoc currPrs i n))))
	                   prs)]
        (if prime (recur (inc ind) newPrs) (recur (inc ind) newPrs))))))

(defn removeFactor [x f]
  (loop [nextX x totallyDivided 1]
    (if (> (rem nextX f) 0)
      [nextX totallyDivided]
      (recur (/ nextX f) (* totallyDivided f)))))

(defn totient [n divisors prevTotients]
  (let [ind (- n 2)
        c (nth prevTotients ind)
        d (nth divisors ind)]
    (if (> c 0)
      prevTotients
      (if (= d n)
        (loop [i (* n n) prevVal (- n 1) newTotients (assoc prevTotients ind prevVal)]
          (if (> i (+ (count prevTotients) 1))
            newTotients
            (recur (* i n) (* prevVal n) (assoc newTotients (- i 2) (* prevVal n)))))
        (let [[f1 f2] (removeFactor n d)]
	        (assoc prevTotients
	               ind
	               (* (nth prevTotients (- f1 2)) (nth prevTotients (- f2 2)))))))))

(defn totients [top]
  (loop [n 2
         divisors (primes top)
         totients (vec (repeat (- top 1) 0))]
    (if (> n top)
      totients
      (recur (inc n) divisors (totient n divisors totients)))))

(defn properFractions [top]
  (let [tots (totients top)]
    (reduce #(+ %1 %2) tots)))
