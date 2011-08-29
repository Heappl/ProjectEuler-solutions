(ns Clojure.pythagoreanTriples
  (:require [clojure.contrib.string :as string])
  (:require [clojure.contrib.seq-utils :as sequtils])
  (:require [clojure.contrib.math :as math])
  (:require [clojure.set :as set]))

(defn generateTriplesGivenMN [m n maxLength soFar]
  (let [msq (* m m) nsq (* n n)
        a (- msq nsq) b (* 2 m n) c (+ msq nsq)]
    (loop [k 1 acc soFar]
      (if (<= (* k (+ a b c)) maxLength)
        (recur (inc k) (conj acc (sort [(* k a) (* k b) (* k c)])))
        acc))))

(defn generateTriplesGivenM [m maxLength soFar]
  (loop [n 1 acc soFar]
    (if (>= n m)
      acc
      (if (= (math/gcd m n) 1)
        (recur (inc n) (generateTriplesGivenMN m n maxLength acc))
        (recur (inc n) acc)))))

(defn generateTriples [maxLength]
  (loop [m 2 acc #{}]
    (if (> (* m m) maxLength)
      acc
	    (recur (inc m) (generateTriplesGivenM m maxLength acc)))))

(defn update [m k v]
  (let [pv (get m k)]
    (assoc m k (+ (if pv pv 0) v))))

(defn countUniqueLengths [maxLength]
  (let [triples (generateTriples maxLength)]
    (loop [[h & tail] (vec triples) acc {}]
      (if h
        (let [[a b c] h l (+ a b c)] (recur tail (update acc l 1)))
        (count (filter (fn [[l c]] (= c 1)) acc))))))
