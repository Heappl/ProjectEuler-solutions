(ns Clojure.bouncyNumbers
  (:require [clojure.contrib.string :as string])
  (:require [clojure.contrib.seq-utils :as sequtils])
  (:require [clojure.contrib.math :as math])
  (:require [clojure.set :as set]))

(defn update [hm k v]
  (let [previous (if (nil? (get hm k)) 0 (get hm k))]
    (assoc hm k (+ v previous))))
(defn updateAll [hm [k & ktail] v]
  (if (nil? k)
    hm
    (recur (update hm k v) ktail v)))

(defn calcNextIncreasing [prevResults]
  (loop [k 9 nextResults {9 1}]
		(if (< k 0)
			nextResults
	   (recur (dec k) (updateAll nextResults (range 0 k) (get prevResults k))))))

(defn calcNextDecreasing [prevResults]
  (loop [k 1 nextResults {0 1}]
		(if (= k 9)
     nextResults
	   (recur (inc k) (updateAll nextResults (range k 10) (get prevResults k))))))

(defn calcCounts [top calcNextFun initial]
  (loop [n 2 results {1 initial}]
		(if (> n top)
			results
      (recur (inc n) (assoc results n (calcNextFun (get results (dec n))))))))
