(ns Clojure.cubePermutations
  (:require [clojure.contrib.string])
  (:require [clojure.contrib.seq-utils])
  (:require [clojure.contrib.math])
  (alias string clojure.contrib.string)
  (alias math clojure.contrib.math)
  (alias sequtils clojure.contrib.seq-utils))

(defn getSortedValue [x]
  (loop [nextX x acc []]
    (if (> nextX 0)
      (recur (math/floor (/ nextX 10)) (conj acc (rem nextX 10)))
      (reduce #(+ (* %1 10) %2) (reverse (sort acc))))))

(defn findSmallest [n]
  (loop [x 1 acc {}]
    (let [cube (* x x x) sortedVal (getSortedValue cube)
          [counter smallest] (get acc sortedVal)
          nextCounter (if (nil? counter) 1 (inc counter))
          nextSmallest (if (nil? smallest) cube (min smallest cube))]
      (if (= nextCounter n)
        nextSmallest
        (recur (inc x) (assoc acc sortedVal [nextCounter nextSmallest]))))))