(ns Clojure.magic5gonRing
  (:require [clojure.contrib.string :as string])
  (:require [clojure.contrib.seq-utils :as sequtils])
  (:require [clojure.contrib.math :as math])
  (:require [clojure.set :as set]))

(defn getComb [arg]
  (loop [x arg acc [] i 1]
    (if (> x 0)
      (recur (bit-shift-right x 1) (if (= (bit-and x 1) 1) (conj acc i) acc) (inc i))
      acc)))

(def comb1 [[1 2] [1 3] [1 4] [2 3] [2 4] [3 4]])
(def comb2 [[0 1] [1 0]])
(def ring [[0 1] [1 2] [2 3] [3 4] [4 0]])

(defn pickComb [repr svls]
  (let [c1 (comb1 (int (/ repr 2)))
        c2 (comb2 (rem repr 2))
        firstPick [(svls 0) (svls (c1 0)) (svls (c1 1))]
        leftSvls (vec (apply disj (set svls) firstPick))
        secondPick [(leftSvls (c2 0)) (leftSvls (c2 1))]
        pick [(firstPick 0) (firstPick 1) (secondPick 0) (secondPick 1) (firstPick 2)]]
    pick))

(defn correct [sum comb]
  (loop [leftVals (apply disj #{1 2 3 4 5 6 7 8 9 10} comb)
         [h & tail] ring]
    (if h
      (let [v1 (comb (h 0)) v2 (comb (h 1)) v3 (- sum (+ v1 v2))]
        (if (get leftVals v3)
          (recur (disj leftVals v3) tail)
          false))
      true)))

(defn findSumAndCombs [pickedRing]
  (loop [sum 11 acc []]
    (if (> sum 19)
      acc
      (recur (inc sum) (if (correct sum pickedRing) (conj acc [sum pickedRing]) acc)))))

(defn findCombs [picked]
  (loop [order 0 acc []]
    (if (> order 11)
      acc
      (let [pickedRing (pickComb order picked)]
        (recur (inc order) (concat acc (findSumAndCombs pickedRing)))))))

(defn findAllCombs []
  (loop [pick (+ 1 2 4 8 16) acc []]
    (if (> pick (+ 16 32 64 128 256))
      acc
      (let [comb (getComb pick)]
        (recur (inc pick) (if (= (count comb) 5) (concat acc (findCombs comb)) acc))))))

