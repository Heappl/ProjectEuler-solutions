(ns gridWithRectangulars
  (:require [clojure.contrib.string])
  (:require [clojure.contrib.seq-utils])
  (:require [clojure.contrib.math])
  (alias string clojure.contrib.string)
  (alias math clojure.contrib.math)
  (alias sequtils clojure.contrib.seq-utils))

(defn calcStep [k l]
  (* (+ k 1) (+ l 1) l 1/2))

(defn goUp [[k l v]]
  [(inc k) l (+ v (calcStep k l))])

(defn goRight [[k l v]]
  [k (inc l) (+ v (calcStep l k))])

(defn goDown [[k l v]]
  [(dec k) l (- v (calcStep (dec k) l))])

(defn goLeft [[k l v]]
  [k (dec l) (- v (calcStep (dec l) k))])

(defn mwhile [testFun bodyFun initial]
  (loop [i initial]
    (if (testFun i)
     		(recur (bodyFun i))
      i)))

(defn findInitialValue [threshold]
  (mwhile (fn [[_, _, v]] (< v threshold)) #(goUp (goRight %)) [1 1 1]))

(defn chooseClosest [threshold [k1 l1 v1] [k2 l2 v2]]
  (if (<= (math/abs (- v1 threshold)) (math/abs (- v2 threshold)))
    [k1 l1 v1]
    [k2 l2 v2]
    ))

(defn findClosest [threshold initial stepFun]
  (let [[_ best] (mwhile (fn [[[k l _] _]] (and (> k 1) (> l 1)))
                    (fn [[i best]]
                      (let [nextI (stepFun i)] [nextI (chooseClosest threshold best nextI)]))
                    [initial initial])]
        best))

(defn step [threshold testFun stepFun closingFun backFun previous]
  (backFun (mwhile (fn [[_, _, v]] (testFun v)) #(closingFun %) (stepFun previous)))
  )

(defn closest [threshold]
  (let [initial (findInitialValue threshold)]
    (chooseClosest threshold
                   (findClosest threshold
                                initial
                                #(step threshold (fn [v] (> v threshold)) goUp goLeft goRight %))
                   (findClosest threshold
                                (goDown (goLeft initial))
                                #(step threshold (fn [v] (< v threshold)) goLeft goUp goDown %)))))



