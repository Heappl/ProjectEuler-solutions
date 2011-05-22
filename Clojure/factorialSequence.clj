(ns factorialSequence
  (:require [clojure.contrib.string])
  (:require [clojure.contrib.seq-utils])
  (:require [clojure.contrib.math])
  (alias string clojure.contrib.string)
  (alias math clojure.contrib.math)
  (alias sequtils clojure.contrib.seq-utils))

(defn factorial [n] (apply * (range 1 (inc n))))

(def digFactorials [1 1 2 6 24 120 720 (factorial 7) (factorial 8) (factorial 9)])

(defn convert [n]
  (loop [i n acc 0]
    (if (> i 0)
      (recur (unchecked-divide i 10) (+ acc (get digFactorials (rem i 10))))
      acc)))

(defn replaceByIndex [rmap col]
  (loop [ks (keys rmap) rcol col]
    (let [k (first ks)]
      (if k
        (recur (rest ks) (assoc rcol k (get rmap k)))
        rcol))))

(defn initialMem []
  (replaceByIndex {1 1, 2 1, 145 1, 169 3, 363601 3, 1454 3, 871 2, 45361 2, 872 2, 45362 2, 40585 1}
           (vec (for [x (range 2540162)] -1))))
(defn initialMemStub []
  (vec (for [x (range 102)] 0)))

(defn findNonRepeatingSize [[n mem]]
	(if (> (nth mem n) -1)
   [(nth mem n) mem]
   (let [[c newMem] (findNonRepeatingSize [(convert n) mem])]
     [(inc c) (assoc mem n (inc c))])))

(defn mwhile [testFun bodyFun initial]
  (loop [i initial]
    (if (testFun i)
     		(recur (bodyFun i))
      i)))

(defn findStub [[n mem]] [1 mem])

(defn findAllNonRepeatingSizesUpTo [x]
  (loop [i x mem (initialMem)]
    (if (= 0 (rem i 1000)) (println i))
    (if (< i 2)
      mem
      (recur (dec i) (let [[_, newMem] (findNonRepeatingSize [i mem])] newMem)))))
  

(defn find60s [m]
  (count (filter #(= % 60) (findAllNonRepeatingSizesUpTo m))))


