(ns Clojure.minimalMatrixPath
  (:require [clojure.contrib.string :as string])
  (:require [clojure.contrib.seq-utils :as sequtils])
  (:require [clojure.contrib.math :as math])
  (:require [clojure.set :as set]))

(defn transpose [m]
  (loop [[h & tail] m nextM [] accL [] accM []]
    (if h
      (let [[e & ltail] h]
        (if e
          (recur tail (conj nextM (if ltail ltail [])) (conj accL e) accM)
          accM))
      (recur nextM [] [] (conj accM accL)))))

(def matrix 
  (transpose (map #(map (fn [n] (Integer/parseInt n)) (string/split #"," %1))
                  (string/split-lines (slurp "d:/matrix.txt")))))

(defn addIfNotNil [x y] (if x (+ x y) x))

(defn minimalPathInRow [row weights]
  (loop [n (count row) outrow row]
    (if (> n 0)
      (recur (dec n)
             (loop [temprow outrow i 0]
               (if (>= i (count row))
                 temprow
                 (let [c1 (addIfNotNil (get temprow (dec i)) (nth weights i))
                       c2 (nth temprow i)
                       c3 (addIfNotNil (get temprow (inc i)) (nth weights i))
                       cands (filter #(not (nil? %1)) [c1 c2 c3])]
                   (recur (assoc temprow i (apply min cands)) (inc i))))))
      outrow)))

(defn moveRight [row weights]
  (loop [[hr & rtail] row [hw & wtail] weights acc []]
    (if hr
      (recur rtail wtail (conj acc (+ hr hw)))
      acc)))

(defn minimalPath [m]
  (loop [[h & tail] m curr (repeat (count h) 0)]
    (if h
      (recur tail (minimalPathInRow (moveRight curr h) h))
      curr)))
