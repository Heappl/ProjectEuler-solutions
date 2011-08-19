(ns Clojure.DiophantineEquations
  (:require [clojure.contrib.string :as string])
  (:require [clojure.contrib.seq-utils :as sequtils])
  (:require [clojure.contrib.math :as math])
  (:require [clojure.set :as set]))

(defn continuedFractionExpansion [x]
  (let [a0 (math/floor (math/sqrt x))]
	  (loop [m 0 d 1 a a0 as [a0] triples {}]
	    (let [mn (- (* d a) m)
	          dn (/ (- x (* mn mn)) d)
	          an (math/floor (/ (+ a0 mn) dn))
            triple [mn dn an]]
         (if (get triples triple)
           (split-at (get triples triple) (map #(bigint %1) as))
           (recur mn dn an (conj as an) (assoc triples triple (count as))))))))

(defn partialCfe [exp initial]
  (loop [[h & tail] (reverse exp) est initial]
    (if h
      (recur tail (+ h (/ 1 (if (= est 0) 1 est))))
      est)))

(defn partialLoopCfe [exp steps]
  (loop [est (partialCfe exp 0) s steps]
    (if (= s 0) est (recur (partialCfe exp est) (dec s)))))

(defn cfe [x steps]
  (let [[iexp lexp] (continuedFractionExpansion x)]
    (partialCfe iexp (partialLoopCfe lexp steps))))

(defn partialConvergents [A0 A1 B0 B1 exp pred]
  (loop [App A0 Ap A1 Bpp B0 Bp B1 [h & tail] exp]
    (if (and h (not (pred Ap Bp)))
      (recur Ap (+ (* h Ap) App) Bp (+ (* h Bp) Bpp) tail)
      (if (not h) nil [Ap Bp]))))

(defn convergents [x times]
  (let [[iexp lexp] (continuedFractionExpansion x)
        [b0 b1 & initTail] (concat iexp (flatten (repeat times lexp)))
        pred (fn [a b] (= 1 (- (* a a) (* x b b))))
        [A B] (partialConvergents b0 (+ (* b0 b1) 1) 1 b1 initTail pred)]
    [x A B]))

(defn isNaturalSquared [x]
  (let [sq (int (math/sqrt x)) bsq (* sq sq)] (= bsq x)))

(defn predMax [col pred]
  )

(defn maximumSolution [top]
  (let [xs (filter #(not (isNaturalSquared %1)) (range 1 top))
        sols (map #(convergents %1 100) xs)
        sortedSols (sort (fn [[n1 x1 y1] [n2 x2 y2]] (> x1 x2)) sols)
        [bestN bestX bestY] (first sortedSols)]
    bestN))

