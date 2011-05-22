(ns Clojure.monopoly
  (:require [clojure.contrib.string])
  (:require [clojure.contrib.seq-utils])
  (:require [clojure.contrib.math])
  (alias string clojure.contrib.string)
  (alias math clojure.contrib.math)
  (alias sequtils clojure.contrib.seq-utils)
  (:import [java.util Random]))

(defn random [top] (.nextInt (java.util.Random.) top))

(def board `("GO" "A1" "CC1" "A2" "T1" "R1" "B1" "CH1" "B2" "B3"
             "JAIL" "C1" "U1" "C2" "C3" "R2" "D1" "CC2" "D2" "D3"
             "FP" "E1" "CH2" "E2" "E3" "R3" "F1" "F2" "U2" "F3"
             "G2J" "G1" "G2" "CC3" "G3" "R4" "CH3" "H1" "T2" "H2"))

(defn communityMove [x]
  (nth [0 10 x x x x x x x x x x x x x x] (random 16)))

(defn nextR [x] (+ (* (unchecked-divide (rem (+ x 5) 40) 10) 10) 5))
(defn nextU [x] (if (and (> x 12) (< x 28)) 28 12))

(defn chanceMove [x]
  (nth [0 10 11 24 39 5 (nextR x) (nextR x) (nextU x) (- x 3) x x x x x x] (random 16)))

(def topRoll 4)
(defn roll []
  (letfn [(auxRoll [acc times]
                   (if (> times 3)
                     :bust
                     (let [r1 (inc (random topRoll)) r2 (inc (random topRoll))]
                       (if (= r1 r2)
                         (auxRoll (+ acc r1 r2) (inc times))
                         (+  acc r1 r2)))))]
         (auxRoll 0 1)))

(defn incPos [[pos & tail] acc]
  (if pos
    (recur tail (assoc acc pos (inc (nth acc pos))))
    acc))

(defn move [x d]
  (if (= d :bust)
    [10]
    (let [nx (rem (+ x d) 40) bf (nth board nx)]
	    (cond
	      (= (subs bf 0 2) "CC") [(communityMove nx)]
	      (= (subs bf 0 2) "CH") (let [nnx (chanceMove nx) nbf (nth board nnx)]
                                  (if (= (subs nbf 0 2) "CC") [(communityMove nnx)] [nnx]))
	      (= bf "G2J") [10]
	      :else [nx]))))

(defn calc [iters]
  (loop [pos 0 acc (vec (map (fn [_] 0) board)) it iters]
    (if (= it 0)
      acc
      (let [nextMoves (move pos (roll))]
        (recur (last nextMoves) (incPos nextMoves acc) (dec it))))))
