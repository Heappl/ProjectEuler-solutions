(ns Clojure.coinPiles
  (:require [clojure.contrib.string :as string])
  (:require [clojure.contrib.seq-utils :as sequtils])
  (:require [clojure.contrib.math :as math])
  (:require [clojure.set :as set]))

(defn countCurrentPiles [x seen]
  (loop [i 2 acc 0]
    (if (> i (math/sqrt x))
      (+ acc (seen x) ())
      (let [nx (int (/ x i)) nnx (dec (int (/ x nx)))
            nextAcc (+ acc (seen (dec nx)) (if (not= nnx (dec nx)) (seen nnx) 0))]
        (println x i nx nnx nextAcc)
        (recur (inc i) nextAcc)))))

(defn findNumberDivisible [x div]
  (loop [i 1 seen {0 1}]
    (if (> i x)
      [(seen x)]
      (let [v (countCurrentPiles i seen)]
        (if (= (rem v div) 0)
          i
          (recur (inc i) (assoc seen i v)))))))

