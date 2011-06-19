(ns Clojure.bouncyNumbers
  (:require [clojure.contrib.string :as string])
  (:require [clojure.contrib.seq-utils :as sequtils])
  (:require [clojure.contrib.math :as math])
  (:require [clojure.set :as set]))

(defn isNotBouncy [x]
  (loop [nextX (unchecked-divide x 10) dig (rem x 10) op nil]
		(if (= nextX 0)
      true
      (let [aux1 (rem nextX 10) aux2 (unchecked-divide nextX 10)]
        (if (= aux1 dig) (recur aux2 aux1 op) (if (nil? op)
                                               (recur aux2 aux1 (if (< aux1 dig) <= >=))
                                               (if (op aux1 dig) (recur aux2 aux1 op) false)))))))

(defn calc [x cnt]
  (if (>= (/ (- (dec x) cnt) (dec x)) 99/100)
    (dec x)
    (recur (inc x) (if (isNotBouncy x) (inc cnt) cnt))))