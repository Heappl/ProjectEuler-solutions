(ns Clojure.oddContinousFractions
  (:require [clojure.contrib.string :as string])
  (:require [clojure.contrib.seq-utils :as sequtils])
  (:require [clojure.contrib.math :as math])
  (:require [clojure.set :as set])
  (:require [Clojure.DiophantineEquations :as cfe]))

(defn calcOddCFE [top]
  (let [xs (filter #(not (cfe/isNaturalSquared %1)) (range 2 (inc top)))
        cfes (map #(cfe/continuedFractionExpansion %1) xs)]
    (count (filter (fn [[iexp lexp]] (not= (rem (count lexp) 2) 0)) cfes))))
