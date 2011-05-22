(ns originWithinTrinagles
  (:require [clojure.contrib.string])
  (:require [clojure.contrib.seq-utils])
  (:require [clojure.contrib.math])
  (alias string clojure.contrib.string)
  (alias math clojure.contrib.math)
  (alias sequtils clojure.contrib.seq-utils))

(defn calcSide [[x1 y1] [x2 y2] [px py]]
  (compare (- (* (- py y1) (- x2 x1)) (* (- px x1) (- y2 y1))) 0))

(defn originInside [p1 p2 p3]
  (let [sides [(calcSide p1 p2 [0 0]) (calcSide p2 p3 [0 0]) (calcSide p3 p1 [0 0])]]
    (or (every? #(>= % 0) sides) (every?  #(<= % 0) sides))))


(defn convertTriangleLine [line]
  (let [[x1 y1 x2 y2 x3 y3] (map #(Integer/parseInt %) (.split #"," line))]
    [[x1 y1] [x2 y2] [x3 y3]]
  ))

(defn readTriangles [file]
  (map #(convertTriangleLine %) (string/split-lines (slurp file))))

(defn countWithOrigin [triangles]
  (loop [trs triangles acc 0]
    (println (first trs))
    (if (first trs)
      (recur (rest trs) (+ acc (if (apply originInside (first trs)) 1 0)))
      acc)))
