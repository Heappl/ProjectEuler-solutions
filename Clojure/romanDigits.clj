(ns romanDigits
  (:require [clojure.string])
  (:require [clojure.contrib.seq-utils])
  (:require [clojure.contrib.math])
  (alias string clojure.string)
  (alias math clojure.contrib.math)
  (alias sequtils clojure.contrib.seq-utils))

(def romanNumbers
  (hash-map 1 "I", 4 "IV", 5 "V", 9 "IX", 10 "X", 40 "XL", 50 "L", 90 "XC", 100 "C",
            400 "CD", 500 "D", 900 "CM", 1000 "M"))
(def romanSymbols
  (hash-map "I" 1, "IV" 4, "V" 5, "IX" 9, "X" 10, "XL" 40, "L" 50, "XC" 90, "C" 100,
            "CD" 400, "D" 500, "CM" 900, "M" 1000))
(def sortedRomanSymbols
  ["CM", "CD", "XC", "XL", "IX", "IV", "M", "D", "C", "L", "X", "V", "I"])

(defn toRoman [x]
  (if (= x 0) "" 
    (let [nextVal (sequtils/find-first #(<= % x) (reverse (sort (keys romanNumbers))))]
      (str (get romanNumbers, nextVal) (toRoman (- x nextVal))))))

(defn fromRoman [r]
  (if (= r "") 0
    (let [nextSubstr (re-find (re-pattern (string/join "|" sortedRomanSymbols)) r)]
      (+ (get romanSymbols nextSubstr) (fromRoman (string/drop (.length nextSubstr) r))))))

(defn readRoman [file]
  (apply + (map #(- (.length %) (.length (toRoman (fromRoman %))))
                (string/split-lines (slurp file)))))

