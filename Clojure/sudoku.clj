(ns sudoku
  (:require [clojure.contrib.string])
  (:require [clojure.contrib.seq-utils])
  (:require [clojure.contrib.math])
  (:require [clojure.set])
  (alias set clojure.set)
  (alias string clojure.contrib.string)
  (alias math clojure.contrib.math)
  (alias sequtils clojure.contrib.seq-utils))


(defn getValuesIn [pockets]
  (reduce (fn [acc elem] (if (= (count elem) 1) (set/union acc elem) acc)) #{} pockets))

(defn getNoOfSetPockets [pockets]
  (count (filter #(= (count %) 1) pockets)))

(defn getColElems [c riddle]
  (vec (map #(get % c) riddle)))

(defn getSqElems [s riddle]
  (let [getRow (fn [s e] (+ (* (unchecked-divide s 3) 3) (unchecked-divide e 3)))
        getCol (fn [s e] (+ (* (rem s 3) 3) (rem e 3)))]
    (map #(get (get riddle (getRow s %)) (getCol s %)) (range 9))))

(defn getPossibleElems [r c riddle]
  (if (= (count (get (get riddle r) c)) 1)
    (get (get riddle r) c)
    (set/difference
      (set (range 1 10))
     	(getValuesIn (get riddle r))
     	(getValuesIn (getColElems c riddle))
     	(getValuesIn (getSqElems (+ (* (unchecked-divide r 3) 3) (unchecked-divide c 3)) riddle)))))

(defn getSmallestOption [riddle]
  (loop [[option optRow optCol] [nil -1 -1] r 0 c 0]
    (if (> r 8)
      [option optRow optCol]
      (let [cand (get (get riddle r) c)] 
        (recur (if (or (= (count cand) 1) (and option (<= (count option) (count cand))))
                 [option optRow optCol]
                 [cand r c])
               (+ r (unchecked-divide (inc c) 9))
               (rem (inc c) 9))))))
(defn replaceElem [riddle r c newElem]
  (assoc riddle r (assoc (get riddle r) c newElem)))

(defn getNewRiddle [riddle]
  (loop [r 0 c 0 newRiddle riddle]
    (if (> r 8)
      newRiddle
      (recur (+ r (unchecked-divide (inc c) 9))
             (rem (inc c) 9)
             (replaceElem newRiddle r c (getPossibleElems r c riddle))))))

(defn findTrivial [riddle]
  (loop [r riddle prevRiddle []]
    (if (= r prevRiddle)
      r
      (recur (getNewRiddle r) r))))

(defn isOk [riddle]
  (and (every? #(= (count (getValuesIn (get riddle %))) (getNoOfSetPockets (get riddle %)))
               (range 9))
       (every? #(let [colElems (getColElems % riddle)]
                  (= (count (getValuesIn colElems)) (getNoOfSetPockets colElems)))
               (range 9))
       (every? #(let [squareElems (getSqElems % riddle)]
                  (= (count (getValuesIn squareElems)) (getNoOfSetPockets squareElems)))
               (range 9))))

(defn printRow [row]
  (if (first row)
     (let [e (first row)]
       (if (= (count e) 1) (print (first e)) (print "0"))
       (printRow (rest row)))
   nil))

(defn printRiddle [riddle]
  (if (first riddle)
    (let [] (printRow (first riddle)) (println)(printRiddle (rest riddle)))))

(defn trySolve [riddle [option r c]]
  (if (= option nil)
    riddle
    (if (= option #{})
      nil
      (loop [opts option]
        (if (first opts)
	        (let [partialSolution (findTrivial (replaceElem riddle r c #{(first opts)}))]
           (let [solution (trySolve partialSolution (getSmallestOption partialSolution))]
             (if (and solution (isOk solution)) solution (recur (rest opts)))))
         nil)))))

(def counter 0)

(defn solve [riddle]
  (def counter (inc counter))
  (println "riddle no:" counter)
  (printRiddle riddle)
  (let [partialSolution (findTrivial riddle)]
    (let [solution (trySolve partialSolution (getSmallestOption partialSolution))]
      (println "solution no: " counter)
      (printRiddle solution)
      solution)))

(defn getValue [riddle r c]
  (first (get (get riddle r) c)))

(defn getSolutionValue [riddle]
  (+ (* (getValue riddle 0 0) 100) (* (getValue riddle 0 1) 10) (getValue riddle 0 2)))

(defn solveAll [riddles]
  (reduce (fn [acc, riddle] (+ acc (getSolutionValue (solve riddle)))) 0 riddles))

(defn readSudokus [file]
  (let [raw (filter #(not (re-matches #"^Grid.*" %)) (string/split-lines (slurp file)))
        reduceLine (fn [acc elem] (if (= elem "")
                                acc
                                (let [n (Integer/parseInt elem)]
                                  (conj acc (set (if (= n 0) (range 1 10) [n]))))))]
    (map #(vec %)
         (partition-all 9 (vec (map (fn [line] (reduce reduceLine [] (string/split #"" line))) raw))))))


