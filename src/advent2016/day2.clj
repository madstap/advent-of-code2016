(ns advent2016.day2
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]))

(def keypad
  [[1 2 3]
   [4 5 6]
   [7 8 9]])

(def directions
  {:up   [-1 0],  :down [1 0]
   :left [0 -1], :right [0 1]})

(s/def ::direction
  #{:up :down :left :right})

(defn parse [s]
  (->> (str/split-lines s)
       (map (partial keep {\U :up, \D :down, \L :left, \R :right}))))

(def start [1 1])

(assert (= 5 (get-in keypad start)))

(defn key-exists? [coord]
  (boolean (get-in keypad coord)))

(defn move [coord direction]
  (let [new-coord (mapv + coord (directions direction))]
    (if (key-exists? new-coord) new-coord coord)))

(defn solve-part1 [input]
  (let [coords (reduce (fn [acc line]
                         (conj acc (reduce move (or (last acc) start) line)))
                       []
                       (parse input))]
    (apply str (mapv (partial get-in keypad) coords))))

(comment

  (def example
    "ULL
  RRDDD
  LURDL
  UUUUD")

  (= "1985" (solve-part1 example))

  (solve-part1 (slurp "resources/day2.txt")) ;=> "56983"

  )
