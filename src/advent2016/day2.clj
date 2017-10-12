(ns advent2016.day2
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]))

(def normal-keypad
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

(defn key-exists? [keypad coord]
  (boolean (get-in keypad coord)))

(defn move [keypad coord direction]
  (let [new-coord (mapv + coord (directions direction))]
    (if (key-exists? keypad new-coord) new-coord coord)))

(defn find-code [keypad start lines]
  (->> (reduce (fn [acc line]
                 (conj acc (reduce (partial move keypad)
                                   (or (last acc) start)
                                   line)))
               []
               lines)
       (mapv (partial get-in keypad))
       (apply str)))

(defn solve-part1 [input]
  (find-code normal-keypad [1 1] (parse input)))

(def example
  "ULL
  RRDDD
  LURDL
  UUUUD")

(comment

  (= "1985" (solve-part1 example))

  (solve-part1 (slurp "resources/day2.txt")) ;=> "56983"

  )

(def fancy-keypad
  [[nil nil  1  nil nil]
   [nil  2   3   4  nil]
   [ 5   6   7   8   9 ]
   [nil "A" "B" "C" nil]
   [nil nil "D" nil nil]])

(defn solve-part2 [input]
  (find-code fancy-keypad [2 0] (parse input)))

(comment

  (= "5DB3" (solve-part2 example))

  (solve-part2 (slurp "resources/day2.txt")) ;=> "8B8B1"

  )
