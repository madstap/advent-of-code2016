(ns advent2016.day3
  (:require
   [clojure.math.combinatorics :as combo]
   [madstap.comfy :as comfy]
   [clojure.string :as str]))

(defn parse [s]
  (->> (str/split-lines s)
       (map (partial re-seq #"\d+"))
       (map (partial map comfy/str->int))
       (map sort)))

(def input
  (parse (slurp "resources/day3.txt")))

(def example [5 10 25])

(defn valid-triangle? [triangle]
  (every? (fn [[a b c]]
            (> (+ a b) c)) (combo/permutations triangle)))

(defn solve-part1 [input]
  (count (filter valid-triangle? input)))

(comment

  (false? (valid-triangle? example))

  (solve-part1 input) ;=> 1032

  )
