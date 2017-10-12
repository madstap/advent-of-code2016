(ns advent2016.day3
  (:require
   [clojure.math.combinatorics :as combo]
   [madstap.comfy :as comfy]
   [clojure.string :as str]))

(defn parse [s]
  (->> (str/split-lines s)
       (map (partial re-seq #"\d+"))
       (map (partial map comfy/str->int))))

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

(defn re-order-vertically [triangles]
  (->> triangles (apply mapcat vector) (partition 3)))

(defn solve-part2 [input]
  (solve-part1 (re-order-vertically input)))

(comment

  (def ordering-example
    (parse
     "101 301 501
      102 302 502
      103 303 503
      201 401 601
      202 402 602
      203 403 603"))

  (= (re-order-vertically ordering-example)
     (map #(take 3 (iterate inc %)) (range 101 602 100)))

  (solve-part2 input) ;=> 1838

  )
