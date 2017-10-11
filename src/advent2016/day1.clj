(ns advent2016.day1
  (:require
   [clojure.spec.alpha :as s]
   [medley.core :as medley]
   [madstap.comfy :as comfy]
   [clojure.string :as str]))

(defn parse [x]
  (->> (re-seq #"([LR])(\d+)" x)
       (map (fn [[_ dir n]]
              [({"L" :left,
                 "R" :right} dir) (comfy/str->int n)]))))

(s/def ::pos
  (s/and vector? (s/cat :x int? :y int?)))

(s/def ::cardinal #{:north :south :east :west})

(s/def ::direction #{:right :left})

(def cardinals
  {:north [0 1], :south [0 -1], :east [1 0], :west [-1 0]})

(s/def ::turns
  (s/map-of ::cardinal
            (s/map-of ::direction ::cardinal)))

(def turns
  {:north {:left :west
           :right :east}
   :south {:left :east
           :right :west}
   :west {:left :south
          :right :north}
   :east {:left :north
          :right :south}})

(assert (s/valid? ::turns turns))

(defn turn [cardinal direction]
  (get-in turns [cardinal direction]))

(defn step [pos cardinal]
  (mapv + pos (cardinals cardinal)))

(defn steps [pos cardinal n]
  (nth (iterate #(step % cardinal) pos) n))

(defn go [{:keys [pos cardinal]} [direction distance]]
  (let [new-cardinal (turn cardinal direction)]
    {:cardinal new-cardinal
     :pos (steps pos new-cardinal distance)}))

(def start
  {:pos [0 0]
   :cardinal :north})

(defn distance
  ([to] (distance [0 0] to))
  ([from to]
   (->> (map - from to) (map medley/abs) (apply +))))

(defn solve-part1 [path-s]
  (->> (parse path-s) (reduce go start) (:pos) (distance)))

(comment

  (= 5 (solve-part1 "R2, L3"))

  (= 2 (solve-part1 "R2, R2, R2"))

  (= 12 (solve-part1 "R5, L5, R5, R3"))

  (solve-part1 (slurp "resources/day1.txt")) ;=> 300

  )
