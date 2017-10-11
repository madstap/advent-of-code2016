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

(defn go [{:keys [pos cardinal]} [direction distance]]
  (let [new-cardinal (turn cardinal direction)
        positions (take distance (rest (iterate #(step % new-cardinal) pos)))]
    {:cardinal new-cardinal
     :all-positions positions
     :pos (last positions)}))

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

(defn solve-part2 [path-s]
  (let [positions (->> (parse path-s)
                       (reductions go start)
                       (mapcat :all-positions))
        multiple-visits (->> (frequencies positions)
                             (medley/filter-vals #(not= % 1)))]
    (distance (medley/find-first (partial contains? multiple-visits) positions))))

(comment

  (= 4 (solve-part2 "R8, R4, R4, R8"))

  (solve-part2 (slurp "resources/day1.txt")) ;=> 159

  )
