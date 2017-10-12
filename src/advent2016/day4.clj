(ns advent2016.day4
  (:require
   [medley.core :as medley]
   [madstap.comfy :as comfy]
   [clojure.set :as set]
   [clojure.string :as str]))

(defn parse-room [s]
  (-> (zipmap [:name :sector :checksum]
              (rest (re-find #"([a-z-]+)(\d+)\[([a-z]+)\]" s)))
      (update :sector comfy/str->int)))

(defn parse [s]
  (map parse-room (str/split-lines s)))

(def input
  (parse (slurp "resources/day4.txt")))

(defn five-most-common
  "Given a frequencies-map, returns the set of the 5 most common keys.
  Set may contain more than 5 elements in the case of a tie for last place."
  [freqs]
  (->> (sort-by val freqs)
       (reverse)
       (reduce (fn [acc [letter n :as pair]]
                 (if (or (> 5 (count acc))
                         (= n (val (last acc))))
                   (conj acc pair)
                   (reduced acc)))
               [])
       (map key)
       (set)))

(defn real? [{:keys [name checksum]}]
  (let [freqs (-> (frequencies name) (dissoc \-))]
    (and (set/subset? (set checksum) (five-most-common freqs))
         (= checksum (->> checksum
                          (sort-by (juxt freqs (comp - int)))
                          (reverse)
                          (apply str))))))

(defn solve-part1 [in]
  (transduce (comp (filter real?) (map :sector)) + in))

(def real-examples
  (parse
   "aaaaa-bbb-z-y-x-123[abxyz]
    a-b-c-d-e-f-g-h-987[abcde]
    not-a-real-room-404[oarel]"))

(def false-example
  (parse-room "totally-real-room-200[decoy]"))

(def all-examples (conj real-examples false-example))

(comment

  (every? real? real-examples)

  (not (real? false-example))

  (= 1514 (solve-part1 all-examples))

  (solve-part1 input) ;=> 185371

  )

(def alphabet "abcdefghijklmnopqrstuvwxyz")

(defn shift [char n]
  (-> (cycle alphabet)
      (->> (drop-while (complement #{char})))
      (nth n)))

(defn decrypt [{:keys [name sector]}]
  (->> name
       (map #(if (= \- %) \space (shift % sector)))
       (apply str)
       (str/trimr)))

(def encryption-example (parse-room "qzmt-zixmtkozy-ivhz-343[xxxxx]"))

(defn solve-part2 [input]
  (:sector (medley/find-first #(re-find #"north" (decrypt %)) input)))

(comment

  (= "very encrypted name" (decrypt encryption-example))

  (solve-part2 input) ;=> 984

  )
