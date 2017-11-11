(ns advent2016.day7
  (:require
   [clojure.string :as str]))

(def input (-> (slurp "resources/day7.txt") (str/split-lines)))

(defn parse [x]
  (let [xf (comp (map second) (remove str/blank?))]
    {:hypernet (into [] xf (re-seq #"\[([a-z]*)\]" x))
     :ip (into [] xf (cons (re-find #"([a-z]*)\[" x) (re-seq #"\]([a-z]*)" x)))}))

(defn abba? [[a b c d]]
  (and (= a d) (= b c) (not= a b)))

(defn contains-abba? [s]
  (boolean (some abba? (partition 4 1 s))))

(defn supports-tls? [{:keys [hypernet ip]}]
  (and (some contains-abba? ip) (not (some contains-abba? hypernet))))

(def true-examples (map parse ["abba[mnop]qrst" "ioxxoj[asdfgh]zxcvbn"]))

(def false-examples (map parse ["abcd[bddb]xyyx" "aaaa[qwer]tyui"]))

(defn solve-part1 [inp]
  (count (filter supports-tls? (map parse inp))))

(comment

  (and
   (every? supports-tls? true-examples)
   (every? (complement supports-tls?) false-examples))

  (solve-part1 input) ;=> 110

  )
