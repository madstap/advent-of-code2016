(ns advent2016.day7
  (:require
   [clojure.string :as str]
   [clojure.math.combinatorics :as combo]))

(defn parse [x]
  (let [xf (comp (map second) (remove str/blank?))]
    {:hypernet (into [] xf (re-seq #"\[([a-z]*)\]" x))
     :supernet (into [] xf (cons (re-find #"([a-z]*)\[" x) (re-seq #"\]([a-z]*)" x)))}))

(def input (-> (slurp "resources/day7.txt")
               (str/split-lines)
               (->> (map parse))))

(defn abba? [[a b c d]]
  (and (= a d) (= b c) (not= a b)))

(defn contains-abba? [s]
  (boolean (some abba? (partition 4 1 s))))

(defn supports-tls? [{:keys [hypernet supernet]}]
  (and (some contains-abba? supernet) (not (some contains-abba? hypernet))))

(def true-examples1 (map parse ["abba[mnop]qrst" "ioxxoj[asdfgh]zxcvbn"]))
(def false-examples1 (map parse ["abcd[bddb]xyyx" "aaaa[qwer]tyui"]))

(defn solve-part1 [inp]
  (count (filter supports-tls? inp)))

(comment

  (and
   (every? supports-tls? true-examples1)
   (every? (complement supports-tls?) false-examples1))

  (solve-part1 input) ;=> 110

  )

(defn aba? [[a b c]]
  (and (= a c) (not= a b)))

(defn bab? [[a b :as aba] [x y z]]
  (and (aba? aba) (= a y) (= b x z)))

(defn get-abas [s]
  (filter aba? (partition 3 1 s)))

(defn contains-bab? [[aba s]]
  (boolean (some (partial bab? aba) (partition 3 1 s))))

(defn supports-ssl? [{:keys [hypernet supernet]}]
  (let [abas (mapcat get-abas supernet)]
    (boolean
     (some contains-bab? (combo/cartesian-product abas hypernet)))))

(defn solve-part2 [inp]
  (count (filter supports-ssl? inp)))

(def true-examples2 (map parse ["aba[bab]xyz" "aaa[kek]eke" "zazbz[bzb]cdb"]))
(def false-examples2 (map parse ["xyx[xyx]xyx"]))

(comment

  (and
   (every? supports-ssl? true-examples2)
   (every? (complement supports-ssl?) false-examples2))

  (solve-part2 input) ;=> 242

  )
