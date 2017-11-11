(ns advent2016.day5
  (:require
   [clojure.string :as str]
   [medley.core :as medley])
  (:import
   (java.security MessageDigest)
   (java.math BigInteger)))

(def input "ffykfhsq")

;; https://gist.github.com/jizhang/4325757
(defn md5 [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

;; For that hacking-movie one-character-at-a-time effect
(defn p< [x] (println x) x)

(defn solve-part1 [inp]
  (->> (range)
       (keep (fn [n]
               (let [hash (md5 (str inp n))]
                 (when (str/starts-with? hash "00000")
                   (p< (nth hash 5))))))
       (take 8)
       (apply str)))

(comment

  (= "18f47a30" (time (solve-part1 "abc")))

  (time (solve-part1 input)) ;=> "c6697b55"

  )
