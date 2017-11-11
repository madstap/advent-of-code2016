(ns advent2016.day5
  (:require
   [clojure.string :as str]
   [madstap.comfy :as comfy])
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

(defn render [m]
  (apply str (map #(get m % "_")  (range 8))))

(defn solve-part2 [inp]
  (reduce (fn [acc n]
            (let [hash (md5 (str inp n))
                  next-entry
                  (when (str/starts-with? hash "00000")
                    (let [pos (-> hash (nth 5) (str) (comfy/str->int))]
                      (when (and (contains? (set (range 8)) pos)
                                 (not (contains? acc pos)))
                        [pos (nth hash 6)])))]
              (if next-entry
                (let [acc' (conj acc next-entry)
                      password (render acc')]
                  (println password)
                  (if (= (set (keys acc')) (set (range 8)))
                    (reduced password)
                    acc'))
                acc)))
          {}
          (range)))

(comment

  (= "05ace8e3" (time (solve-part2 "abc")))

  (time (solve-part2 input))

  )
