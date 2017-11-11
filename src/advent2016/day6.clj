(ns advent2016.day6
  (:require
   [clojure.string :as str]))

(def input (-> (slurp "resources/day6.txt") (str/split-lines)))

(def example
  (-> "eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar"
      (str/split-lines)))

(defn solve-part1 [inp]
  (transduce (comp (map frequencies)
                   (map (partial apply max-key val))
                   (map key))
             str
             (apply map vector inp)))

(comment

  (= "easter" (solve-part1 example))

  (solve-part1 input) ;=> "umcvzsmw"

  )
