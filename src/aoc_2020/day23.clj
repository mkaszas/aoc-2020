(ns aoc-2020.day23
  (:require [clojure.string :as str]))

(defn get-input [] (slurp "input/23"))
(def test-input "389125467")

(defn parse [input] (map read-string (str/split input #"")))



(defn find-destination [selected others]
  (loop [n selected
         l others]
    (if (< n 0)
      (apply max l)
      (or
        (some #{n} l)
        (recur (dec n) l)))))

(defn next-round [input]
  (let [length (count input)
        starting (first input)
        picked-up (->> (cycle input)
                       (drop 1)
                       (take 3))
        others (->> (cycle input)
                    (drop 4)
                    (take (- length 4)))
        destination (find-destination (dec starting) others)
        splitted (partition-by #{destination} others)
        [before-dest after-dest] [(apply concat (drop-last splitted)) (last splitted)]
        inserted (concat before-dest picked-up after-dest)]
    (concat inserted [starting])))





(->> test-input
     parse
     (iterate next-round)
     ;(take 102)
     (#(nth % 100))

     )