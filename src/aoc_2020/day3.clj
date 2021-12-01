(ns aoc-2020.day3
  (:require [clojure.string :as str]))

(defn get-input [] (slurp "input/3"))

(defn parse [get-input]
  (->> (get-input)
       str/split-lines
       (map #(map (fn [c] (if (= c \#) 1 0)) %))))

(defn get-slope [[x y] input]
  (->> input
       (drop y)
       (take-nth y)
       (map-indexed
         (fn [i row]
           (nth row (mod (* x (inc i)) (count row)))))
       (reduce +)))


(defn a []
  (time (get-slope [3 1] (parse get-input))))

(defn b []
  (time (->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
             (map #(get-slope % (parse get-input)))
             (reduce *))))