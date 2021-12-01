(ns aoc-2020.day1
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn get-input [] (slurp "input/1"))


(defn parse [get-input]
  (->> (get-input)
       str/split-lines
       (map read-string)))

(defn filter-nums [input n]
  (let [min-sum (apply + (take (dec n) (sort input)))
        max-sum (apply + (take (dec n) (reverse (sort input))))]
    (->> input
         (filter #(not (< 2020 (+ min-sum %))))
         (filter #(not (> 2020 (+ max-sum %)))))))

(defn calc [input n]
  (->> (combo/combinations (filter-nums input n) n)
       (filter #(= 2020 (apply + %)))
       first
       (apply *)))

(defn a []
  (time (calc (parse get-input) 2)))

(defn b []
  (time (calc (parse get-input) 3)))



(defn mymap [f coll]
  (if (empty? coll)
    ()
    (cons (f (first coll))
          (mymap f (rest coll)))))