(ns aoc-2020.day9
  (:require [clojure.math.combinatorics :as combo]))

(defn get-input [] (slurp "input/9"))

(defn parse [get-input] (read-string (format "[%s]" (get-input))))

(defn valid? [prev n]
  (->> (combo/combinations prev 2)
       (some #(= n (reduce + %)))))

(defn first-invalid [input k]
  (->> (range 0 (count input))
       (filter #(not (valid? (drop % (take (+ % k) input)) (nth input (+ % k)))))
       first
       (+ k)
       (nth input)))


(defn a []
  (time (first-invalid (parse get-input) 25)))


(defn find-sublist [list sum]
  (if-let
    [sl (->> (map vector list (reductions + list))
               (take-while #(<= (second %) sum))
               (#(if (= sum (second (last %))) (map first %))))]
    sl
    (find-sublist (drop 1 list) sum)))

(defn b []
  (time
    (let [input (parse get-input)
          a-out (first-invalid (parse get-input) 25)
          sublist (find-sublist input a-out)]
      (+ (apply min sublist) (apply max sublist))
      )))