(ns aoc-2020.day5
  (:require [clojure.string :as str]))

(defn get-input [] (slurp "input/5"))

(defn parse [get-input]
  (->> (get-input)
       (#(str/replace % #"F|L" "0"))
       (#(str/replace % #"B|R" "1"))
       str/split-lines
       (map #(Integer/parseInt % 2))))

(defn a []
  (time
    (apply max (parse get-input))))


(defn b []
  (time
    (let [ids (sort (parse get-input))
          min-id (first ids)
          max-id (last ids)
          sum-of-all-ids (/ (* (- (inc max-id) min-id) (+ min-id max-id)) 2) ; Gauss formula
          ]
      (- sum-of-all-ids (reduce + ids)))))