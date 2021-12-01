(ns aoc-2020.day2
  (:require [clojure.string :as str]))


(defn get-input [] (slurp "input/2"))

(defn parse [get-input]
  (->> (get-input)
       str/split-lines
       (map #(str/replace % #"(\d+)-(\d+) ([a-z]): (\w+)$" "{:min $1 :max $2 :chr \"$3\" :pass \"$4\"}"))
       (map read-string)))

(defn check-pass-a [{:keys [min max chr pass]}]
  (let [c (count (filter #(= % (first chr)) pass))]
    (and (>= max c) (<= min c))))

(defn check-pass-b [{:keys [min max chr pass]}]
  (let [fst (if (= (first chr) (nth pass (- min 1))) 1 0)
        snd (if (= (first chr) (nth pass (- max 1))) 1 0)]
    (= 1 (+ fst snd))))

(defn solve [check-pass]
  (->> (parse get-input)
       (map check-pass)
       (filter identity)
       count))

(defn a []
  (time (solve check-pass-a)))

(defn b []
  (time (solve check-pass-b)))