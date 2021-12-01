(ns aoc-2020.day6
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn get-input [] (slurp "input/6"))

(defn count-group [group-answers set-op]
  (->> (str/split-lines group-answers)
       (map #(into #{} (str/split % #"")))
       (reduce set-op)
       count))

(defn count-all [input set-op]
  (->> (str/split input #"\n\n")
       (map #(count-group % set-op))
       (reduce +)))

(defn a []
  (time (count-all (get-input) set/union)))

(defn b []
  (time (count-all (get-input) set/intersection)))