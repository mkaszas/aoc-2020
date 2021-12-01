(ns aoc-2020.day22
  (:require [clojure.string :as str]))

(defn get-input [] (slurp "input/22"))

(def test-input
"Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10")

(defn parse [input]
  (->> (str/split input #"\n\n")
       (map #(map read-string (drop 1 (str/split-lines %))))))

(defn score [cards]
  (reduce + (map * (reverse cards) (iterate inc 1))))

(defn play [player-1 player-2]
  (if (or (empty? player-1) (empty? player-2))
    (score (concat player-1 player-2))
    (let [[head-1 tail-1] (split-at 1 player-1)
          [head-2 tail-2] (split-at 1 player-2)]
      (if (> (first head-1) (first head-2))
        (play (concat tail-1 head-1 head-2) tail-2)
        (play tail-1 (concat tail-2 head-2 head-1))))))



(defn a []
  (time
    (->> (get-input)
         parse
         (apply play))))


(declare play-game)

(defn round-winner [[head-1 tail-1] [head-2 tail-2]]
  (if (and (>= (count tail-1) head-1) (>= (count tail-2) head-2))
    (first (play-game (take head-1 tail-1) (take head-2 tail-2)))
    (if (> head-1 head-2) 1 2)))


(defn play-game [p1 p2]
  (loop [player-1 p1
         player-2 p2
         seen-starts #{}]
   (cond
    (empty? player-2) [1 (score player-1)]
    (empty? player-1) [2 (score player-2)]
    :else
    (let [[head-1 tail-1] (split-at 1 player-1)
          [head-2 tail-2] (split-at 1 player-2)
          hash-state [player-1 player-2]
          next-seen-starts (conj seen-starts hash-state)]
      (if (contains? seen-starts hash-state)
        [1 0]
        (if (= 1 (round-winner [(first head-1) tail-1] [(first head-2) tail-2]))
          (recur (concat tail-1 head-1 head-2) tail-2 next-seen-starts)
          (recur tail-1 (concat tail-2 head-2 head-1) next-seen-starts)))))))


(defn b []
  (time
    (->> (get-input)
         parse
         (apply play-game))))