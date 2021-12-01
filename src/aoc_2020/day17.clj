(ns aoc-2020.day17
  (:require [clojure.string :as str]))

(defn get-input [] (slurp "input/17"))


(defn parse [get-input]
  (->> (get-input)
       str/split-lines
       (map-indexed vector)
       (mapcat (fn [[x v]] (map-indexed (fn [y val] (vector [x y 0] val)) v)))
       (filter #(= \# (second %)))
       (map first)
       (into (sorted-set))))

(def neighbour-offsets
  (let [digits (range -1 2)]
    (for [x digits
          y digits
          z digits
          :when (not= [0 0 0] [x y z])]
      [x y z])))

(defn minmax [l] [(dec (apply min l)) (inc (apply max l))])

(defn coords-to-check [active-coords]
  (let [[[min-x max-x] [min-y max-y] [min-z max-z]] (map minmax (apply mapv vector active-coords))]
    (for [x (range min-x (inc max-x))
          y (range min-y (inc max-y))
          z (range min-z (inc max-z))]
      [x y z])))



(defn active-next? [state coords]
  (let [active-n (->> (map #(mapv + coords %) neighbour-offsets)
                      (filter #(contains? state %))
                      count)
        active? (contains? state coords)]
    (if active?
      (or (= 2 active-n) (= 3 active-n))
      (= 3 active-n))))


(defn next-state [s]
  (->> (coords-to-check s)
       (keep #(if (active-next? s %) %))
       (into (sorted-set))))

(defn a []
  (time
    (-> (iterate next-state (parse get-input))
        (nth 6)
        count)))





;;; ---- Part B

(defn parse-b [get-input]
  (->> (get-input)
       str/split-lines
       (map-indexed vector)
       (mapcat (fn [[x v]] (map-indexed (fn [y val] (vector [x y 0 0] val)) v)))
       (filter #(= \# (second %)))
       (map first)
       (into (sorted-set))))

(def neighbour-offsets-b
  (let [digits (range -1 2)]
    (for [x digits
          y digits
          z digits
          w digits
          :when (not= [0 0 0 0] [x y z w])]
      [x y z w])))

(defn coords-to-check-b [active-coords]
  (let [[[min-x max-x] [min-y max-y] [min-z max-z] [min-w max-w]] (map minmax (apply mapv vector active-coords))]
    (for [x (range min-x (inc max-x))
          y (range min-y (inc max-y))
          z (range min-z (inc max-z))
          w (range min-w (inc max-w))]
      [x y z w])))

(defn active-next?-b [state coords]
  (let [active-n (->> (map #(mapv + coords %) neighbour-offsets-b)
                      (filter #(contains? state %))
                      count)
        active? (contains? state coords)]
    (if active?
      (or (= 2 active-n) (= 3 active-n))
      (= 3 active-n))))

(defn next-state-b [s]
  (->> (coords-to-check-b s)
       (keep #(if (active-next?-b s %) %))
       (into (sorted-set))))


(defn b []
  (time
    (-> (iterate next-state-b (parse-b get-input))
        (nth 6)
        count)))

