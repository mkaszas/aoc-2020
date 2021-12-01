(ns aoc-2020.day11
  (:require [clojure.string :as str]))

(defn get-input [] (slurp "input/11"))

(defn parse [get-input]
  (->> (get-input)
       str/split-lines
       (map #(-> % (str/replace #"L" " :empty ") (str/replace #"\." " :floor ")))
       (map #(format "[%s]" %))
       (map read-string)))


(def neighbour-offsets
  (let [digits (range -1 2)]
    (for [x digits
          y digits
          :when (not= [0 0] [x y])]
      [x y])))

(defn get-val [board [x y]]
  (-> board
      (nth x nil)
      (nth y nil)))


(defn find-seat [board c offset]
  (let [coord (map + c offset)
        value (get-val board coord)]
    (if (= :floor value)
      (find-seat board coord offset)
      value)))

(defn neighbours [board c]
  (->> neighbour-offsets
       (map #(find-seat board c %))))

(defn next-val [board c]
  (case (get-val board c)
    :floor :floor
    :empty (if (not-any? #{:occupied} (neighbours board c)) :occupied :empty)
    :occupied (if (<= 5 (count (filter #{:occupied} (neighbours board c)))) :empty :occupied)))

(defn next-board [board]
  (->> (for [x (range 0 (count board))
             y (range 0 (count (first board)))]
         [x y])
       (partition (count (first board)))
       (map #(map (fn [c] (next-val board c)) %))))

(defn stabilize [board]
  (let [next (next-board board)]
    (if (= board next)
      board
      (stabilize next))))

(defn a []
  (time
    (->> (parse get-input)
         stabilize
         (mapcat #(filter #{:occupied} %))
         count)))



















