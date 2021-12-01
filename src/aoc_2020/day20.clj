(ns aoc-2020.day20
  (:require [clojure.string :as str]))

(defn get-input [] (slurp "input/20"))


(defn parse [input]
  (->> (str/split input #"\n\n")
       (map str/split-lines)
       (map #(vector
               (read-string (str/replace (first %) #"Tile (\d+):" "$1"))
               (mapv (fn [s] (apply vector s)) (drop 1 %))))
       (into {})))

(defn rotate [mat] (apply mapv vector (reverse mat)))

(defn flip [mat] (mapv #(apply vector (reverse %)) mat))

(defn orient [mat [rotations flips]]
  (let [a-mat (atom mat)]
    (do
      (dotimes [_ rotations]
        (swap! a-mat rotate))
      (dotimes [_ flips]
        (swap! a-mat flip))
      @a-mat)))

(defn all-orientations [mat]
  (let [os (for [r [0 1 2 3]
                 f [0 1]]
             [r f])]
    (map #(orient mat %) os)))

(def sides [:top :right :bottom :left])

(defn edge [mat side]
  (case side
    :top (first mat)
    :right (last (apply mapv vector mat))
    :bottom (last mat)
    :left (first (apply mapv vector mat))))

(defn opposite-side [side]
  (case side
    :top :bottom
    :right :left
    :bottom :top
    :left :right))

(defn match [mat-1 mat-2 side]
  (let [edge-to-match (edge mat-1 side)]
    (->> (all-orientations mat-2)
         (filter #(= edge-to-match (edge % (opposite-side side))))
         first)))

(defn neighbours [mat tiles]
  (filter
    (fn [[_ tile]]
      (first (filter #(match mat tile %) sides)))
    tiles))

(defn a []
  (time
    (->> (parse (get-input))
         (#(map (fn [[id t]] (vector id (count (neighbours t %)))) %))
         (filter #(= 3 (second %)))
         (take 4)
         (map first)
         (reduce *))))


(defn b-input []
  (->> (slurp "input/20b")
     read-string
     (map (fn [[k v]] (vector k (filter #(not= k %) v))))
     (into {})))

(defn neighbour-orientation [mat-1 mat-2]
  (filter #(match mat-1 mat-2 %) sides))

(defn next-mat [prev-m side neighbours]
  (first (keep #(match prev-m % side) neighbours)))

(defn first-row [corner ])

(defn b []
  (let [tile-map (parse (get-input))
        neighbour-map (b-input)
        first-corner (first (filter #(= 2 (count (second %))) neighbour-map))
        corner (flip (get tile-map (first first-corner)))]
    [corner (next-mat corner :right (map tile-map (second first-corner)))]))
















































