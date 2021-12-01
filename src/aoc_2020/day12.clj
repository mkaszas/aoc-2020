(ns aoc-2020.day12
  (:require [clojure.string :as str]))

(defn get-input [] (slurp "input/12"))

(defn parse [get-input]
  (->> (get-input)
       str/split-lines
       (map #(str/replace % #"(\w)(\d+)" "{:dir :$1 :val $2}"))
       (map read-string)))

(defn get-dir [{:keys [dir]}]
  (case (mod dir 360)
    0 :N
    90 :W
    180 :S
    270 :E))

(defn move [ship {:keys [dir val]}]
  (case dir
    :N (update ship :y #(+ % val))
    :S (update ship :y #(- % val))
    :E (update ship :x #(+ % val))
    :W (update ship :x #(- % val))
    :L (update ship :dir #(+ % val))
    :R (update ship :dir #(- % val))
    :F (move ship {:dir (get-dir ship) :val val})))

(defn end-position [commands]
  (reduce move {:x 0 :y 0 :dir 270} commands))

(defn abs [x]
  (if (< 0 x)
    x
    (- x)))

(defn manhattan-distance [{:keys [x y]}]
  (+ (abs x) (abs y)))

(defn a []
  (time
    (->> (parse get-input)
         end-position
         manhattan-distance)))

(defn rotate-waypoint [{:keys [x y]} deg]
  (case deg
    0 {:x x :y y}
    90 {:x (- y) :y x}
    180 {:x (- x) :y (- y)}
    270 {:x y :y (- x)}))

(defn move-towards-waypoint [{:keys [x y waypoint]} val]
  (let [[new-x new-y] (map + [x y] (map #(* % val) [(waypoint :x) (waypoint :y)]))]
    {:x new-x
     :y new-y
     :waypoint waypoint}))

(defn move-b [ship {:keys [dir val]}]
  (case dir
    :N (update-in ship [:waypoint :y] #(+ % val))
    :S (update-in ship [:waypoint :y] #(- % val))
    :E (update-in ship [:waypoint :x] #(+ % val))
    :W (update-in ship [:waypoint :x] #(- % val))
    :L (update ship :waypoint #(rotate-waypoint % val))
    :R (update ship :waypoint #(rotate-waypoint % (mod (- val) 360)))
    :F (move-towards-waypoint ship val)))

(defn end-position-b [commands]
  (reduce move-b {:x 0 :y 0 :waypoint {:x 10 :y 1}} commands))

(defn b []
  (time
    (->> (parse get-input)
         end-position-b
         manhattan-distance)))