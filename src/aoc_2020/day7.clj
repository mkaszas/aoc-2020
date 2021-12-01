(ns aoc-2020.day7
  (:require [clojure.string :as str]))

(defn get-input [] (slurp "input/7"))

(defn parse-bag-contents [contents]
  (if (= contents "no other bags")
    []
    (->> (str/split contents #",")
         (map #(str/replace % #"(\d+) ([\w\ ]+) bags?" "[\"$2\" $1]"))
         (map read-string)
         (into {}))))

(defn parse [get-input]
  (->> (get-input)
       str/split-lines
       (map #(-> %
                 (str/replace #"([\w\ ]+) bags? contain ([\w\ \,]+)." "[\"$1\" \"$2\"]")
                 read-string
                 ((fn [[key contents]] [key (parse-bag-contents contents)]))))
       (into {})))



(defn can-contain? [input contents]
  (delay
    (some true?
          (map #(if (= % "shiny gold")
                  true
                  @(input %))
               (keys contents)))))

(def bags-map
  (into
    {}
    (map
      #(vector (first %) (can-contain? bags-map (second %)))
      (parse get-input))))


(def bags-map-b
  (into
    {}
    (map
      (fn [[name contents]]
        [name (delay
                (inc (reduce +
                      (map
                        (fn [[k v]]
                          (* v @(bags-map-b k)))
                        contents))))])
      (parse get-input))))


(defn a []
  (time
    (->> bags-map
         (filter #(deref (second %)))
         count)))

(defn b []
  (time
    (dec @(bags-map-b "shiny gold"))))
