(ns aoc-2020.day21
  (:require [clojure.set :as set]
            [instaparse.core :as insta]
            [clojure.string :as str]))


(defn get-input [] (slurp "input/21"))


(def p-rules
  "S = (I W?)+ #'\\(' C W (A #','? W?)+ #'\\)'\nI = #'\\w+'\nA = #'\\w+'\nW = ' '\nC = 'contains'")

(def parser (insta/parser p-rules))

(defn parse-rule [rule]
  (let [s (parser rule)
        segments (filter vector? s)
        ingredients (keep #(if (= :I (first %)) (second %)) segments)
        allergens (keep #(if (= :A (first %)) (second %)) segments)]
    [(into #{} ingredients) (into #{} allergens)]))

(defn parse [input]
  (->> (str/split-lines input)
       (map parse-rule)
       (into {})))

(defn build-map [ingredients]
  (->> (vals ingredients)
       (reduce set/union)
       (map #(vector % (apply set/intersection (keep (fn [[k v]] (if (contains? v %) k)) ingredients))))
       (into {})))


(defn resolve-allergens [resolved not-resolved]
  (if (= 0 (count not-resolved))
    resolved
    (let [sure (filter #(= 1 (count (second %))) not-resolved)
          rest (filter #(not= 1 (count (second %))) not-resolved)
          new-resolved (reduce #(assoc %1 (first %2) (first (second %2))) resolved sure)
          taken-vals (into #{} (vals new-resolved))
          new-not-resolved (map (fn [[k v]] (vector k (set/difference v taken-vals))) rest)]
      (resolve-allergens new-resolved new-not-resolved))))


(defn a []
  (time
    (let [input (parse (get-input))
          allergen-ingredients (->> input
                                    build-map
                                    (resolve-allergens {})
                                    vals
                                    (into #{}))]
      (->> input
           keys
           (mapcat identity)
           (filter #(not (allergen-ingredients %)))
           count))))


(defn b []
  (time
    (->> (parse (get-input))
         build-map
         (resolve-allergens {})
         (sort-by first)
         (map second)
         (str/join ","))))