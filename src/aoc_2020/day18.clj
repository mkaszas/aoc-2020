(ns aoc-2020.day18
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(defn get-input [] (slurp "input/18"))

(defn parse [get-input]
  (->> (str/split-lines (get-input))
       (map #(format "(%s)" %))))

(def operator #{'+ '*})

(s/def ::expression (s/or :number number? :group ::group))

(s/def ::group
  (s/cat :head ::expression
         :tail (s/* (s/cat :op operator :clause ::expression))))


(declare eval-group)
(defn eval-expr [[t v]]
  (case t
    :number v
    :group (eval-group v)))

(defn eval-group [{:keys [head tail]}]
  (reduce (fn [acc {:keys [op clause]}] ((eval op) acc (eval-expr clause))) (eval-expr head) tail))


(defn a []
  (time
    (->> (parse get-input)
         (map #(eval-group (s/conform ::group (read-string %))))
         (reduce +))))






(defn merge-expr [exp operation]
  [:group {:head exp :tail [operation]}])

(defn fix-order [l]
  (let [h (first l)]
    (reduce
      (fn [acc operation]
        (if (and (= '* ((last acc) :op)) (= '+ (operation :op)))
          (conj (into [] (drop-last 1 acc)) (update (last acc) :clause #(merge-expr % operation)))
          (conj acc operation))
        )
      [h]
      (drop 1 l))))


(declare eval-group-b)
(defn eval-expr-b [[t v]]
  (case t
    :number v
    :group (eval-group-b v)))

(defn eval-group-b [{:keys [head tail]}]
  (reduce
    (fn [acc {:keys [op clause]}] ((eval op) acc (eval-expr-b clause)))
    (eval-expr-b head)
    (fix-order tail)))


(defn b []
  (time
    (->> (parse get-input)
         (map #(eval-group-b (s/conform ::group (read-string %))))
         (reduce +))))