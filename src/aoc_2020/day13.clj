(ns aoc-2020.day13
  (:require [clojure.string :as str]))

(defn get-input [] (slurp "input/13"))

(defn parse [get-input]
  (-> (read-string (str/replace (get-input) #"(\d+)\n([\d,x]+)\n" "{:timestamp $1 :ids [$2]}"))
      (update :ids #(remove #{'x} %))))

(defn first-bus [{:keys [timestamp ids]}]
  (->> ids
       (map #(vector % (- % (mod timestamp %))))
       (sort-by second)
       first))

(defn a []
  (time
    (->> (parse get-input)
         first-bus
         (apply *))))



(defn parse-b [get-input]
  (read-string (str/replace (get-input) #"(\d+)\n([\d,x]+)\n" "{:timestamp $1 :ids [$2]}")))

(defn abs [x]
  (if (< 0 x)
    x
    (- x)))

(defn extended-gcd
  [a b]
  (cond (zero? a) [(abs b) 0 1]
        (zero? b) [(abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (abs b)
                     r0 (abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))

(defn chinese-remainder
  [n a]
  (let [prod (apply * n)
        reducer (fn [sum [n_i a_i]]
                  (let [p (quot prod n_i)
                        egcd (extended-gcd p n_i)
                        inv_p (second egcd)]
                    (+ sum (* a_i inv_p p))))
        sum-prod (reduce reducer 0 (map vector n a))]
    (mod sum-prod prod)))


(defn b []
  (time
    (->> (parse-b get-input)
         :ids
         (map-indexed vector)
         (remove #(= 'x (second %)))
         (apply map list)
         (#(chinese-remainder (second %) (map - (first %)))))))