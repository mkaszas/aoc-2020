(ns aoc-2020.day16
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn get-input [] (slurp "input/16"))



(defn parse [get-input]
  (let [[constraints your nearby] (str/split (get-input) #"\n\n")]
    {:constraints (->> (str/split-lines constraints)
                       (map #(str/replace % #"([\w\s]+): (\d+)-(\d+) or (\d+)-(\d+)" "[\"$1\" [[$2 $3] [$4 $5]]]"))
                       (map read-string)
                       (map (fn [[k [[a b] [c d]]]] [k (concat (range a (inc b)) (range c (inc d)))]))
                       (into {}))
     :your (->> (str/split-lines your)
                second
                (format "[%s]")
                read-string)
     :nearby (->> (str/split-lines nearby)
                  (drop 1)
                  (map #(read-string (format "[%s]" %))))}))



(defn invalid-fields [ticket constraints]
  (filter #(not-any? #{%} (apply concat (vals constraints))) ticket))

(defn a []
  (time
    (let [input (parse get-input)]
      (reduce + (mapcat #(invalid-fields % (input :constraints)) (input :nearby))))))


(defn valid-tickets [{:keys [constraints your nearby]}]
  (->> nearby
       (filter (fn [l] (every? #(some #{%} (apply concat (vals constraints))) l)))
       (cons your)))

(defn valid-field-names [constraints values]
  (->> constraints
       (keep (fn [[k v]] (if (every? (into #{} v) values) k)))))

(defn find-order [possible-keys]
  (->> (map-indexed vector possible-keys)
       (sort-by #(count (second %)))
       (reduce (fn [m [i v]] (assoc m i (first (filter #(not-any? #{%} (vals m)) v)))) (sorted-map))
       vals))


(defn b []
  (time
    (let [input (parse get-input)
          your-ticket (input :your)
          ticket-values (apply mapv vector (valid-tickets input))]
      (->> ticket-values
           (map #(valid-field-names (input :constraints) %))
           find-order
           (keep-indexed #(if (str/includes? %2 "departure") (nth your-ticket %1)))
           (reduce *)))))