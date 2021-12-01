(ns aoc-2020.day8
  (:require [clojure.string :as str]))

(defn get-input [] (slurp "input/8"))

(defn parse [get-input]
  (->> (get-input)
       str/split-lines
       (map #(read-string (format "[:%s]" %)))))

(defn run [instructions]
  (let [acc (atom 0)
        i (atom 0)
        visited-i (atom #{})
        terminate-index (count instructions)]
    (do
      (while (not (or (contains? @visited-i @i) (= @i terminate-index)))
        (let [[cmd arg] (nth instructions @i)]
          (do
            (swap! visited-i conj @i)
            (if (= cmd :acc) (swap! acc + arg))
            (if (= cmd :jmp) (swap! i + arg) (swap! i inc)))))
    [(if (= @i terminate-index) :term :loop) @acc])))


(defn a []
  (time (second (run (parse get-input)))))

(defn b []
  (time
    (let [instruction-map (into (sorted-map) (map-indexed vector (parse get-input)))
          switches (filter #(not= :acc (first (second %))) instruction-map)
          switch (fn [[i [cmd arg]]] (assoc instruction-map i [(if (= :jmp cmd) :nop :jmp) arg]))
          possible-programs (map switch switches)]
      (->> possible-programs
           (map #(run (vals %)))
           (filter #(= :term (first %)))
           first
           second))))