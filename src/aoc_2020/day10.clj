(ns aoc-2020.day10)

(defn get-input [] (slurp "input/10"))

(defn parse [get-input] (read-string (format "[%s]" (get-input))))

(defn diffs [input]
  (map - input (conj input 0)))

(defn a []
  (time
    (->> (parse get-input)
         sort
         diffs
         frequencies
         (#(* (% 1) (inc (% 3)))))))

(defn multiplier [ns]
  (if (= 3 (first ns))
    1
    (case (count ns)
      1 1
      2 2
      3 4
      4 7)))

(defn b []
  (time
    (->> (parse get-input)
         sort
         diffs
         (partition-by identity)
         (map multiplier)
         (reduce *))))
