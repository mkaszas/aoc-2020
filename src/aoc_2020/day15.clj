(ns aoc-2020.day15)

(defn get-input [] (slurp "input/15"))

(defn parse [get-input] (read-string (format "[%s]" (get-input))))



(defn next_ [{:keys [i num seen]}]
  {:i (inc i)
   :num (if-let [n (seen num)] (- i n) 0)
   :seen (assoc seen num i)})

(defn init-game [numbers]
  {:seen (->> (drop-last 1 numbers)
              (map-indexed #(vector %2 (inc %1)))
              (into {}))
   :num (last numbers)
   :i (count numbers)})

(defn play-until-round [n numbers]
  (-> (iterate next_ (init-game numbers))
      (nth (- n (count numbers)))
      :num))


(defn a []
  (time
    (play-until-round 2020 (parse get-input))))

(defn b []
  (time
    (play-until-round 30000000 (parse get-input))))
