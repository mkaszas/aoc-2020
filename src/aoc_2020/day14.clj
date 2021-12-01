(ns aoc-2020.day14
  (:require [clojure.string :as str]))

(defn get-input [] (slurp "input/14"))

(defn parse [get-input]
  (->> (get-input)
       str/split-lines
       (map #(or
               (->> (re-matches #"mask = (.*)" %)
                    ((fn [[_ val]] (if (string? val) {:cmd :mask :val val}))))
               (->> (re-matches #"mem\[(\d+)\] = (\d+)" %)
                    ((fn [[_ addr val]] {:cmd :mem :addr (Integer/parseInt addr) :val (Integer/parseInt val)})))))))


(defn parse-big-bin [str] (read-string (format "2r%s" str)))

(defn parse-mask [str-mask]
  {:mask-0 (parse-big-bin (str/replace str-mask #"X" "1"))
   :mask-1 (parse-big-bin (str/replace str-mask #"X" "0"))})

(defn masked-value [{:keys [mask-0 mask-1]} value]
  (->> value
       (bit-and mask-0)
       (bit-or mask-1)))

(defn eval-cmd [state {:keys [cmd addr val]}]
  (case cmd
    :mask (assoc state :mask (parse-mask val))
    :mem (assoc-in state [:mem addr] (masked-value (state :mask) val))))


(defn a []
  (time
    (->> (parse get-input)
         (reduce eval-cmd {})
         :mem
         vals
         (reduce +))))



(defn floating-bit-combos [bits]
  (if (some #{:X} bits)
    (->> (map-indexed vector bits)
         (filter #(= :X (second %)))
         ffirst
         (#(vector (floating-bit-combos (assoc bits % 0)) (floating-bit-combos (assoc bits % 1))))
         (mapcat identity)
         )
    [(parse-big-bin (apply str bits))]))


(defn masked-addresses [mask address]
  (let [address-bin (str/replace (format (str "%" (count mask) "s") (Integer/toBinaryString address)) #" " "0")]
    (->> (range 0 (count mask))
         (mapv #(case (nth mask %)
                  \1 1
                  \0 (nth address-bin %)
                  \X :X))
         floating-bit-combos)))


(defn assoc-all [m ks v]
  (reduce #(assoc %1 %2 v) m ks))

(defn eval-cmd-b [state {:keys [cmd addr val]}]
  (case cmd
    :mask (assoc state :mask val)
    :mem (update state :mem #(assoc-all % (masked-addresses (state :mask) addr) val))))


(defn b []
  (time
    (->> (parse get-input)
         (reduce eval-cmd-b {})
         :mem
         vals
         (reduce +))))
