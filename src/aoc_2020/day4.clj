(ns aoc-2020.day4
  (:require [clojure.string :as str]))

(defn get-input [] (slurp "input/4"))

(defn parse [get-input]
  (->> (str/split (get-input) #"\n\n")
       (map #(->> (str/replace % #"(byr|iyr|eyr|hgt|hcl|ecl|pid|cid):(\#?[A-z0-9]+)(\s|$)" ":$1 \"$2\"")
                  (format "{%s}")
                  read-string))))

(def required-fields
  [:byr
   :iyr
   :eyr
   :hgt
   :hcl
   :ecl
   :pid])

(defn check-fields [pass]
  (every? #(contains? pass %) required-fields))

(defn validate-number [min max s]
  (try (let [n (Integer/parseInt s)] (and (>= n min) (<= n max))) (catch Exception _ false)))

(defn validate-height [{:keys [unit val]}]
  (case unit
    :cm (and (>= val 150) (<= val 193))
    :in (and (>= val 59) (<= val 76))
    false))

(def field-validators
  {:byr #(validate-number 1920 2002 %)
   :iyr #(validate-number 2010 2020 %)
   :eyr #(validate-number 2020 2030 %)
   :hgt #(try (let [n (read-string (str/replace % #"^(\d+)(cm|in)$" "{:unit :$2 :val $1}"))] (validate-height n)) (catch Exception _ false))
   :hcl #(string? (re-matches #"\#[0-9a-f]{6}" %))
   :ecl #(string? (re-matches #"amb|blu|brn|gry|grn|hzl|oth" %))
   :pid #(string? (re-matches #"[0-9]{9}" %))})


(defn validate [pass]
  (->> required-fields
       (every? #((field-validators %) (pass %)))))




(defn a []
  (time (->> (parse get-input)
             (filter check-fields)
             count)))

(defn b []
  (time (->> (parse get-input)
             (filter check-fields)
             (filter validate)
             count)))
