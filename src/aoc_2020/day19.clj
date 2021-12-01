(ns aoc-2020.day19
  (:require [clojure.string :as str]
            [instaparse.core :as insta]))

(defn get-input [] (slurp "input/19"))

(defn parse [input]
  (let [[rules words] (str/split input #"\n\n")]
    {:rules (->> (str/split-lines rules)
                 (map #(str/replace % #"(\d+): \"?([^\"]*)\"?$" "[$1 \"$2\"]"))
                 (map read-string)
                 (into {}))
     :words (str/split-lines words)}))


(defn resolve-rules [rules]
  (let [a-rules (atom {})]
    (do
      (doseq [[i rule-str] rules]
        (swap!
          a-rules
          assoc
          i
          (delay
            (if (re-seq #"[a-z]" rule-str)
              rule-str
              (->> (str/split rule-str #" \| ")
                   (map #(format "[%s]" %))
                   (map read-string)
                   (map #(map (fn [n] @(@a-rules n)) %))
                   (map #(map (fn [s] (format "(%s)" s)) %))
                   (map str/join)
                   (str/join "|"))))))
      @a-rules)))



(defn a []
  (let [{:keys [rules words]} (parse (get-input))
        rule-0 @((resolve-rules rules) 0)]
    ;(->> words
    ;     (filter #(re-seq (re-pattern (str "^" rule-0 "$")) %))
    ;     count)
    rule-0
    ))



(defn resolve-rules-b [rules]
  (let [a-rules (atom {})]
    (do
      (doseq [[i rule-str] rules]
        (swap!
          a-rules
          assoc
          i
          (delay
            (if (re-seq #"[a-z]" rule-str)
              rule-str
              (case i
                8 (format "(%s)+" @(@a-rules 42))
                11 (format "(?<g12>((%1$s)(%2$s))|((%1$s)\\g<g12>(%2$s)))" @(@a-rules 42) @(@a-rules 31))
                (->> (str/split rule-str #" \| ")
                   (map #(format "[%s]" %))
                   (map read-string)
                   (map #(map (fn [n] @(@a-rules n)) %))
                   (map #(map (fn [s] (format "(%s)" s)) %))
                   (map str/join)
                   (str/join "|")))))))
      @a-rules)))

(defn b []
  (let [{:keys [rules words]} (parse (get-input))
        rule-0 @((resolve-rules-b rules) 0)]
    ;(->> words
    ;     (filter #(re-seq (re-pattern (str "^" rule-0 "$")) %))
    ;     )

    (println rule-0)
    ))



;;;;; SECOND SOLUTION (instaparser)



(defn a []
  (time
    (let [[rules words] (str/split (get-input) #"\n\n")
          parser (insta/parser rules :start :0)]
      (->> (str/split-lines words)
           (map parser)
           (filter vector?)
           count))))

(defn b []
  (time
    (let [[rules words] (str/split (get-input) #"\n\n")
          rules-replaced (-> rules
                             (str/replace #"8: 42" "8: 42 | 42 8")
                             (str/replace #"11: 42 31" "11: 42 31 | 42 11 31"))
          parser (insta/parser rules-replaced :start :0)]
      (->> (str/split-lines words)
           (map parser)
           (filter vector?)
           count))))