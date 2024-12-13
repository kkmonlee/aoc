(ns calibration-solver
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def number-words
  {"one" "1" "two" "2" "three" "3" "four" "4" "five" "5"
   "six" "6" "seven" "7" "eight" "8" "nine" "9"})

(def all-numbers
  (merge number-words
         (zipmap (map str (range 10)) (map str (range 10)))))

(defn create-pattern [words]
  (re-pattern (str "(" (str/join "|" words) ")")))

(def forward-pattern
  (create-pattern (keys all-numbers)))

(def reverse-pattern
  (create-pattern (map str/reverse (keys all-numbers))))

;; part 1
(defn process-line-part1 [line]
  (let [digits (re-seq #"\d" line)
        first-digit (first digits)
        last-digit (last digits)]
    (Integer/parseInt (str first-digit last-digit))))

;; part 2
(defn find-first-number [s pattern reverse?]
  (let [match (re-find pattern (if reverse? (str/reverse s) s))
        matched-str (if reverse?
                     (str/reverse (second match))
                     (second match))]
    (get all-numbers matched-str)))

(defn process-line-part2 [line]
  (let [first-num (find-first-number line forward-pattern false)
        last-num (find-first-number line reverse-pattern true)]
    (Integer/parseInt (str first-num last-num))))

(defn solve-calibration [input-file]
  (let [lines (-> input-file
                  io/file
                  slurp
                  str/split-lines)
        part1-values (map process-line-part1 lines)
        part2-values (map process-line-part2 lines)]
    {:part1-calibs part1-values
     :part1-sum (reduce + part1-values)
     :part2-calibs part2-values
     :part2-sum (reduce + part2-values)}))

;; Run the solution
(let [results (solve-calibration "input/input_01.txt")]
  (println "p1 calibration values:" (:part1-calibs results))
  (println "p1 sum:" (:part1-sum results))
  (println "\np2 calibration values:" (:part2-calibs results))
  (println "p2 sum:" (:part2-sum results)))