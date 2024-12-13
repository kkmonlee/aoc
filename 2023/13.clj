(ns reflection-solver
  (:require [clojure.string :as str]))

(defn parse-patterns [input-text]
  (->> (str/split-lines input-text)
       (reduce (fn [acc line]
                 (if (str/blank? line)
                   (if (seq (:current-pattern acc))
                     {:patterns (conj (:patterns acc) (:current-pattern acc))
                      :current-pattern []}
                     acc)
                   (update acc :current-pattern conj line)))
               {:patterns [] :current-pattern []})
       ((fn [{:keys [patterns current-pattern]}]
          (if (seq current-pattern)
            (conj patterns current-pattern)
            patterns)))))

(defn find-reflection [pattern exclude]
  (let [rows (count pattern)
        cols (count (first pattern))]
        ;; horizontal
    (or
     (some (fn [i]
             (let [top i
                   bottom (inc i)]
               (loop [top top bottom bottom is-reflection true]
                 (cond
                   (or (< top 0) (>= bottom rows))
                   (if is-reflection (when (not= exclude ["H" (inc i)]) ["H" (inc i)]) nil)

                   (not= (nth pattern top) (nth pattern bottom))
                   nil

                   :else
                   (recur (dec top) (inc bottom) is-reflection)))))
           (range (dec rows)))
        ;; vertical
     (some (fn [i]
             (let [left i
                   right (inc i)]
               (loop [left left right right is-reflection true]
                 (cond
                   (or (< left 0) (>= right cols))
                   (if is-reflection (when (not= exclude ["V" (inc i)]) ["V" (inc i)]) nil)

                   (not= (apply str (map #(nth % left) pattern))
                          (apply str (map #(nth % right) pattern)))
                   nil

                   :else
                   (recur (dec left) (inc right) is-reflection)))))
           (range (dec cols))))
     [nil 0]))

(defn try-fix-smudge [pattern]
  (let [original-reflection (find-reflection pattern nil)]
    (loop [i 0]
      (if (>= i (count pattern))
        [nil 0]
        (let [row (nth pattern i)]
          (loop [j 0]
            (if (>= j (count row))
              (recur (inc i))
              (let [flipped (update-in pattern [i j] #(if (= % \#) \., \#))
                    reflection (find-reflection flipped original-reflection)]
                (if (first reflection)
                  reflection
                  (recur (inc j)))))))))))

(defn calculate-summary [patterns fix-smudges]
  (reduce (fn [total pattern]
            (let [[reflection-type value]
                  (if fix-smudges
                    (try-fix-smudge pattern)
                    (find-reflection pattern nil))]
              (+ total (condp = reflection-type
                        "H" (* 100 value)
                        "V" value
                        0))))
          0
          patterns))

(defn solve [input-text]
  (let [patterns (parse-patterns input-text)]
    {:part1 (calculate-summary patterns false)
     :part2 (calculate-summary patterns true)}))

(defn -main [& args]
  (let [input-text (slurp (first args))
        results (solve input-text)]
    (println "Part 1: " (:part1 results))
    (println "Part 2: " (:part2 results))))
