(require '[clojure.string :as str])

(let [[range-sec id-sec] (str/split (slurp "input/input_5.txt") #"\n\n")
    ranges (->> (str/split-lines range-sec)
              (map #(mapv parse-long (str/split % #"-"))))
    ids (map parse-long (str/split-lines id-sec))
    in-range? (fn [id] (some (fn [[a b]] (<= a id b)) ranges))
    merged (->> (sort ranges)
              (reduce (fn [acc [s e]]
                        (let [[ls le] (peek acc)]
                          (if (and le (<= s (inc le)))
                            (conj (pop acc) [ls (max le e)])
                            (conj acc [s e])))) []))]
  (println "Part 1:" (count (filter in-range? ids)))
  (println "Part 2:" (reduce + (map (fn [[s e]] (- (inc e) s)) merged))))