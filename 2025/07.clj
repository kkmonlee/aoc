(def grid (vec (clojure.string/split-lines (slurp "input/input_7.txt"))))
(def start (.indexOf (first grid) "S"))
(def cols (count (first grid)))
(defn valid? [c] (<= 0 c (dec cols)))

(defn step1 [[beams splits] row]
  (reduce (fn [[b s] c]
            (if (= (nth row c) \^)
              [(into b [(dec c) (inc c)]) (inc s)]
              [(conj b c) s]))
          [#{} splits] beams))

(defn step2 [tl row]
  (->> tl
       (mapcat (fn [[c n]]
                 (if (= (nth row c) \^)
                   [[(dec c) n] [(inc c) n]]
                   [[c n]])))
       (filter (comp valid? first))
       (reduce (fn [m [k v]] (update m k (fnil + 0) v)) {})))

(println "Part 1:" (second (reduce step1 [#{start} 0] (rest grid))))
(println "Part 2:" (biginteger (reduce + (vals (reduce step2 {start 1N} (rest grid))))))
