(require '[clojure.string :as str])

(def graph
  (->> (slurp "input/input_11.txt")
       str/split-lines
       (remove str/blank?)
       (map #(str/split % #": "))
       (map (fn [[k v]] [k (str/split v #" ")]))
       (into {})))

(defn paths [start goal]
  (let [memo (atom {})]
    (letfn [(go [node]
              (cond
                (= node goal) 1
                (contains? @memo node) (get @memo node)
                :else (let [s (reduce + 0 (map go (get graph node [])))]
                        (swap! memo assoc node s)
                        s)))]
      (go start))))

(def part1 (paths "you" "out"))

(def part2
  (+ (* (paths "svr" "dac")
        (paths "dac" "fft")
        (paths "fft" "out"))
     (* (paths "svr" "fft")
        (paths "fft" "dac")
        (paths "dac" "out"))))

(println "Part 1:" part1)
(println "Part 2:" part2)
