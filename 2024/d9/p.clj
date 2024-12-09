(ns solution
  (:require [clojure.string :as str]))

(defn parse-lengths [s]
  (map #(Character/digit % 10) (str/trim s)))

(defn pairs [xs]
  (cond
    (empty? xs) []
    (empty? (rest xs)) [(first xs) 0]
    :else (cons [(first xs) (second xs)] (pairs (drop 2 xs)))))

(defn build-disk [lengths]
  (loop [i 0
         fid 0
         acc []]
    (if (empty? lengths)
      acc
      (let [f (first lengths)
            r (next lengths)]
        (if (even? i)
          (recur (inc i) (inc fid) (if (pos? f) (into acc (repeat f fid)) acc))
          (recur (inc i) fid (if (pos? f) (into acc (repeat f ".")) acc))
          )))))

(defn find-gap [disk]
  (.indexOf disk "."))

(defn pop-right [disk]
  (if (empty? disk)
    [nil []]
    [(peek disk) (pop disk)]))

(defn pop-file-from-right [disk trailing]
  (let [[x xs] (pop-right disk)]
    (cond
      (nil? x) [nil xs trailing]
      (= x ".") (recur xs (conj trailing "."))
      :else [x xs trailing])))

(defn compact-step [disk]
  (let [gap (find-gap disk)]
    (if (neg? gap)
      disk
      (let [[block xs trailing] (pop-file-from-right disk [])]
        (cond
          (nil? block) (into xs trailing)
          (= block ".") (into xs trailing)
          :else (let [d' (vec (concat xs trailing))]
                  (if (>= gap (count d'))
                    (let [new-gap (find-gap d')]
                      (if (neg? new-gap)
                        (conj d' block)
                        (assoc d' new-gap block)))
                    (assoc d' gap block))))))))

(defn compact [disk]
  (loop [d disk]
    (let [gap (find-gap d)]
      (if (neg? gap)
        d
        (let [d' (compact-step d)]
          (if (= d d')
            d'
            (recur d')))))))

(defn checksum [disk]
  (reduce + (map-indexed (fn [i x] (if (= x ".") 0 (* i x))) disk)))

(defn file-info [disk]
  (reduce (fn [m [i b]]
            (if (= b ".")
              m
              (let [fid b
                    info (get m fid {:start i :end i :length 1})]
                (assoc m fid (if (= (:start info) (:end info))
                               info
                               (assoc info :end i :length (inc (:length info))))))))
          {}
          (map-indexed vector disk)))

(defn find-segment-for-file [disk file-length max-index]
  (loop [i 0
         count 0
         segment-start nil]
    (cond
      (>= i max-index) nil
      (= (nth disk i) ".")
      (let [ss (if (nil? segment-start) i segment-start)
            c (inc count)]
        (if (= c file-length)
          ss
          (recur (inc i) c ss)))
      :else (recur (inc i) 0 nil))))

(defn move-files [disk files]
  (let [order (reverse (sort (keys files)))]
    (reduce (fn [d f_id]
              (let [f_info (files f_id)
                    f_length (:length f_info)
                    f_start (:start f_info)
                    f_end (:end f_info)
                    target_start (find-segment-for-file d f_length f_start)]
                (if (nil? target_start)
                  d
                  (let [d1 (reduce (fn [acc pos] (assoc acc pos ".")) d (range f_start (inc f_end)))
                        d2 (reduce (fn [acc [idx blk]] (assoc acc idx blk))
                                   d1
                                   (map vector (range target_start (+ target_start f_length)) (repeat f_id)))]
                    (let [f_new_start target_start
                          f_new_end (dec (+ target_start f_length))]
                      d2)))))
            disk
            order)))

(defn -main []
  (let [disk-map (slurp "input.txt")
        lengths (parse-lengths disk-map)
        disk (build-disk lengths)
        final (compact disk)
        part1 (checksum final)]

    (println part1) 

    ;; rebuild
    (let [disk2 (build-disk lengths)
          file_id_count (loop [i 0 fid 0 lst lengths]
                          (if (empty? lst)
                            fid
                            (let [[f & r] lst]
                              (if (even? i)
                                (recur (inc i) (inc fid) r)
                                (recur (inc i) fid r)))))
          files (file-info disk2)
          moved (move-files disk2 files)
          part2 (checksum moved)]
      (println part2))))

(-main)