(ns aoc-2022.core
  (:require [aoc-2022.utils :refer [input]]
            [clojure.string :as str]))

(defn day1 [actual? part]
  (let [input (input 1 {:lines? false :actual? actual?})
        elves (->> (str/split input #"\n\n")
                   (map str/split-lines)
                   (map #(map read-string %))
                   (map (partial apply +)))]
    (case part
      1 (apply max elves)
      2 (apply + (take 3 (sort > elves))))))

(defn day2 [actual? part]
  (let [input (input 2 {:lines? true :actual? actual?})
        match-result1 (fn [a b]
                        (let [b ({\X \A
                                  \Y \B
                                  \Z \C} b)
                              ab (str a b)]
                          (if (= a b)
                            3
                            (case ab
                              "AB" 6
                              "AC" 0
                              "BC" 6
                              "BA" 0
                              "CB" 0
                              "CA" 6
                              0))))
        rps (fn [a res]
              (case res
                \X (char (+ 65 (mod (int a) 3)))
                \Y a
                \Z (char (+ 65 (mod (+ 2 (int a)) 3)))))
        val {\X 1
             \Y 2
             \Z 3
             \A 1
             \B 2
             \C 3}
        res-1 (loop [input input
                     acc 0]
                (if (> (count input) 0)
                  (recur (rest input)
                         (+ acc (val (last (first input))) (match-result1 (ffirst input) (last (first input)))))
                  acc))
        res-2 (loop [input input
                     acc 0]
                (if (empty? input)
                  acc
                  (let [mine (rps (ffirst input) (last (first input)))]
                    (recur (rest input)
                           (+ acc (val mine)
                              (case (last (first input))
                                \X 0
                                \Y 3
                                \Z 6))))))]
    (case part
      1 res-1
      2 res-2)))

(defn day3 [actual? part]
  (let [char-to-prio (fn [char]
                       (let [i (int char)]
                         (if (>= (int \Z) i)
                           (- i 38)
                           (- i 96))))
        input (input 3 {:actual? actual?})]
    (case part
      1 (reduce #(+ %1 (let [h1 (apply str (take (/ (count %2) 2) %2))
                             h2 (apply str (drop (/ (count %2) 2) %2))]
                         (char-to-prio (first (for [x h1
                                                    :when (str/includes? h2 (str x))]
                                                x))))) 0 input)
      2 (loop [input input
               acc 0]
          (if (empty? input)
            acc
            (recur
             (drop 3 input)
             (+ acc (first (let [elves (take 3 input)]
                             (for [x (first elves)
                                   :when (and
                                          (str/includes? (nth elves 1) (str x))
                                          (str/includes? (nth elves 2) (str x)))]
                               (char-to-prio x)))))))))))

(defn day4 [actual? part]
  (let [input (input 4
                     {:actual? actual?})
        pairs (->> input
               (map (fn [pair]
                      (->> (str/split pair #",")
                           (map #(str/split % #"-"))
                           (map #(map read-string %))
                           (map (fn [[start end]]
                                  (range start (inc end))))
                           (sort-by (comp - count))))))]
    (case part
      1 (count (filter (fn [pair]
                       (= (second pair) (filter (set (first pair)) (second pair)))) pairs))
      2 (count (filter (fn [pair]
                         (seq (filter (set (first pair)) (second pair)))) pairs)))))
