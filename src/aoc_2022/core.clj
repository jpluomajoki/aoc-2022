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

(day1 false 1)
(day1 true 1)
(day1 false 2)
(day1 true 2)
