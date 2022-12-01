(ns day01.solution
  (:require [clojure.java.io :as io]
            [clojure.data.priority-map :as pm]))

;; solution part 1

(defn- calorie-order
  [calories]
  (loop [[calorie & remaining] calories
         acc (pm/priority-map-by > 0 0)
         idx 0]
    (cond
      (nil? calorie)
      acc

      (empty? calorie)
      (let [idx* (inc idx)]
        (recur
         remaining
         (assoc acc idx* 0)
         idx*))

      :else
      (let [n (Integer. calorie)]
        (recur
         remaining
         (update acc idx + n)
         idx)))))

(time
 (with-open [rdr (io/reader "src/day01/input.txt")]
   (->> rdr
        line-seq
        calorie-order
        peek
        second)))

;; solution part 2

(time
 (with-open [rdr (io/reader "src/day01/input.txt")]
   (->> rdr
        line-seq
        calorie-order
        seq
        (take 3)
        (map last)
        (apply +))))
