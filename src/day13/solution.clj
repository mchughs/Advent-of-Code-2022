(ns day13.solution
  (:require [clojure.java.io :as io]))

(def enumerate
  (partial map-indexed (fn [idx item] [(inc idx) item])))

(defn kompare
  "A negative return value means the ordering is correct"
  [l r]
  (cond
    (and (number? l) (number? r))
    (- l r)

    (or (number? l) (number? r))
    (let [l (if (vector? l) l [l])
          r (if (vector? r) r [r])]
      (kompare l r))

    :else
    (loop [[komparison & remaining] (map kompare l r)]
      (case komparison
        nil (- (count l) (count r))
        0   (recur remaining)
        komparison))))

;; part 1 solution

(time
 (with-open [rdr (io/reader "src/day13/input.txt")]
   (->> rdr
        line-seq
        (remove empty?)
        (map read-string)
        (partition 2)
        enumerate
        (reduce (fn [acc [idx [l r]]]
                  (if (neg? (kompare l r))
                    (+ acc idx)
                    acc))
                0))))

;; part 2 solution

(time
 (with-open [rdr (io/reader "src/day13/input.txt")]
   (->> rdr
        line-seq
        (remove empty?)
        (map read-string)
        (concat [[[2]] [[6]]])
        (sort-by identity kompare)
        enumerate
        (filter (fn [[_idx packet]]
                  (#{[[2]] [[6]]} packet)))
        (map first)
        (reduce * 1))))

