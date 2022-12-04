(ns day04.solution
  (:require [clojure.java.io :as io]))

(def ^:private parse-values
  (comp (partial map read-string)
        (partial re-seq #"\d+")))

(defn- fully-contained? [[start-1 end-1 start-2 end-2]]
  (if (or (<= start-1 start-2 end-2 end-1)
          (<= start-2 start-1 end-1 end-2))
    1
    0))

;; solution part 1

(time
 (with-open [rdr (io/reader "src/day04/input.txt")]
   (->> rdr
        line-seq
        (transduce
         (map (comp fully-contained? parse-values))
         +))))

;; solution part 2

(defn- any-overlap? [[start-1 end-1 start-2 end-2]]
  (if (or (<= start-1 start-2 end-1 end-2)
          (<= start-2 start-1 end-2 end-1)
          (<= start-1 start-2 end-2 end-1)
          (<= start-2 start-1 end-1 end-2))
    1
    0))

(time
 (with-open [rdr (io/reader "src/day04/input.txt")]
   (->> rdr
        line-seq
        (transduce
         (map (comp any-overlap? parse-values))
         +))))
