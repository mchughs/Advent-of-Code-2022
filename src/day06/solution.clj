(ns day06.solution
  (:require [clojure.java.io :as io]))

(defn char-seq
  [^java.io.BufferedReader rdr]
  (let [c (.read rdr)]
    (when (not= c -1)
      (cons (char c) (lazy-seq (char-seq rdr))))))

;; solution part 1

(time
 (with-open [rdr (io/reader "src/day06/input.txt")]
   (->> rdr
        char-seq
        (partition 4 1)
        (reduce (fn [acc marker]
                  (if-not (apply distinct? marker)
                    (inc acc)
                    (reduced acc)))
                4))))

;; solution part 2

(time
 (with-open [rdr (io/reader "src/day06/input.txt")]
   (->> rdr
        char-seq
        (partition 14 1)
        (reduce (fn [acc marker]
                  (if-not (apply distinct? marker)
                    (inc acc)
                    (reduced acc)))
                14))))
