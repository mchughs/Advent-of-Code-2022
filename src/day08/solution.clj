(ns day08.solution
  (:require [clojure.java.io :as io]))

(defn transpose [M]
  (apply mapv vector M))

(defn visible? [{:keys [rows columns coords]}]
  (let [[x y]   coords
        column  (get columns x)
        row     (get rows    y)
        value   (get column y) ;; or could have been (get row x) 
        top     (subvec column 0       y)
        left    (subvec row    0       x)
        bottom  (subvec column (inc y))
        right   (subvec row    (inc x))]
    (when (or (every? #(> value %) top)
              (every? #(> value %) bottom)
              (every? #(> value %) left)
              (every? #(> value %) right))
      coords)))

;; solution part 1

(time
 (with-open [rdr (io/reader "src/day08/input.txt")]
   (let [rows
         (->> rdr
              line-seq
              (mapv (fn [row]
                      (mapv #(Character/getNumericValue %) row))))

         columns
         (transpose rows)

         visible-interior-trees
         (for [x (range 1 (dec (count columns)))
               y (range 1 (dec (count rows)))
               :when (visible? {:rows rows
                                :columns columns
                                :coords [x y]})]
           [x y])

         width  (count (first rows))
         height (count (first columns))
         edge-trees-count (- (+ width width
                                height height)
                             4) ;; remove corner duplicates
         ]
     (+ (count visible-interior-trees)
        edge-trees-count))))

;; solution part 2

(defn view-distance [value coll]
  (let [x (->> coll
               (take-while #(< % value))
               count)]
    (if (= x (count coll))
      x
      (inc x))))

(defn compute-score [{:keys [rows columns coords]}]
  (let [[x y]   coords
        column  (get columns x)
        row     (get rows    y)
        value   (get column y) ;; or could have been (get row x) 
        top     (subvec column 0       y)
        left    (subvec row    0       x)
        bottom  (subvec column (inc y))
        right   (subvec row    (inc x))]
    (* (view-distance value (reverse left))
       (view-distance value right)
       (view-distance value (reverse top))
       (view-distance value bottom))))

(time
 (with-open [rdr (io/reader "src/day08/input.txt")]
   (let [rows
         (->> rdr
              line-seq
              (mapv (fn [row]
                      (mapv #(Character/getNumericValue %) row))))

         columns
         (transpose rows)

         scores
         (for [x (range 1 (dec (count columns)))
               y (range 1 (dec (count rows)))
               :let [score (compute-score {:rows rows
                                           :columns columns
                                           :coords [x y]})]]
           score)]
     (apply max scores))))
