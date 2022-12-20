(ns day20.solution 
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(def enumerate
  #(map-indexed (fn [idx item] [idx item]) %))

(def add-id
  #(map (fn [item] [(random-uuid) item]) %))

(defn mix
  [mix-order unmixed]
  (let [unmixed (->> unmixed (into {}) set/map-invert)
        length (count mix-order)
        mixed* (loop [[item & remaining] mix-order
                      file unmixed]
                 (if-not item
                   file
                   (let [[_id value] item
                         idx (get file item)
                         idx' (mod (+ idx value) (dec length))
                         idx' (if (zero? idx') (dec length) idx')
                         file' (reduce-kv (fn [m k v] ;; k is the [id value]
                                            ;; v is the curr idx
                                            (cond
                                              (= k item)        (assoc m k idx')
                                              (and (< idx v)
                                                   (<= v idx')) (assoc m k (dec v))
                                              (and (<= idx' v)
                                                   (< v idx))   (assoc m k (inc v))
                                              :else             (assoc m k v)))
                                          {}
                                          file)]
                     (recur remaining file'))))]
    (->> mixed*
         set/map-invert
         (sort-by first))))

;; solution part 1

(time
 (with-open [rdr (io/reader "src/day20/input.txt")]
   (let [encrypted-file (->> rdr line-seq (mapv parse-long))
         encrypted-file' (add-id encrypted-file)
         unmixed (->> encrypted-file' (into []) enumerate)
         mixed (mix encrypted-file' unmixed)
         mixed' (mapv (comp last last) mixed)
         origin (.indexOf mixed' 0)
         grove-indices '(1000 2000 3000)]
     (->> grove-indices
          (map #(nth (cycle mixed')
                     (+ % origin)))
          (reduce + 0)))))

;; solution part 2

(time
 (with-open [rdr (io/reader "src/day20/input.txt")]
   (let [decryption-key 811589153
         encrypted-file (->> rdr line-seq (mapv parse-long) (mapv #(* decryption-key %)))
         encrypted-file' (add-id encrypted-file)
         unmixed (->> encrypted-file' (into []) enumerate)
         mixed (->> unmixed
                    (iterate #(mix encrypted-file' %))
                    (take 11)
                    last)
         mixed' (mapv (comp last last) mixed)
         origin (.indexOf mixed' 0)
         grove-indices '(1000 2000 3000)]
     (->> grove-indices
          (map #(nth (cycle mixed')
                     (+ % origin)))
          (reduce + 0)))))
