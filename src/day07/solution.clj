(ns day07.solution
  (:require [clojure.java.io :as io]
            [clojure.zip :as z]
            [com.walmartlabs.cond-let :refer [cond-let]]))


(defn- move-to-dir [zipper target-dir]
  (->> zipper
       z/down
       (iterate z/right)
       (some (fn [loc]
               (when (= target-dir (:dir (z/node loc)))
                 loc)))))

#_:clj-kondo/ignore
(defn- run-cmd [zipper cmd]
  (cond-let
   (= "$ ls" cmd) zipper

   (= "$ cd .." cmd) (z/up zipper)

   :let [[_ dir] (re-matches #"\$ cd (\w+)" cmd)]
   dir (move-to-dir zipper dir)

   :let [[_ dir] (re-matches #"dir (\w+)" cmd)]
   dir (z/append-child zipper {:dir dir :children []})

   :let [[_ size file] (re-matches #"(\d+) ([\w\.]+)" cmd)]
   (and size file) (z/append-child zipper [file (parse-long size)])

   :else zipper))

(def init-zipper
  (z/zipper (comp sequential? :children)
            :children
            (fn [node children]
              (assoc node :children children))
            {:dir "/" :children []}))

(defn compute-dir-data
  "Computes the size of data in the directory, the path of the directory, and an accumulation of size data of all the sub directories."
  ([node]
   (compute-dir-data node nil))
  ([{:keys [dir children] :as _node} parent-path]
   (let [path (if (= "/" dir)
                "."
                (str parent-path "/" dir))]
     (if (empty? children)
       {:size 0
        :path path
        :acc '()}
       (let [{sub-nodes true
              files false} (group-by map? children)
             file-size (reduce #(+ %1 (second %2)) 0 files)
             sub-dirs (map (fn [node] (compute-dir-data node path)) sub-nodes)
             direct-sub-dir-sizes (map :size sub-dirs)
             indirect-sub-dir-sizes (map :acc sub-dirs)
             total-subdir-size (reduce + 0 direct-sub-dir-sizes)]
         {:size (+ file-size total-subdir-size)
          :path path
          :acc (flatten (concat direct-sub-dir-sizes indirect-sub-dir-sizes))})))))

;; solution part 1

(time
 (with-open [rdr (io/reader "src/day07/input.txt")]
   (let [dir-data (->> rdr
                       line-seq
                       rest
                       (reduce run-cmd init-zipper)
                       z/root
                       compute-dir-data)
         sizes (conj (:acc dir-data) (:size dir-data))]
     (transduce (filter #(>= 100000 %)) + 0 sizes))))

;; solution part 2

(time
 (with-open [rdr (io/reader "src/day07/input.txt")]
   (let [total-diskspace 70000000
         needed-space 30000000
         dir-data (->> rdr
                       line-seq
                       rest
                       (reduce run-cmd init-zipper)
                       z/root
                       compute-dir-data)
         sorted-sizes (sort (conj (:acc dir-data) (:size dir-data)))
         total-used (last sorted-sizes)
         total-unused (- total-diskspace total-used)
         delta (- needed-space total-unused)]
     (->> sorted-sizes
          (filter #(< delta %))
          first))))
