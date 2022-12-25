(ns day24.solution 
  (:require [clojure.string :as string]
            [clojure.zip :as z]))

;; --- A* algorithm code taken from https://matthewdowney.github.io/astar-in-clojure-find-k-shortest-paths.html

(declare a*, a*-seq, next-a*-path, unseen?, step-factory, rpath, cmp-step)

(defn a*
  "A sequence of paths from `src` to `dest`, shortest first, within the supplied `graph`.
  If the graph is weighted, supply a `distance` function. To make use of A*, supply a 
  heuristic function. Otherwise performs like Dijkstra's algorithm."
  [graph src dest & {:keys [distance heuristic]}]
  (let [init-adjacent (sorted-set-by cmp-step {:node src :cost 0 :entered 0})]
    (a*-seq graph dest init-adjacent
            (or distance (constantly 1))
            (or heuristic (constantly 0)))))

(defn a*-seq
  "Construct a lazy sequence of calls to `next-a*-path`, returning the shortest path first."
  [graph dest adjacent distance heuristic]
  (lazy-seq
   (when-let [[path, adjacent'] (next-a*-path graph dest adjacent distance heuristic)]
     (cons path (a*-seq graph dest adjacent' distance heuristic)))))

(defn next-a*-path [graph dest adjacent f-cost f-heur]
  (when-let [{:keys [node] :as current} (first adjacent)]
    (let [path (rpath current)
          adjacent' (disj adjacent current)] ;; "pop" the current node
      (if (= node dest)
        [(reverse path), adjacent']
        (let [last-idx (or (:entered (last adjacent')) 0)
              factory (step-factory current last-idx f-cost f-heur dest)
              xform (comp (filter (partial unseen? path)) (map-indexed factory))
              adjacent'' (into adjacent' xform (get graph node))]
          (recur graph dest adjacent'' f-cost f-heur))))))

(defn unseen? [path node]
  (not-any? #{node} path))

(defn step-factory [parent last-insertion cost heur dest]
  (fn [insertion-idx node]
    {:parent parent
     :node node
     :entered (+ last-insertion (inc insertion-idx))
     :cost (+ (:cost parent) (cost (:node parent) node) (heur node dest))}))

(defn rpath [{:keys [node parent]}]
  (lazy-seq
   (cons node (when parent (rpath parent)))))

(defn cmp-step [step-a step-b]
  (let [cmp (compare (:cost step-a) (:cost step-b))]
    (if (zero? cmp)
      (compare (:entered step-a) (:entered step-b))
      cmp)))

;; ---

(def enumerate
  #(map-indexed (fn [idx item] [idx item]) %))

(def move->fn
  {\> #(update % 0 inc)
   \< #(update % 0 dec)
   \^ #(update % 1 dec)
   \v #(update % 1 inc)
   :wait identity})
(def moves (vals move->fn))

(defn bound [width height [x y]]  
  (let [x' (mod x (dec width))
        y' (mod y (dec height))]
    [(if (zero? x') 1 x')
     (if (zero? y') 1 y')]))

(defn project [width height blizzards]
  (reduce-kv (fn [acc point blizzard]
               (let [point' (bound width
                                   height
                                   ((move->fn blizzard) point))]
                 (assoc acc point' blizzard)))
             {}
             blizzards))

(defn move [width height start end blizzards point]
  (let [blizzard-locations (set (keys (project width height blizzards)))
        options (into #{}
                      (comp (map #(% point))
                            (filter (fn [[x y :as point]]
                                      (or (#{start end} point)
                                          (and (< 0 x width)
                                               (< 0 y height)))))
                            (remove blizzard-locations))
                      moves)]
    options))

(defn iter-zip [zipper]
  (->> zipper
       (iterate z/next)
       (take-while (complement z/end?))))

(defn build-tree [width height start end blizzards zipper]
  (->> zipper
       iter-zip
       (map #(z/edit % (fn [node] node
                         #_
                         (if (is-child? node)
                           node
                           #_
                           (move width height start end blizzards node)
                           #_(cons % (move width height start end blizzards
                                           (z/node %)))
                           node))))))

;; solution part 1

(time
 (let [grid (string/split-lines (slurp "src/day24/input.txt"))
       width (count (first grid))
       height (count grid)
       start [1 0]
       end [(- width 2) (- height 1)]
       blizzards (->> (for [[y line] (enumerate grid)]
                        (for [[x char] (enumerate line)
                              :when (#{\> \< \^ \v} char)]
                          [[x y] char]))
                      (apply concat)
                      (into {}))
       zipper (z/zipper list? seq conj (list start))]
   #_blizzards
   #_(project width height blizzards)
   #_(move width height start end blizzards start)
   (build-tree width height start end blizzards zipper)))

#_(z/insert-child (z/seq-zip (list [1 0]))
                [1 1])

(cons [0 1] #{[1 1] [2 2]})

(defn is-child? [loc]
  (nil? (z/down loc)))

(is-child? (z/down (z/zipper list? seq conj (list [0 1]))))
