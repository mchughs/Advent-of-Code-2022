(ns day24.solution
  (:require [clojure.string :as string]))

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
  [(cond
     (zero? x) (- width 2)
     (= (dec width) x) 1
     :else x)
   (cond
     (zero? y) (- height 2)
     (= (dec height) y) 1
     :else y)])

(defn project [width height blizzards]
  (reduce-kv (fn [acc point blizzards]
               (reduce (fn [acc' blizzard]
                         (let [point' (bound width
                                             height
                                             ((move->fn blizzard) point))]
                           (update acc' point' conj blizzard)))
                       acc
                       blizzards))
             {}
             blizzards))

(defn move
  [{:keys [width height start end]} blizzards point]
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

;; solution part 1

(time ;; 179 too low
 (let [grid (string/split-lines (slurp "src/day24/input.txt"))
       width (count (first grid))
       height (count grid)
       start [1 0]
       end [(- width 2) (- height 1)]
       blizzards (->> (for [[y line] (enumerate grid)]
                        (for [[x char] (enumerate line)
                              :when (#{\> \< \^ \v} char)]
                          [[x y] (list char)]))
                      (apply concat)
                      (into {}))
       constants {:width width
                  :height height
                  :start start
                  :end end}]
   (->> {:places #{start} :blizzards blizzards}
        (iterate (fn [{:keys [places blizzards]}]
                   {:places (into #{} (mapcat #(move constants blizzards %) places))
                    :blizzards (project width height blizzards)}))
        (take-while (fn [{:keys [places]}]
                        (not (places end))))
        count)))
