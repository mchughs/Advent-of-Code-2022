(ns day05.solution 
  (:require [clojure.string :as string]))

(defn- transpose [m]
  (apply mapv vector m))

(defn- ->stack [items]
  (reduce (fn [acc item]
            (let [item (string/trim item)]
              (if (empty? item)
                acc
                (let [[_ letter _] item]
                  (conj acc letter)))))
          []
          items))

(defn- parse-cargo [input]
  (let [[cargo-input _] (string/split input #"\n\n")
        [_ & stack-layers-bottom-to-top] (reverse (string/split-lines cargo-input))]
    (->> stack-layers-bottom-to-top
         (map (partial re-seq #"\[[A-Z]\]\s?|\s{3}\s?"))
         transpose
         (map ->stack)
         vec)))

(defn- parse-moves [input]
  (for [[_ & xs] (re-seq #"move (\d+) from (\d+) to (\d+)" input)]
    (let [[amount from to] (mapv parse-long xs)]
      {:amount amount
       :from (dec from)
       :to (dec to)})))

(defn- split-at-last
  [coll n]
  [(drop-last n coll)
   (take-last n coll)])

(defn- perform-move [crane-version stacks {:keys [amount from to]}]
  (let [crane-fn (case crane-version
                   :9000 reverse
                   :9001 identity)
        
        [from-stack popped-values]
        (-> stacks
            (get from)
            (split-at-last amount))

        to-stack
        (-> stacks
            (get to)
            (concat (crane-fn popped-values)))]

    (-> stacks
        (assoc from (vec from-stack))
        (assoc to (vec to-stack)))))

(defn- read-from-top [stacks]
  (apply str (map last stacks)))

;; solution part 1

(time
 (let [input (slurp "src/day05/input.txt")
       stacks (parse-cargo input)
       moves (parse-moves input)]
   (->> moves
        (reduce (partial perform-move :9000) stacks)
        read-from-top)))

;; solution part 2

(time
 (let [input (slurp "src/day05/input.txt")
       stacks (parse-cargo input)
       moves (parse-moves input)]
   (->> moves
        (reduce (partial perform-move :9001) stacks)
        read-from-top)))
