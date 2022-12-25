(ns day25.solution
  (:require [clojure.java.io :as io]))

(def snafu-values
  {\= -2
   \- -1
   \0 0
   \1 1
   \2 2})

;; technically you can have higher carries but we won't worry about those in this case
(def inv 
  {-4 {:carry -1 :digit "1"}
   -3 {:carry -1 :digit "2"}
   -2 {:carry 0  :digit "="}
   -1 {:carry 0  :digit "-"} 
    0 {:carry 0  :digit "0"}
    1 {:carry 0  :digit "1"}
    2 {:carry 0  :digit "2"}
    3 {:carry 1  :digit "="}
    4 {:carry 1  :digit "-"}})

(def snafu-digits
  {\0 "0"
   \1 "1"
   \2 "2"
   \3 "1="
   \4 "1-"})

(defn parse-input [input]
  (map snafu-values input))

(defn quinary->decimal [n]
  (apply +
         (map *
              (reverse n)
              (iterate #(* 5 %) 1))))

(defn add-snafu [m n]
  (->> (map (fn [a b] [a b]) (reverse m) (reverse n))
       (reduce
        (fn [{:keys [acc carry]} [a b]]
          (let [{c :carry d :digit} (inv (+ (snafu-values a)
                                            (snafu-values b)
                                            carry))]
            {:acc (str acc d)
             :carry c}))
        {:acc ""
         :carry 0})
       :acc
       reverse
       (apply str)))

(defn decimal->snafu [n]
  (let [f (fn decimal->quinary [n]
            (if (zero? n)
              ""
              (str (rem n 5)
                   (decimal->quinary (quot n 5)))))
        quinary (f n)
        expansion (->> (loop [[digit & remaining] quinary
                              place 0
                              acc []]
                         (if-not digit
                           acc
                           (let [digit' (snafu-digits digit)]
                             (recur remaining
                                    (inc place)
                                    (conj acc (apply str
                                                     digit'
                                                     (repeat place "0")))))))
                       (map #(let [n (- (count quinary)
                                        (count %))]
                               (str (apply str (repeat n "0"))
                                    %))))]
    (reduce add-snafu
            (apply str (repeat (count quinary) "0"))
            expansion)))

;; solution part 1

(time ;; 2-0-0=1-0=2====20=-2
 (with-open [rdr (io/reader "src/day25/input.txt")]
   (->> rdr
        line-seq
        (transduce (comp (map parse-input)
                         (map quinary->decimal))
                   +
                   0)
        decimal->snafu)))

;; solution part 2

;; See https://adventofcode.com/2022/day/25#part2
