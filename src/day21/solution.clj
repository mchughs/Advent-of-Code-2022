(ns day21.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-input [input]
  (let [[_ monkey number x op y]  (re-find #"(\w+): (?:(\d+)|(\w+) ([\+\/\*\-]) (\w+))" input)]
    {(keyword monkey) (if number
                        (parse-long number)
                        {:op (read-string op)
                         :x (keyword x) :y (keyword y)})}))

(defn step [monkeys]
  (reduce-kv (fn [m k v]
               (assoc m k (if (number? v)
                            v
                            (let [{:keys [op x y]} v
                                  op (eval op)
                                  m (monkeys x)
                                  n (monkeys y)]
                              (if (and (number? m) (number? n))
                                (op m n)
                                v)))))
             {}
             monkeys))

;; solution part 1

(time
 (with-open [rdr (io/reader "src/day21/input.txt")]
   (let [monkeys (->> rdr line-seq (map parse-input) (apply merge))]
     (->> monkeys
          (iterate step)
          (take-while #(not (number? (:root %))))
          last
          step
          :root))))

;; solution part 2

(defn parse-input' [input]
  (let [[_ monkey number x op y]  (re-find #"(\w+): (?:(\d+)|(\w+) ([\+\/\*\-]) (\w+))" input)]
    {monkey (if number
              (parse-long number)
              (format "(%s %s %s)" op x y))}))

(defn expand [monkeys]
  (reduce-kv (fn [m k v]
               (assoc m k
                      (cond (number? v)
                            v
                            
                            (nil? (re-seq #"[Aa-z]" v))
                            (eval (read-string v))
                            
                            :else
                            (string/replace v #"[a-z]+" (comp str monkeys)))))
             {}
             monkeys))

(defn simplify [equation]
  (string/replace equation #"\(([\+\-\/\*]) (\d+) (\d+)\)"
                  (fn [[_ op x y]]
                    (str
                     ((eval (read-string op))
                      (parse-long x)
                      (parse-long y))))))

(def inverses
  {/ *
   * /
   + -
   - +})

(defn solve [equation-string]
  ;; WARNING: Assumes the sub-expression is on the left hand side of the equation.
  (let [[_ source target] (read-string equation-string)]
    (loop [target target
           [op x y] source]
      (let [op' (eval op)
            inverse (get inverses op')
            sub-expression-first-position (when-not (number? x) x)
            sub-expression-last-position (when-not (number? y) y)
            n (if (number? x) x y)]
        (cond
          (= sub-expression-first-position 'A)
          (inverse target n)

          (= sub-expression-last-position 'A)
          (cond
            (= op' -) (- n target)
            (= op' /) (/ n target)
            :else (inverse target n))

          sub-expression-first-position
          (recur (inverse target n)
                 sub-expression-first-position)

          :else
          (cond
            (= op' -) (recur (- n target)
                             sub-expression-last-position)
            (= op' /) (recur (/ n target)
                             sub-expression-last-position)
            :else (recur (inverse target n)
                         sub-expression-last-position)))))))

(time
 (with-open [rdr (io/reader "src/day21/input.txt")]
   (let [monkeys (->> rdr
                      line-seq
                      (map parse-input')
                      (apply merge))
         monkeys' (-> monkeys
                      (assoc "humn" "A")
                      (update "root" #(let [op (first (re-seq #"[\+\-\/\*]" %))]
                                        (string/replace % op "="))))
         expanded (->> monkeys'
                       (iterate expand)
                       (take-while #(let [expression (get % "root")]
                                      (seq (re-seq #"[a-z]" expression))))
                       last
                       expand)
         equation (get expanded "root")
         expression (->> equation
                         (iterate simplify)
                         (partition 2 1)
                         (take-while (fn [[a b]]
                                       (not= a b)))
                         last
                         last)]
     (solve expression))))
