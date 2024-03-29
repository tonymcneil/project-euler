(ns prob001
  (:require [clojure.set]))

(quote "Multiples of 3 or 5
Problem 1
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.")

(defn multiples-between [start end targets]
  (set (apply concat (map #(range start end %) targets))))

(def ans-a
  (reduce + (multiples-between 0 1000 [3 5])))

(time
 (println (str *ns* ":" ans-a)))
