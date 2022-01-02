(ns prob001
  (:require [clojure.set]))

(quote "Multiples of 3 or 5
Problem 1
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.")

; no external deps
(def ans-a
 (reduce +
  (set 
   (concat
     (range 0 1000 3)
     (range 0 1000 5)))))

; a slightly more terse solution using clojure.set library for union
(def ans-b
 (reduce +
  (clojure.set/union
   (range 0 1000 3)
   (range 0 1000 5))))

(println (str *ns* ":" ans-a))
;; (println (str *ns* ":" ans-b))