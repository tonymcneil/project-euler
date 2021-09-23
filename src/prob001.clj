(ns prob001
  (:require [clojure.set]))

; no external deps
(defn ans-a []
 (reduce +
  (set 
   (concat
     (range 0 1000 3)
     (range 0 1000 5)))))

; a slightly more terse solution using clojure.set library for union
(defn ans-b []
 (reduce +
  (clojure.set/union
   (range 0 1000 3)
   (range 0 1000 5))))

(println (str "001:" (ans-a)))