(ns prob002)

(defn fib?
  "fibinacci sequence with stop-pred? used to decide when to stop and filter-pred? to limit the items in the sequence (for all items use something trutyh for filter-pred? like identity?)"
  [stop-pred? filter-pred?]
  (loop [cur 0 nxt 1 col []]
    (if (stop-pred? cur)
      col
      (recur nxt (+ cur nxt)
             (if (filter-pred? cur)
               (conj col cur)
               col)))))

(def ans-a 
  (reduce + (fib? #(>= % 4000000) even?)))

(println (str "002:" ans-a))
