(ns prob002)

(quote "Even Fibonacci numbers
Problem 2
Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:

1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.")

(defn fib?
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

(println (str *ns* ":" ans-a))
