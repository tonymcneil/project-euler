(ns prob003)

(quote "Largest prime factor 
Problem 3
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?


primes...
https://en.m.wikipedia.org/wiki/Sieve_of_Eratosthenes
       
Create a list of consecutive integers from 2 through n: (2, 3, 4, ..., n).
Initially, let p equal 2, the smallest prime number.
       
Enumerate the multiples of p by counting in increments of p from 2p to n, and mark them in the list (these will be 2p, 3p, 4p, ...; the p itself should not be marked).
       
Find the smallest number in the list greater than p that is not marked. If there was no such number, stop. Otherwise, let p now equal this new number (which is the next prime), and repeat from step 3.
When the algorithm terminates, the numbers remaining not marked in the list are all the primes below n.
")

(defn primes-to [pmax]
(loop
  [p 2
   ps (into [] (range 0 (inc pmax)))]
(if (nil? p)
  (remove nil? (drop 2 ps))
(recur
 (->> ps
         (drop (inc p))
         (remove nil?)
         first)
(let [nils (rest (range p (inc pmax) p))]
  (if (empty? nils) ps
    (apply assoc ps
   (interleave 
    nils
    (repeat nil)))))
 
 ))))

(quote
(println
 ;; (def pidxs
  #?(:clj ^java.lang.Double 0
      :cljs 0)))
(println
 (java.lang.Double/MAX_VALUE))

;; (println (last (primes-to 10000)))
;; (println (str *ns* ":" ))
;; (println (persistent! (transient (into [] (range 3)))))

;; https://stackoverflow.com/a/7941430
(quote (do
(def certainty 5)

(defn prime? [n]
      (.isProbablePrime (BigInteger/valueOf n) certainty))

  (println (last
(concat [2] (take 1000
   (filter prime? 
      (take-nth 2 
         (range 1 10000)))))
))))

;; https://stackoverflow.com/a/7625207
(quote (do
(defn gen-primes "Generates an infinite, lazy sequence of prime numbers"
  []
  (letfn [(reinsert [table x prime]
             (update-in table [(+ prime x)] conj prime))
          (primes-step [table d]
             (if-let [factors (get table d)]
               (recur (reduce #(reinsert %1 d %2) (dissoc table d) factors)
                      (inc d))
               (lazy-seq (cons d (primes-step (assoc table (* d d) (list d))                                 (inc d))))))]
    (primes-step {} 2)))

(take 5 (gen-primes))
))

(quote (do
(defn sieve [n]
  "Returns a BitSet with bits set for each prime up to n"
  (let [bs (new java.util.BitSet n)]
    (.flip bs 2 n)
    (doseq [i (range 4 n 2)] (.clear bs i))
    (doseq [p (range 3 (Math/sqrt n))]
      (if (.get bs p)
        (doseq [q (range (* p p) n (* 2 p))] (.clear bs q))))
    bs))

;; (do (sieve 1e6) nil)
(def theBitSet (sieve 6))

(take-while #(not (= % -1)) (iterate #(.nextSetBit theBitSet (inc %)) 2))

))

;; my initial solutions
(quote (do
; faster nilling impl
  (loop [nils (vec (rest (range p (inc pmax) p)))
         nilled-ps ps]
    (if (empty? nils)
      nilled-ps
(recur (rest nils)
       (assoc nilled-ps 
              (first nils) nil))))
; slower nilling impl
(let [nils (rest (range p (inc pmax) p))]
  (if (empty? nils) ps
    (apply assoc ps
   (interleave 
    nils
    (repeat nil)))))
 ))
 