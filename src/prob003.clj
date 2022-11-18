(ns prob003
 (:require [lib.bitbag :refer :all]))

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


(quote (do
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://stackoverflow.com/a/22668959/4908704
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sieve [n]
  "Returns a BitSet with bits set for each prime up to n"
  (let [bs (new java.util.BitSet n)]
    (.flip bs 2 n)
    (doseq [i (range 4 n 2)] (.clear bs i))
    (doseq [p (range 3 (Math/ceil (Math/sqrt n)))]
      (if (.get bs p)
        (doseq [q (range (* p p) n (* 2 p))] (.clear bs q))))
    bs))

(def theBitSet (seive 6))
(take-while #(not (= % -1)) (iterate #(.nextSetBit theBitSet (inc %)) 2))
))

(quote (do
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://gist.github.com/dukky/6265182
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; public static int sieve(int n) {
;; 		BitSet bits = new BitSet(n+1);
;; 		bits.clear();
;; 		bits.flip(0, n);
;; 		double csqrtn = Math.ceil(Math.sqrt(n));
;; 		for (int i = 2; i < csqrtn; ++i) 
;; 			if (bits.get(i)) 
;; 				for (int j = i * i; j < n; j += i) 
;; 					bits.set(j, false);
;; 		return bits.cardinality() - 2;
;; 	}
))



;; (quote (do
;; (defn sieve [n]
;;   "Returns a bit-bag with bits set for each prime up to n"
;;   (let [bb (bit-bag n)]
;;     (bit-flip! bb 2 n)
;;     (doseq [i (range 3 (Math/ceil (Math/sqrt n)))]
;;       (if (bit-set? bb i)
;;         (doseq [j (range (* i i) n)]
;;           (bit-clear! bb j)))
;;     bb)))

;; (do (sieve 1e6) nil)

;; (println "###############################")
;; (println (range 4 (inc 9) 2))
;; (println "###############################")

;; (let [n 9
;;       bb (sieve n)]
;;   (doseq [bit (range 1 (inc n))]
;;     (println (str "bit-set?:" bit ":" (bit-set? bb bit)))
;;     )
;;     )

;; (take-while #(not (= % -1)) 
;; (iterate 
;;  #(.nextSetBit theBitSet (inc %)) 2))

;; ))
 