(ns lib.bitbag)

(defn- bit-meta 
  "Bit index metadata for internal lookup"
  [bit-idx]
  {:pre [(>= bit-idx 0)]}
  (let [bit-n (inc bit-idx)
        q (quot bit-n 8)
        r (rem bit-n 8)
        seg-idx (if (= 0 r) (dec q) q)
        bit-idx (dec (- bit-n (* seg-idx 8)))]
    {:seg-idx seg-idx
     :bit-idx bit-idx}))

(defn- bit-last-idx
  "Returns the last index the given bitbag is capable of recording."
  [bitbag]
  (-> (count bitbag)
      (* 8)
      dec))

(defn new-bitbag
  "Based on java.util.BitSet single arg constructor.
  (However this lib sets it's bit bag as a fixed size, i.e. not growable).
  Creates a bit bag whose size is large enough to explicitly represent bits with indices in the range 0 through nbits-1.
  Bit bags just use the hosting environments native byte arrays to manage a fixed size of unchecked bytes to track the bits (unchecked as we want to use all 8 bits in the byte without errors being thrown from the hosting environment.
  "
  [nbits]
  {:pre [(> nbits 0)]}
  (byte-array
    (replicate
      (inc (:seg-idx (bit-meta (dec nbits))))
          (unchecked-byte 0))))

(defn- bit-idx-mutation! [bitbag bit-idx bit-op-fn]
  {:pre 
    [(>= bit-idx 0)
     (<= (:seg-idx (bit-meta bit-idx)) (count bitbag))]}
  (let [bm (bit-meta bit-idx)
        byte-seg-orig (aget bitbag (:seg-idx bm))
        byte-seg-new (unchecked-byte (bit-op-fn byte-seg-orig (:bit-idx bm)))]
      (aset bitbag (:seg-idx bm) (byte byte-seg-new))))

(defn- bit-idx-range-operation! [bitbag from-idx to-idx bit-op-fn]
  {:pre 
    [(>= from-idx 0)
     (>= to-idx from-idx)
     (<= (:seg-idx (bit-meta to-idx)) (count bitbag))]}
  (if (< from-idx to-idx)
    (loop [bit-idxs (range from-idx to-idx)
            bit-idx (first bit-idxs)
            {:keys [seg-idx bit-idx]} (bit-meta bit-idx)]
        ;; perform bit-op-fn on current segment
        (let [byte-seg-orig (aget bitbag seg-idx)
              byte-seg-new (unchecked-byte (bit-op-fn byte-seg-orig bit-idx))]
          (aset bitbag seg-idx (unchecked-byte byte-seg-new)))
        ;; decide if we continue processing bits, and recur if so
        (let [remaining-bit-idxs (rest bit-idxs)
              next-bit-idx (first remaining-bit-idxs)]
          (if next-bit-idx
            (recur remaining-bit-idxs next-bit-idx (bit-meta next-bit-idx)))))))


(defn bit-flip!
  "Based on: java.util.BitSet.flip methods.
  Flips bit value(s) 0 to 1, or 1 to 0 in a given bitbag, mutating bitbag in place (no return val).
  For arity [bitbag bit-idx]: TODO: implement this arity
     Sets the bit at the specified index to the complement of its current value.
  For arity [bitbag from-idx to-idx]:
     Sets each bit from the specified from-idx (inclusive) to the specified to-idx (exclusive) to the complement of its current value."
  ([bitbag bit-idx]
  (bit-idx-mutation! bitbag bit-idx bit-flip))

  ([bitbag from-idx to-idx]
  (bit-idx-range-operation! bitbag from-idx to-idx bit-flip)))


(defn bit-clear!
  "Based on: java.util.BitSet.clear methods.
  Clears bit value(s) to 0 in a given bitbag, mutating bitbag in place (no return val).
  For arity [bitbag bit-idx]:
  Sets the bit specified by the index to false.
  For arity [bitbag from-idx to-idx]:
  Sets the bits from the specified from-idx (inclusive) to the specified to-idx (exclusive) to false."
  ([bitbag bit-idx]
  (bit-idx-mutation! bitbag bit-idx bit-clear))
  
  ([bitbag from-idx to-idx]
  (bit-idx-range-operation! bitbag from-idx to-idx bit-clear)))

(defn bit-set!
  "Based on: java.util.BitSet.set methods.
  Sets bit value(s) to 1 in a given bitbag, mutating bitbag in place (no return val).
  For arity [bitbag bit-idx]:
  Sets the bit specified by the index to false.
  For arity [bitbag from-idx to-idx]:
  Sets the bits from the specified from-idx (inclusive) to the specified to-idx (exclusive) to false."
  ([bitbag bit-idx]
  (bit-idx-mutation! bitbag bit-idx bit-set))
  
  ([bitbag from-idx to-idx]
  (bit-idx-range-operation! bitbag from-idx to-idx bit-set)))


(defn bit-get
  "Based on: java.util.BitSet.get methods.
  For arity [bitbag bit-idx]:
  Returns the value of the bit with the specified index.
  For arity [bitbag from-idx to-idx]: TODO: implement...
  Returns a new BitSet composed of bits from this BitSet from from-idx (inclusive) to to-idx (exclusive).
  "
  [bitbag bit-idx]
  {:pre 
    [(>= bit-idx 0)
     (<= (:seg-idx (bit-meta bit-idx)) (count bitbag))]}
 (let [bm (bit-meta bit-idx)]
    (bit-test (aget bitbag (:seg-idx bm)) (:bit-idx bm))))


(defn bit-idx-predicate?
  ([bitbag from-idx pred?]
    (let [to-idx (inc (bit-last-idx bitbag))]
      (bit-idx-predicate? bitbag from-idx to-idx pred?)))

  ([bitbag from-idx to-idx pred?]
  {:pre 
    [(>= from-idx 0)
     (>= to-idx from-idx)
     (<= (:seg-idx (bit-meta to-idx)) (count bitbag))]}
  (if (< from-idx to-idx)
    (loop [bit-idxs (range from-idx to-idx)
          bit-idx (first bit-idxs)
          {:keys [seg-idx bit-idx]} (bit-meta bit-idx)]
      ;; perform pred? logic on bit, and return idx if set
      (let [byte-seg (aget bitbag seg-idx)
            bit-matches-pred (bit-test byte-seg bit-idx)]
        (if bit-matches-pred
          (+ bit-idx (* seg-idx 8))
          ;; decide if we continue processing bits, and recur, else return -1
          (let [remaining-bit-idxs (rest bit-idxs)
                next-bit-idx (first remaining-bit-idxs)]
            (if next-bit-idx
              (recur remaining-bit-idxs next-bit-idx (bit-meta next-bit-idx))
              -1))))))))

(defn bit-next-set
  "Based on: java.util.BitSet.nextSetBit method (with an addition of an ending index).
  For arity [bitbag from-idx]:
  Returns the index of the first bit that is set to true that occurs on or after the specified starting index.
  For arity [bitbag from-idx to-idx]:
  Returns the index of the first bit that is set to true that occurs on or after the specified starting index (inclusive) to the specified ending index (exclusive).
  "
  ([bitbag from-idx]
    (bit-idx-predicate? bitbag from-idx bit-set))

  ([bitbag from-idx to-idx]
    (bit-idx-predicate? bitbag from-idx to-idx bit-set)))

(defn bit-next-clear
  "Based on: java.util.BitSet.nextClearBit method (with an addition of an ending index).
  For arity [bitbag from-idx]:
  Returns the index of the first bit that is set to false that occurs on or after the specified starting index.
  For arity [bitbag from-idx to-idx]:
  Returns the index of the first bit that is set to false that occurs on or after the specified starting index (inclusive) to the specified ending index (exclusive).
  "
  ([bitbag from-idx]
    (bit-idx-predicate? bitbag from-idx bit-set))

  ([bitbag from-idx to-idx]
    (bit-idx-predicate? bitbag from-idx to-idx bit-set)))