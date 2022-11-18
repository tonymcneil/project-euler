(ns lib.bitbag-tests
  (:require
    [lib.bitbag :refer :all]
    [clojure.test :refer :all]
    [clojure.pprint :refer :all]
    ;; example printing:
    ;; (println (cl-format nil "2r~8,'0',B" (unchecked-byte (aget bitbag 0))))))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bit-meta
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest bit-meta-tests
  (testing "bit-meta variations"
    ;; first segment  
    (is (= {:seg-idx 0 :bit-idx 0} (bit-meta 0)))
    (is (= {:seg-idx 0 :bit-idx 1} (bit-meta 1)))
    (is (= {:seg-idx 0 :bit-idx 2} (bit-meta 2)))
    (is (= {:seg-idx 0 :bit-idx 3} (bit-meta 3)))
    (is (= {:seg-idx 0 :bit-idx 4} (bit-meta 4)))
    (is (= {:seg-idx 0 :bit-idx 5} (bit-meta 5)))
    (is (= {:seg-idx 0 :bit-idx 6} (bit-meta 6)))
    (is (= {:seg-idx 0 :bit-idx 7} (bit-meta 7)))
    ;; second segment
    (is (= {:seg-idx 1 :bit-idx 0} (bit-meta 8)))
    (is (= {:seg-idx 1 :bit-idx 1} (bit-meta 9)))
    (is (= {:seg-idx 1 :bit-idx 2} (bit-meta 10)))
    (is (= {:seg-idx 1 :bit-idx 3} (bit-meta 11)))
    (is (= {:seg-idx 1 :bit-idx 4} (bit-meta 12)))
    (is (= {:seg-idx 1 :bit-idx 5} (bit-meta 13)))
    (is (= {:seg-idx 1 :bit-idx 6} (bit-meta 14)))
    (is (= {:seg-idx 1 :bit-idx 7} (bit-meta 15)))
    ;; third segment
    (is (= {:seg-idx 2 :bit-idx 0} (bit-meta 16)))
    ;; tenth segment
    (is (= {:seg-idx 9 :bit-idx 7} (bit-meta 79))))
  
  (testing "bit-meta pre-condition assertion"
    (is (= "Assert failed: (>= bit-idx 0)"
      (try
        (bit-meta -1)
        #?(:clj (catch java.lang.AssertionError e (.getMessage e))
          ;; TODO: fix if ths doesn't work on cljs...
          :cljs (catch js/Error e (.message e))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new-bitbag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest new-bitbag-tests
  (testing "new-bitbag single segment"
    (let [bitbag (new-bitbag 8)]
    (is (= 1 (count bitbag)))
    (is (every? zero? bitbag))))
  
  (testing "new-bitbag multiple segments"
    (let [bitbag (new-bitbag 80)]
    (is (= 10 (count bitbag)))
    (is (every? zero? bitbag))))
    
  (testing "new-bitbag pre-condition assertion"
    (is (= "Assert failed: (> nbits 0)"
      (try
        (new-bitbag 0)
        #?(:clj (catch java.lang.AssertionError e (.getMessage e))
          ;; TODO: fix if ths doesn't work on cljs...
          :cljs (catch js/Error e (.message e))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bit-last-idx
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest bit-last-idx-test
  (testing "bit-last-idx single segment"
    (is (= 7 (bit-last-idx (new-bitbag 1)))))

  (testing "bit-last-idx multi segment"
    (is (= 15 (bit-last-idx (new-bitbag 16))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bit-flip!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest bit-flip!-tests
  (testing "bit-flip! single bit on single segment (first bit)"
    ;; given
    (let [bitbag (new-bitbag 8)]
      ;; when 
      (bit-flip! bitbag 0)
      ;; then
      (is (= (aget bitbag 0) (unchecked-byte 2r00000001)))))
      
  (testing "bit-flip! single bit on single segment (last bit)"
    ;; given
    (let [bitbag (new-bitbag 8)]
      ;; when 
      (bit-flip! bitbag 7)
      ;; then
      (is (= (aget bitbag 0) (unchecked-byte 2r10000000)))))
      
  (testing "bit-flip! single bit on multiple segment (last bit)"
    ;; given
    (let [bitbag (new-bitbag 16)]
      ;; when 
      (bit-flip! bitbag 15)
      ;; then
      (is (= (aget bitbag 0) (unchecked-byte 2r00000000)))
      (is (= (aget bitbag 1) (unchecked-byte 2r10000000)))))

  (testing "bit-flip! range of no bits on single segment (first bit)"
    (let [bitbag (new-bitbag 8)]
      ;; flip on
      (bit-flip! bitbag 0 0)
      (is (= (aget bitbag 0) 2r00000000))
      ;; flip off
      (bit-flip! bitbag 0 0)
      (is (= (aget bitbag 0) 2r00000000))))

  (testing "bit-flip! range of single bit on single segment (first bit)"
    (let [bitbag (new-bitbag 8)]
      ;; flip on
      (bit-flip! bitbag 0 1)
      (is (= (aget bitbag 0) 2r00000001))
      ;; flip off
      (bit-flip! bitbag 0 1)
      (is (= (aget bitbag 0) 2r00000000))))

  (testing "bit-flip! range of single bit on single segment (last bit)"
    (let [bitbag (new-bitbag 8)]
      ;; flip on
      (bit-flip! bitbag 7 8)
      (is (= (aget bitbag 0) (unchecked-byte 2r10000000)))
      ;; flip off
      (bit-flip! bitbag 7 8)
      (is (= (aget bitbag 0) (unchecked-byte 2r00000000)))
      ))

  (testing "bit-flip! range over entire single segment"
    (let [bitbag (new-bitbag 8)]
      ;; flip on      
      (bit-flip! bitbag 0 8)
      (is (= (aget bitbag 0) (unchecked-byte 2r11111111)))
      ;; flip off
      (bit-flip! bitbag 0 8)
      (is (= (aget bitbag 0) (unchecked-byte 2r00000000)))))

  (testing "bit-flip! range over entire multiple segments"
    (let [bitbag (new-bitbag 16)]
      ;; flip on      
      (bit-flip! bitbag 0 16)
      (is (= (aget bitbag 0) (unchecked-byte 2r11111111)))
      (is (= (aget bitbag 1) (unchecked-byte 2r11111111)))
      ;; flip off
      (bit-flip! bitbag 0 16)
      (is (= (aget bitbag 0) (unchecked-byte 2r00000000)))
      (is (= (aget bitbag 1) (unchecked-byte 2r00000000)))))

  (testing "bit-flip! partial range over single segment"
    (let [bitbag (new-bitbag 8)]
      ;; flip on 
      (bit-flip! bitbag 2 6)
      (is (= (aget bitbag 0) 2r00111100))
      ;; flip off
      (bit-flip! bitbag 2 6)
      (is (= (aget bitbag 0) 2r00000000))))

  (testing "bit-flip! multiple ranges over single segment"
    (let [bitbag (new-bitbag 8)]
      ;; flip on
      (bit-flip! bitbag 1 3)
      (bit-flip! bitbag 4 6)
      (is (= (aget bitbag 0) 2r00110110))
      ;; flip off
      (bit-flip! bitbag 1 3)
      (bit-flip! bitbag 4 6)
      (is (= (aget bitbag 0) 2r00000000))))

  (testing "bit-flip! multiple ranges over multiple segments"
    (let [bitbag (new-bitbag 16)]
      ;; flip on
      (bit-flip! bitbag 1 3)
      (bit-flip! bitbag 4 6)
      (bit-flip! bitbag 8 9)
      (bit-flip! bitbag 15 16)
      (is (= (aget bitbag 0) 2r00110110))
      (is (= (aget bitbag 1) (unchecked-byte 2r10000001)))
      ;; flip off
      (bit-flip! bitbag 1 3)
      (bit-flip! bitbag 4 6)
      (bit-flip! bitbag 8 9)
      (bit-flip! bitbag 15 16)
      (is (= (aget bitbag 0) 2r00000000))
      (is (= (aget bitbag 1) (unchecked-byte 2r00000000)))))

  (testing "bit-flip! pre-condition assertions"
    (let [bitbag (new-bitbag 1)]
        (is (= "Assert failed: (>= bit-idx 0)"
          (try
            (bit-flip! bitbag -1)
            #?(:clj (catch java.lang.AssertionError e (.getMessage e))
              ;; TODO: fix if ths doesn't work on cljs...
              :cljs (catch js/Error e (.message e)))))))

    (let [bitbag (new-bitbag 1)]
      (is (= "Assert failed: (>= from-idx 0)"
        (try
          (bit-flip! bitbag -1 0)
          #?(:clj (catch java.lang.AssertionError e (.getMessage e))
            ;; TODO: fix if ths doesn't work on cljs...
            :cljs (catch js/Error e (.message e)))))))

    (let [bitbag (new-bitbag 1)]
      (is (= "Assert failed: (>= to-idx from-idx)"
        (try
          (bit-flip! bitbag 2 1)
          #?(:clj (catch java.lang.AssertionError e (.getMessage e))
            ;; TODO: fix if ths doesn't work on cljs...
            :cljs (catch js/Error e (.message e)))))))

    (let [bitbag (new-bitbag 1)]
      (is (= "Assert failed: (<= (:seg-idx (bit-meta to-idx)) (count bitbag))"
        (try
          (bit-flip! bitbag 0 999)
          #?(:clj (catch java.lang.AssertionError e (.getMessage e))
            ;; TODO: fix if ths doesn't work on cljs...
            :cljs (catch js/Error e (.message e)))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bit-clear!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest bit-clear!-tests
  (testing "bit-clear! single bit on single segment (first bit)"
    ;; given
    (let [bitbag (byte-array [2r00000001])]
      ;; when 
      (bit-clear! bitbag 0)
      ;; then
      (is (= (aget bitbag 0) (unchecked-byte 2r00000000)))))

  (testing "bit-clear! single bit on single segment (last bit)"
    ;; given
    (let [bitbag (byte-array [2r10000000])]
      ;; when 
      (bit-clear! bitbag 7)
      ;; then
      (is (= (aget bitbag 0) (unchecked-byte 2r00000000)))))
   
  (testing "bit-clear! single bit on multiple segment (last bit)"
    ;; given
    (let [bitbag (byte-array [2r00000000 2r10000000])]
      ;; when 
      (bit-clear! bitbag 15)
      ;; then
      (is (= (aget bitbag 0) (unchecked-byte 2r00000000)))
      (is (= (aget bitbag 1) (unchecked-byte 2r00000000)))))

  (testing "bit-clear! range of no bits on single segment (first bit)"
    ;; given
    (let [bitbag (byte-array [2r00000001])]
      ;; when
      (bit-clear! bitbag 0 0)
      ;; then
      (is (= (aget bitbag 0) 2r00000001))))

  (testing "bit-clear! range of single bit on single segment (first bit)"
    ;; given
    (let [bitbag (byte-array [2r00000001])]
      ;; when
      (bit-clear! bitbag 0 1)
      ;; then
      (is (= (aget bitbag 0) 2r00000000))))

  (testing "bit-clear! range of single bit on single segment (last bit)"
    ;; given
    (let [bitbag (byte-array [2r10000000])]
      ;; when
      (bit-clear! bitbag 7 8)
      ;; then
      (is (= (aget bitbag 0) (unchecked-byte 2r00000000)))))

  (testing "bit-clear! range over entire single segment"
    ;; given
    (let [bitbag (byte-array [2r11111111])]
      ;; when
      (bit-clear! bitbag 0 8)
      ;; then
      (is (= (aget bitbag 0) (unchecked-byte 2r00000000)))))

  (testing "bit-clear! range over entire multiple segments"
    ;; given
    (let [bitbag (byte-array [2r11111111 2r11111111])]
      ;; when
      (bit-clear! bitbag 0 16)
      ;; then
      (is (= (aget bitbag 0) (unchecked-byte 2r00000000)))
      (is (= (aget bitbag 1) (unchecked-byte 2r00000000)))))

  (testing "bit-clear! partial range over single segment"
    ;; given
    (let [bitbag (byte-array [2r00111100])]
      ;; when
      (bit-clear! bitbag 2 6)
      ;; then
      (is (= (aget bitbag 0) 2r00000000))))

  (testing "bit-clear! multiple ranges over single segment"
    ;; given
    (let [bitbag (byte-array [2r00110110])]
      ;; when
      (bit-clear! bitbag 1 3)
      (bit-clear! bitbag 4 6)
      ;; then
      (is (= (aget bitbag 0) 2r00000000))))

  (testing "bit-clear! multiple ranges over multiple segments"
    ;; given
    (let [bitbag (byte-array [2r00110110 2r10000001])]
      ;; when
      (bit-clear! bitbag 1 3)
      (bit-clear! bitbag 4 6)
      (bit-clear! bitbag 8 9)
      (bit-clear! bitbag 15 16)
      ;; then
      (is (= (aget bitbag 0) 2r00000000))
      (is (= (aget bitbag 1) (unchecked-byte 2r00000000))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bit-set!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest bit-set!-tests
  (testing "bit-set! single bit on single segment (first bit)"
    ;; given
    (let [bitbag (new-bitbag 1)]
      ;; when 
      (bit-set! bitbag 0)
      ;; then
      (is (= (aget bitbag 0) (unchecked-byte 2r00000001)))))

  (testing "bit-set! single bit on single segment (last bit)"
    ;; given
    (let [bitbag (new-bitbag 1)]
      ;; when 
      (bit-set! bitbag 7)
      ;; then
      (is (= (aget bitbag 0) (unchecked-byte 2r10000000)))))

  (testing "bit-set! single bit on multiple segment (last bit)"
    ;; given
    (let [bitbag (new-bitbag 16)]
      ;; when 
      (bit-set! bitbag 15)
      ;; then
      (is (= (aget bitbag 0) (unchecked-byte 2r00000000)))
      (is (= (aget bitbag 1) (unchecked-byte 2r10000000)))))

  (testing "bit-set! range of no bits on single segment (first bit)"
    ;; given
    (let [bitbag (new-bitbag 8)]
      ;; when
      (bit-set! bitbag 0 0)
      ;; then
      (is (= (aget bitbag 0) 2r00000000))))

  (testing "bit-set! range of single bit on single segment (first bit)"
    ;; given
    (let [bitbag (new-bitbag 8)]
      ;; when
      (bit-set! bitbag 0 1)
      ;; then
      (is (= (aget bitbag 0) 2r00000001))))

  (testing "bit-set! range of single bit on single segment (last bit)"
    ;; given
    (let [bitbag (new-bitbag 8)]
      ;; when
      (bit-set! bitbag 7 8)
      ;; then
      (is (= (aget bitbag 0) (unchecked-byte 2r10000000)))))

  (testing "bit-set! range over entire single segment"
    ;; given
    (let [bitbag (new-bitbag 8)]
      ;; when
      (bit-set! bitbag 0 8)
      ;; then
      (is (= (aget bitbag 0) (unchecked-byte 2r11111111)))))

  (testing "bit-set! range over entire multiple segments"
    ;; given
    (let [bitbag (new-bitbag 16)]
      ;; when
      (bit-set! bitbag 0 16)
      ;; then
      (is (= (aget bitbag 0) (unchecked-byte 2r11111111)))
      (is (= (aget bitbag 1) (unchecked-byte 2r11111111)))))

  (testing "bit-set! partial range over single segment"
    ;; given
    (let [bitbag (new-bitbag 8)]
      ;; when
      (bit-set! bitbag 2 6)
      ;; then
      (is (= (aget bitbag 0) 2r00111100))))

  (testing "bit-set! multiple ranges over single segment"
    ;; given
    (let [bitbag (new-bitbag 8)]
      ;; when
      (bit-set! bitbag 1 3)
      (bit-set! bitbag 4 6)
      ;; then
      (is (= (aget bitbag 0) 2r00110110))))

  (testing "bit-set! multiple ranges over multiple segments"
    ;; given
    (let [bitbag (new-bitbag 16)]
      ;; when
      (bit-set! bitbag 1 3)
      (bit-set! bitbag 4 6)
      (bit-set! bitbag 8 9)
      (bit-set! bitbag 15 16)
      ;; then
      (is (= (aget bitbag 0) 2r00110110))
      (is (= (aget bitbag 1) (unchecked-byte 2r10000001))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bit-get
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest bit-get-tests
  (testing "bit-get single bit on single segment (first bit)"
    ;; given
    (let [bitbag (byte-array [2r00000001])]
      ;; when, then
      (is (bit-get bitbag 0))))

  (testing "bit-get single bit on single segment (last bit)"
    ;; given
    (let [bitbag (byte-array [2r10000000])]
      ;; when, then
      (is (bit-get bitbag 7))))

  (testing "bit-get single bit on multiple segment (last bit)"
    ;; given
    (let [bitbag (byte-array [2r00000000 2r10000000])]
      ;; when, then
      (is (bit-get bitbag 15)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; TODO ....
;;   ;; (testing "bit-get range of single bit on single segment (first bit)"
;;   ;;   ;; given
;;   ;;   (let [bitbag (byte-array [2r00000001])]
;;   ;;     ;; when
;;   ;;     (bit-get bitbag 0 0)
;;   ;;     ;; then
;;   ;;     (is (= (aget bitbag 0) 2r00000000))))
;; )
;;   ;; (testing "bit-clear! range of single bit on single segment (last bit)"
;;   ;;   ;; given
;;   ;;   (let [bitbag (byte-array [2r10000000])]
;;   ;;     ;; when
;;   ;;     (bit-flip! bitbag 7 7)
;;   ;;     ;; then
;;   ;;     (is (= (aget bitbag 0) (unchecked-byte 2r00000000)))))

;;   ;; (testing "bit-clear! range over entire single segment"
;;   ;;   ;; given
;;   ;;   (let [bitbag (byte-array [2r11111111])]
;;   ;;     ;; when
;;   ;;     (bit-clear! bitbag 0 7)
;;   ;;     ;; then
;;   ;;     (is (= (aget bitbag 0) (unchecked-byte 2r00000000)))))

;;   ;; (testing "bit-clear! range over entire multiple segments"
;;   ;;   ;; given
;;   ;;   (let [bitbag (byte-array [2r11111111 2r11111111])]
;;   ;;     ;; when
;;   ;;     (bit-clear! bitbag 0 15)
;;   ;;     ;; then
;;   ;;     (is (= (aget bitbag 0) (unchecked-byte 2r00000000)))
;;   ;;     (is (= (aget bitbag 1) (unchecked-byte 2r00000000)))))


;;   ;; (testing "bit-clear! partial range over single segment"
;;   ;;   ;; given
;;   ;;   (let [bitbag (byte-array [2r00111100])]
;;   ;;     ;; when
;;   ;;     (bit-clear! bitbag 2 5)
;;   ;;     ;; then
;;   ;;     (is (= (aget bitbag 0) 2r00000000))))


;;   ;; (testing "bit-clear! multiple ranges over single segment"
;;   ;;   ;; given
;;   ;;   (let [bitbag (byte-array [2r00110110])]
;;   ;;     ;; when
;;   ;;     (bit-clear! bitbag 1 2)
;;   ;;     (bit-clear! bitbag 4 5)
;;   ;;     ;; then
;;   ;;     (is (= (aget bitbag 0) 2r00000000))))


;;   ;; (testing "bit-clear! multiple ranges over multiple segments"
;;   ;;   ;; given
;;   ;;   (let [bitbag (byte-array [2r00110110 2r10000001])]
;;   ;;     ;; when
;;   ;;     (bit-clear! bitbag 1 2)
;;   ;;     (bit-clear! bitbag 4 5)
;;   ;;     (bit-clear! bitbag 8 8)
;;   ;;     (bit-clear! bitbag 15 15)
;;   ;;     ;; then
;;   ;;     (is (= (aget bitbag 0) 2r00000000))
;;   ;;     (is (= (aget bitbag 1) (unchecked-byte 2r00000000))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bit-next-set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest bit-next-set-tests
  (testing "bit-next-set single bit on single segment"
    ;; given
    (let [bitbag (byte-array [2r01000001])]
      ;; when, then
      (is (= 0 (bit-next-set bitbag 0)))
      (is (= 6 (bit-next-set bitbag 1)))
      (is (= 6 (bit-next-set bitbag 6)))
      (is (= -1 (bit-next-set bitbag 7)))))

  (testing "bit-next-set single bit on multiple segments"
    ;; given
    (let [bitbag (byte-array [2r00000001 2r00000001])]
      ;; when, then
      (is (= 0 (bit-next-set bitbag 0)))
      (is (= 8 (bit-next-set bitbag 1)))))

  (testing "bit-next-set bit range on single segment"
    ;; given
    (let [bitbag (byte-array [2r00000001])]
      ;; when, then
      (is (= 0 (bit-next-set bitbag 0 1)))
      (is (= 0 (bit-next-set bitbag 0 2)))
      (is (= 0 (bit-next-set bitbag 0 8)))
      (is (= -1 (bit-next-set bitbag 1 1)))
      (is (= -1 (bit-next-set bitbag 1 7)))))
      
;;   (testing "bit-next-set bit range on multiple segments"
;;     ;; given
;;     (let [bitbag (byte-array [2r00000001 2r00000001 2r10000000])]
;;       ;; when, then
;;       (is (= 0 (bit-next-set bitbag 0 0)))
;;       ;; (is (= 0 (bit-next-set bitbag 0 1)))
;;       ;; (is (= 0 (bit-next-set bitbag 0 15)))
;;       ;; (is (= 8 (bit-next-set bitbag 1 15)))
;;       ;; (is (= 8 (bit-next-set bitbag 8 15)))
;;       ;; (is (= -1 (bit-next-set bitbag 1 7)))
;;       ;; (is (= -1 (bit-next-set bitbag 9 15)))
;;       ;; (is (= -1 (bit-next-set bitbag 15 15)))
;;       ;; (is (= -1 (bit-next-set bitbag 15 15)))
      
;;       ))
      
;;       )
      

