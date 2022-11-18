(ns test
  (:require [clojure.test :as t]
            [lib.bitbag-tests]))

(def test-results
  (t/run-tests 'lib.bitbag-tests))

(def failures-and-errors
  (let [{:keys [:fail :error]} test-results]
    (+ fail error)))

(System/exit failures-and-errors)