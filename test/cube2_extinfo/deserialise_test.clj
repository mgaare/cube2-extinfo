(ns cube2-extinfo.deserialise-test
  (:require [clojure.test :refer :all])
  (:refer-clojure :exclude (flush repeat))
  (:use cube2-extinfo.deserialise))

(deftest wrap-required-test
  (testing "single byte"
    (is (= (byte-seq (wrap-required (byte-d) 1) (byte-array [1]))
           '(1)))
    (is (= (byte-seq (wrap-required (byte-d) 1) (byte-array [0]))
           '())))
  (testing "multi byte"
    (is (= (byte-seq (wrap-required (byte-d) "hello")
                     (byte-array (conj (mapv byte "hello") 0))))
        '("hello"))
    (is (= (byte-seq (wrap-required (byte-d) "hello")
                     (byte-array (conj (mapv byte "bye") 0)))
           '()))))
