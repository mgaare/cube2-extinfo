(ns cube2-extinfo.deserialise-test
  (:require [clojure.test :refer :all])
  (:refer-clojure :exclude (flush repeat))
  (:use cube2-extinfo.deserialise))

(deftest hmap-test
  (is (= (byte-seq (hmap :a (cstring) :b (int-d))
                   (-> (mapv byte "hello")
                       (conj 0)
                       (concat [0 0 1 0])
                       (byte-array)))
         '({:a "hello" :b 256}))))

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

(deftest wrap-non-negative-test
  (let [nn-d (wrap-non-negative (byte-d))]
    (is (= (deserialise nn-d 1)
           [1 nil]))
    (is (= (deserialise nn-d 0)
           [0 nil]))
    (is (= (deserialise nn-d -1)
           [nil nil]))))
(deftest backtracking-alts-test
  (is (= (byte-seq (backtracking-alts
                    (hmap :a (byte-d)
                          :b (wrap-required (byte-d) 3)
                          :c (byte-d))
                    (hmap :d (byte-d)
                          :e (wrap-required (byte-d) 2)
                          :f (byte-d)))
                   (byte-array [1 2 3]))
         '({:d 1 :e 2 :f 3}))))
