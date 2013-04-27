(ns clj-math.test.stats
  (:use [clj-math.stats])
  (:use [clojure.test]))

(deftest med 
  (is (= (median [1 2 3]) 2))
  (is (= (median [1 2 3 4]) 5/2)))
