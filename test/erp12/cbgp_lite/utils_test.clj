(ns erp12.cbgp-lite.utils-test
  (:require [clojure.test :refer [deftest is]]
            [erp12.cbgp-lite.utils :as u]))

(deftest first-non-nil-test
  (is (= 1 (u/first-non-nil [nil 1 nil 2])))
  (is (= :x (u/first-non-nil '(nil :x))))
  (is (= :x (u/first-non-nil [:x nil :z]))))

(deftest safe-rand-nth-test
  (is (nil? (u/safe-rand-nth []))))

(deftest enhance-test
  (is (= {:a 0} (u/enhance {} :a count)))
  (is (= {:a 0 :b 1}
         (u/enhance {}
                  :a count
                  :b (fn [{:keys [a]}] (inc a))))))
