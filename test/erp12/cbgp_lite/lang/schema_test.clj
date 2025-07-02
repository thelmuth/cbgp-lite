(ns erp12.cbgp-lite.lang.schema-test
  (:require [clojure.test :refer [deftest is testing]]
            [erp12.cbgp-lite.lang.schema :as sch]
            [erp12.cbgp-lite.lang.lib :as lib]))

(deftest occurs?-test
  (is (sch/occurs? 'a {:op :local :name 'a}))
  (is (sch/occurs? {:type :s-var :sym 'A}
                   {:type   :=>
                    :input  {:type     :cat
                             :children [{:type :s-var :sym 'A}]}
                    :output {:type 'int?}}))
  (is (not (sch/occurs? 'x {:op :var :var '+})))
  (is (not (sch/occurs? {:type :s-var :sym 'A}
                        {:type   :=>
                         :input  {:type     :cat
                                  :children [{:type :s-var :sym 'B}]}
                         :output {:type 'int?}}))))

(deftest schema-terms-test
  (testing "No Typeclasses"
    (is (= #{:scheme :cat :=> :map-of :s-var} (sch/schema-terms (get lib/type-env 'get)))))
  
  (testing "Typeclasses")
  
  (testing "Overloaded"))