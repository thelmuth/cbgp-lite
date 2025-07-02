(ns erp12.cbgp-lite.lang.problem-tests
  (:require [clojure.string :as str]
           [clojure.test :refer :all]
           [clojure.walk :as w]
           [erp12.cbgp-lite.lang.ast :as a]
           [erp12.cbgp-lite.lang.compile :as c]
           [erp12.cbgp-lite.lang.lib :as lib]
           [erp12.cbgp-lite.lang.schema :as schema]
           [mb.hawk.core]
           [meander.epsilon :as m]))

;; Functional Programming
;; comp (comp2-fn1, comp3-fn1, comp2-fn2, comp3-fn2), partial
(deftest comp-test
  (testing "comp2-fn1-test"
    (println)
    (println "---STARTING COMP TEST---")
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :local :idx 0}
                                                    {:gene :var :name 'inc}
                                                    {:gene :var :name `lib/square}
                                                    {:gene :var :name 'comp}
                                                    {:gene :apply}
                                                    {:gene :apply}]
                                        :locals    ['in1]
                                        :ret-type  (lib/s-var 'a)
                                        :type-env  (assoc lib/type-env
                                                          'in1 {:type 'int?})
                                        :dealiases lib/dealiases}))
          _ (is (= 'int? (:type type)))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [~'in1] ~form))]
      (is (= 25 (func 4)))))
  
  (testing "comp3-fn1-test"
    (println)
    (println "---STARTING COMP TEST 2---")
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :local :idx 0}
                                                    {:gene :var :name `lib/square}
                                                    {:gene :var :name 'inc}
                                                    {:gene :var :name 'inc}
                                                    {:gene :var :name 'comp}
                                                    {:gene :apply}
                                                    {:gene :apply}]
                                        :locals    ['in1]
                                        :ret-type  (lib/s-var 'a)
                                        :type-env  (assoc lib/type-env
                                                          'in1 {:type 'int?})
                                        :dealiases lib/dealiases}))
          _ (is (= 'int? (:type type)))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [~'in1] ~form))]
      (is (= 18 (func 4)))))
  
  (testing "comp2-fn2-test"
    (println)
    (println "---STARTING COMP TEST 3---")
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :local :idx 1}
                                                    {:gene :local :idx 0}
                                                    {:gene :var :name '+}
                                                    {:gene :var :name 'dec}
                                                    {:gene :var :name 'comp}
                                                    {:gene :apply}
                                                    {:gene :apply}]
                                        :locals    ['in1 'in2 'in3]
                                        :ret-type  (lib/s-var 'a)
                                        :type-env  (assoc lib/type-env
                                                          'in1 {:type 'int?}
                                                          'in2 {:type 'int?})
                                        :dealiases lib/dealiases}))
          _ (is (= 'int? (:type type)))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [~'in1 ~'in2] ~form))]
      (is (= 14 (func 10 5)))))

  (testing "comp3-fn2-test"
    (println)
    (println "---STARTING COMP TEST 4---")
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :local :idx 2}
                                                    {:gene :local :idx 1}
                                                    {:gene :local :idx 0}
                                                    {:gene :var :name '+}
                                                    {:gene :var :name 'dec}
                                                    {:gene :var :name 'str}
                                                    {:gene :var :name 'comp}
                                                    {:gene :apply}
                                                    {:gene :apply}]
                                        :locals    ['in1 'in2 'in3]
                                        :ret-type  (lib/s-var 'a)
                                        :type-env  (assoc lib/type-env
                                                          'in1 {:type 'int?}
                                                          'in2 {:type 'int?}
                                                          'in3 {:type 'int?})
                                        :dealiases lib/dealiases}))
          _ (is (= 'string? (:type type)))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [~'in1 ~'in2 ~'in3] ~form))]
      (is (= "14" (func 10 5 1))))) 
  )

(deftest partial-test
  ; partial (partial1-fn2, partial1-fn3, partial2-fn3)
  ; target form: ((partial * 100) 5)
  (testing "partial1-fn2-test"
    (println)
    (println "---STARTING PARTIAL 1 TEST---")
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :local :idx 0}
                                                    {:gene :lit :val 100 :type {:type 'int?}}
                                                    {:gene :var :name '*}
                                                    {:gene :var :name 'partial}
                                                    {:gene :apply}
                                                    {:gene :apply}]
                                        :locals    ['in1]
                                        :ret-type  (lib/s-var 'a)
                                        :type-env  (assoc lib/type-env
                                                          'in1 {:type :s-var :sym 'a})
                                        :dealiases lib/dealiases}))
          _ (is (= 'int? (:type type)))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [~'in1] ~form))]
      (is (= 500 (func 5)))))

  ; target form: ((partial assoc {:a 1 :b 2 :c 3} :z) 42)
  (testing "partial1-fn3-test"
    (println)
    (println "---STARTING PARTIAL 2 TEST---")
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :local :idx 0}
                                                    {:gene :lit :val \z :type {:type 'char?}}
                                                    {:gene :lit :val {\a 1 \b 2 \c 3} :type {:type :map-of :key {:type 'char?} :value {:type 'int?}}}
                                                    {:gene :var :name 'assoc}
                                                    {:gene :var :name 'partial}
                                                    {:gene :apply}
                                                    {:gene :apply}]
                                        :locals    ['in1]
                                        :ret-type  {:type :map-of :key {:type 'char?} :value {:type 'int?}}
                                        :type-env  (assoc lib/type-env
                                                          'in1 {:type 'int?})
                                        :dealiases lib/dealiases}))
          _ (is (= :map-of (:type type)))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [~'in1] ~form))]
       (is (= {\a 1 \b 2 \c 3 \z 42} (func 42)))))

  ; target form: ((partial test3 param1 param2) param3)
  ;          eg. ((partial assoc {:a 1 :b 2 :c 3} :z) 42)
  (testing "partial2-fn3-test"
    (println)
    (println "---STARTING PARTIAL 3 TEST---")
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :local :idx 1}
                                                    {:gene :local :idx 0}
                                                    {:gene :lit :val {0 42 1 2999 2 108} :type {:type :map-of :key {:type 'int?} :value {:type 'int?}}}
                                                    {:gene :var :name 'assoc}
                                                    {:gene :var :name 'partial}
                                                    {:gene :apply}
                                                    {:gene :apply}]
                                        :locals    ['in1 'in2]
                                        :ret-type  {:type :map-of :key {:type 'int?} :value {:type 'int?}}
                                        :type-env  (assoc lib/type-env
                                                          'in1 {:type 'int?}
                                                          'in2 {:type 'int?})
                                        :dealiases lib/dealiases}))
          _ (is (= :map-of (:type type)))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [~'in1 ~'in2] ~form))]
      (is (= {0 42 1 2999 2 108 3 42} (func 3 42))))
    ))



  

