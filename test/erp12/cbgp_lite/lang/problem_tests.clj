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
; working w/ normal order
; (broken) reversed schema order: (comp erp12.cbgp-lite.lang.lib/square inc in1)
;; (deftest comp2-fn1-test
;;   (println)
;;   (println "---STARTING COMP TEST---")
;;   (let [{::c/keys [ast type]} (:ast (c/push->ast
;;                                      {:push      [{:gene :local :idx 0}
;;                                                   {:gene :var :name 'inc}
;;                                                   {:gene :var :name `lib/square}
;;                                                   {:gene :var :name 'comp}
;;                                                   {:gene :apply}
;;                                                   {:gene :apply}]
;;                                       :locals    ['in1]
;;                                       :ret-type  (lib/s-var 'a)
;;                                       :type-env  (assoc lib/type-env
;;                                                         'in1 {:type :s-var :sym 'a}) 
;;                                       :dealiases lib/dealiases}))
;;         _ (is (= :s-var (:type type)))
;;         _ (println "REAL-AST: " ast)
;;         form (a/ast->form ast)
;;         _ (println "FORM: " form)
;;         func (eval `(fn [~'in1] ~form))]
;;     (is (= 25 (func 4)))))

;; ; broken, wrong form: ((comp erp12.cbgp-lite.lang.lib/square inc) in1)
;; ; reversed schema order: ((comp erp12.cbgp-lite.lang.lib/square inc inc) in1)
;; (deftest comp3-fn1-test
;;   (println)
;;   (println "---STARTING COMP TEST 2---")
;;   (let [{::c/keys [ast type]} (:ast (c/push->ast
;;                                      {:push      [{:gene :local :idx 0}
;;                                                   {:gene :var :name 'inc}
;;                                                   {:gene :var :name 'inc}
;;                                                   {:gene :var :name `lib/square}
;;                                                   {:gene :var :name 'comp}
;;                                                   {:gene :apply}
;;                                                   {:gene :apply}]
;;                                       :locals    ['in1]
;;                                       :ret-type  (lib/s-var 'a)
;;                                       :type-env  (assoc lib/type-env
;;                                                         'in1 {:type 'int?}
;;                                                         ;'in1 {:type :s-var :sym 'a}
;;                                                         )
;;                                       :dealiases lib/dealiases}))
;;         _ (is (= :s-var (:type type)))
;;         _ (println "REAL-AST: " ast)
;;         form (a/ast->form ast)
;;         _ (println "FORM: " form)
;;         func (eval `(fn [~'in1] ~form))]
;;     (is (= 26 (func 4)))))

;; ; broken, wrong form: ((comp in1 in2) +)  <- what.
;; ; reversed schema order: (comp in1 in2 +) <- w h a t.
;; (deftest comp2-fn2-test
;;   (println)
;;   (println "---STARTING COMP TEST 3---")
;;   (let [{::c/keys [ast type]} (:ast (c/push->ast
;;                                      {:push      [{:gene :local :idx 2}
;;                                                   {:gene :local :idx 1}
;;                                                   {:gene :local :idx 0}
;;                                                   {:gene :var :name '-}
;;                                                   {:gene :var :name '+}
;;                                                   {:gene :var :name 'comp}
;;                                                   {:gene :apply}
;;                                                   {:gene :apply}]
;;                                       :locals    ['in1 'in2 'in3]
;;                                       :ret-type  (lib/s-var 'a)
;;                                       :type-env  (assoc lib/type-env
;;                                                         'in1 {:type :s-var :sym 'a}
;;                                                         'in2 {:type :s-var :sym 'a}
;;                                                         'in3 {:type :s-var :sym 'a}
;;                                                         )
;;                                       :dealiases lib/dealiases}))
;;         _ (is (= 'int? (:type type)))
;;         _ (println "REAL-AST: " ast)
;;         form (a/ast->form ast)
;;         _ (println "FORM: " form)
;;         func (eval `(fn [~'in1 ~'in2] ~form))]
;;     (is (= 14 (func 10 5 1)))))

;; ; broken, wrong form: ((comp str in1) +)
;; ; reversed schema order:  ((comp str in1 +) in2 in3)
;; (deftest comp3-fn2-test
;;   (println)
;;   (println "---STARTING COMP TEST 4---")
;;   (let [{::c/keys [ast type]} (:ast (c/push->ast
;;                                      {:push      [{:gene :local :idx 2}
;;                                                   {:gene :local :idx 1}
;;                                                   {:gene :local :idx 0}
;;                                                   {:gene :var :name 'str}
;;                                                   {:gene :var :name '-}
;;                                                   {:gene :var :name '+}
;;                                                   {:gene :var :name 'comp}
;;                                                   {:gene :apply}
;;                                                   {:gene :apply}]
;;                                       :locals    ['in1 'in2 'in3]
;;                                       :ret-type  (lib/s-var 'a)
;;                                       :type-env  (assoc lib/type-env
;;                                                         'in1 {:type :s-var :sym 'a}
;;                                                         'in2 {:type :s-var :sym 'a}
;;                                                         'in3 {:type :s-var :sym 'a})
;;                                       :dealiases lib/dealiases}))
;;         _ (is (= 'string? (:type type)))
;;         _ (println "REAL-AST: " ast)
;;         form (a/ast->form ast)
;;         _ (println "FORM: " form)
;;         func (eval `(fn [~'in1 ~'in2 ~'in3] ~form))]
;;     (is (= "14" (func 10 5 1)))))

;; ; partial (partial1-fn2, partial1-fn3, partial2-fn3)
;; ; works
;; ; (broken) reverse schema order: (partial in1 * 100)
;; (deftest partial1-fn2-test
;;     (println)
;;   (println "---STARTING PARTIAL 1 TEST---")
;;   (let [{::c/keys [ast type]} (:ast (c/push->ast
;;                                      {:push      [{:gene :local :idx 0}
;;                                                   {:gene :lit :val 100 :type {:type 'int?}}
;;                                                   {:gene :var :name '*}
;;                                                   {:gene :var :name 'partial}
;;                                                   {:gene :apply}
;;                                                   {:gene :apply}]
;;                                       :locals    ['in1]
;;                                       :ret-type  (lib/s-var 'a)
;;                                       :type-env  (assoc lib/type-env
;;                                                         'in1 {:type :s-var :sym 'a})
;;                                       :dealiases lib/dealiases}))
;;         _ (is (= 'int? (:type type)))
;;         _ (println "REAL-AST: " ast)
;;         form (a/ast->form ast)
;;         _ (println "FORM: " form)
;;         func (eval `(fn [~'in1] ~form))]
;;     (is (= 500 (func 5)))))

   ; broken, wrong form: ((partial * 100) in1)
   ; reverse schema order: ((partial in1 count *) 100)
  (testing "partial1-fn3-test"
    (println)
    (println "---STARTING PARTIAL 2 TEST---")
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :local :idx 0}
                                                    {:gene :lit :val 100 :type {:type 'int?}}
                                                    {:gene :var :name '*}
                                                    {:gene :var :name 'count}
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
      (is (= 500 (func [1 2 3 4 5])))))
  
  ; broken, wrong form: ((partial * 100) in1)
  ; reverse schema order: ((partial in1 count *) 100)
  ; target form: ((partial test +) 1 2 3)
  (testing "partial2-fn3-test"
    (println)
    (println "---STARTING PARTIAL 3 TEST---")
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :local :idx 2}
                                                    {:gene :local :idx 1}
                                                    {:gene :local :idx 0}
                                                  ;{:gene :lit :val 100 :type {:type 'int?}}
                                                    {:gene :var :name '+}
                                                    {:gene :fn :arg-types [lib/INT lib/INT lib/INT] :ret-type lib/INT}
                                                    [{:gene :local :idx 2}
                                                     {:gene :var :name 'inc}
                                                     {:gene :apply}
                                                     {:gene :local :idx 1}
                                                     {:gene :var :name 'inc}
                                                     {:gene :apply}
                                                     {:gene :local :idx 0}
                                                     {:gene :var :name 'inc}
                                                     {:gene :apply}]
                                                    {:gene :var :name 'partial}
                                                    {:gene :apply}
                                                    {:gene :apply}]
                                        :locals    ['in1 'in2]
                                        :ret-type  (lib/s-var 'a)
                                        :type-env  (assoc lib/type-env
                                                          'in1 {:type :s-var :sym 'a}
                                                          'in2 {:type :s-var :sym 'a}
                                                          'in3 {:type :s-var :sym 'a})
                                        :dealiases lib/dealiases}))
          _ (is (= 'int? (:type type)))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [~'in1 ~'in2 ~'in3] ~form))]
      (is (= 45 (func 10 30 5)))))
