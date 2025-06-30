(ns erp12.cbgp-lite.lang.ad-hoc-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [clojure.walk :as w]
            [erp12.cbgp-lite.lang.ast :as a]
            [erp12.cbgp-lite.lang.compile :as c]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.lang.schema :as schema]
            [mb.hawk.core]
            [meander.epsilon :as m]))

;; Indexing vectors and strings
;; First, second, last, rest, butlast, nth, 


;; Collection Conversion
;; Vec, Set, ->map
(deftest vec-cast-test
  (let [{::c/keys [ast type]} (:ast (c/push->ast
                                     {:push      [{:gene :lit :val 42 :type {:type 'int?}}
                                                  {:gene :local :idx 0}
                                                  {:gene :var :name 'vec}
                                                  {:gene :apply}]
                                      :locals    ['in1]
                                      :ret-type  (lib/s-var 'a)
                                      :type-env  (assoc lib/type-env
                                                        'in1 {:type :s-var :sym 'c :typeclasses #{:countable}})
                                      :dealiases lib/dealiases}))
        _ (is (= :vector (:type type)))
        _ (println "REAL-AST: " ast)
        form (a/ast->form ast)
        _ (println "FORM: " form)
        func (eval `(fn [~'in1] ~form))]
    (is (= [[1 "hello"] [3 "world"]] (func {1 "hello" 3 "world"})))
    (is (= [1 4 9 5 10] (func #{1 4 9 5 10})))
    (is (= [\h \e \l \l \o \!] (func "hello!"))) 
    (is (= [] (func [])))
    (is (= [1 2 3] (func [1 2 3])))
    (is (thrown?
         java.lang.RuntimeException
         (func 5)))))
     
(deftest set-cast-test
  (let [{::c/keys [ast type]} (:ast (c/push->ast
                                     {:push      [{:gene :lit :val 42 :type {:type 'int?}}
                                                  {:gene :local :idx 0}
                                                  {:gene :var :name 'set}
                                                  {:gene :apply}]
                                      :locals    ['in1]
                                      :ret-type  (lib/s-var 'a)
                                      :type-env  (assoc lib/type-env
                                                        'in1 {:type :s-var :sym 'c :typeclasses #{:countable}})
                                      :dealiases lib/dealiases}))
        _ (is (= :set (:type type)))
        _ (println "REAL-AST: " ast)
        form (a/ast->form ast)
        _ (println "FORM: " form)
        func (eval `(fn [~'in1] ~form))]
    (is (= #{[1 "hello"] [3 "world"]} (func {1 "hello" 3 "world"})))
    (is (= #{\a \b \c \d} (func [\a \b \c \d])))
    (is (= #{} (func []))) 
    (is (= #{\t \e \s} (func "test"))))) ; should this error b/c no overload?
     
(deftest map-cast-test
  (let [{::c/keys [ast type]} (:ast (c/push->ast
                                     {:push      [{:gene :lit :val 42 :type {:type 'int?}}
                                                  {:gene :local :idx 0}
                                                  {:gene :var :name `lib/->map}
                                                  {:gene :apply}]
                                      :locals    ['in1]
                                      :ret-type  (lib/s-var 'a)
                                      :type-env  (assoc lib/type-env
                                                        'in1 {:type :s-var :sym 'c :typeclasses #{:countable}})
                                      :dealiases lib/dealiases}))
        _ (is (= :map-of (:type type)))
        _ (println "REAL-AST: " ast)
        form (a/ast->form ast)
        _ (println "FORM: " form)
        func (eval `(fn [~'in1] ~form))] 
    (is (= {1 "hello" 3 "world"} (func #{[1 "hello"] [3 "world"]})))
    (is (= {\a 26 \m 14 \n 13 \z 1} (func [[\a 26] [\m 14] [\n 13] [\z 1]])))
    (is (= {} (func [])))
    (is (thrown?
         java.lang.IllegalArgumentException
         (func "test")))))
     
;; Combining collections
;; Conj, Concat, join


;; Higher Order functions
;; Reduce, Filter, Remove


;; Functional Programming
;; comp (comp2-fn1, comp3-fn1, comp2-fn2, comp3-fn2), partial
; working w/ normal order
; (broken) reversed schema order: (comp erp12.cbgp-lite.lang.lib/square inc in1)
(deftest comp2-fn1-test
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
                                                        'in1 {:type :s-var :sym 'a}) 
                                      :dealiases lib/dealiases}))
        _ (is (= :s-var (:type type)))
        _ (println "REAL-AST: " ast)
        form (a/ast->form ast)
        _ (println "FORM: " form)
        func (eval `(fn [~'in1] ~form))]
    (is (= 25 (func 4)))))

; broken, wrong form: ((comp erp12.cbgp-lite.lang.lib/square inc) in1)
; reversed schema order: ((comp erp12.cbgp-lite.lang.lib/square inc inc) in1)
(deftest comp3-fn1-test
  (println)
  (println "---STARTING COMP TEST 2---")
  (let [{::c/keys [ast type]} (:ast (c/push->ast
                                     {:push      [{:gene :local :idx 0}
                                                  {:gene :var :name 'inc}
                                                  {:gene :var :name 'inc}
                                                  {:gene :var :name `lib/square}
                                                  {:gene :var :name 'comp}
                                                  {:gene :apply}
                                                  {:gene :apply}]
                                      :locals    ['in1]
                                      :ret-type  (lib/s-var 'a)
                                      :type-env  (assoc lib/type-env
                                                        'in1 {:type 'int?}
                                                        ;'in1 {:type :s-var :sym 'a}
                                                        )
                                      :dealiases lib/dealiases}))
        _ (is (= :s-var (:type type)))
        _ (println "REAL-AST: " ast)
        form (a/ast->form ast)
        _ (println "FORM: " form)
        func (eval `(fn [~'in1] ~form))]
    (is (= 26 (func 4)))))

; broken, wrong form: ((comp in1 in2) +)  <- what.
; reversed schema order: (comp in1 in2 +) <- w h a t.
(deftest comp2-fn2-test
  (println)
  (println "---STARTING COMP TEST 3---")
  (let [{::c/keys [ast type]} (:ast (c/push->ast
                                     {:push      [{:gene :local :idx 2}
                                                  {:gene :local :idx 1}
                                                  {:gene :local :idx 0}
                                                  {:gene :var :name '-}
                                                  {:gene :var :name '+}
                                                  {:gene :var :name 'comp}
                                                  {:gene :apply}
                                                  {:gene :apply}]
                                      :locals    ['in1 'in2 'in3]
                                      :ret-type  (lib/s-var 'a)
                                      :type-env  (assoc lib/type-env
                                                        'in1 {:type :s-var :sym 'a}
                                                        'in2 {:type :s-var :sym 'a}
                                                        'in3 {:type :s-var :sym 'a}
                                                        )
                                      :dealiases lib/dealiases}))
        _ (is (= 'int? (:type type)))
        _ (println "REAL-AST: " ast)
        form (a/ast->form ast)
        _ (println "FORM: " form)
        func (eval `(fn [~'in1 ~'in2] ~form))]
    (is (= 14 (func 10 5 1)))))

; broken, wrong form: ((comp str in1) +)
; reversed schema order:  ((comp str in1 +) in2 in3)
(deftest comp3-fn2-test
  (println)
  (println "---STARTING COMP TEST 4---")
  (let [{::c/keys [ast type]} (:ast (c/push->ast
                                     {:push      [{:gene :local :idx 2}
                                                  {:gene :local :idx 1}
                                                  {:gene :local :idx 0}
                                                  {:gene :var :name 'str}
                                                  {:gene :var :name '-}
                                                  {:gene :var :name '+}
                                                  {:gene :var :name 'comp}
                                                  {:gene :apply}
                                                  {:gene :apply}]
                                      :locals    ['in1 'in2 'in3]
                                      :ret-type  (lib/s-var 'a)
                                      :type-env  (assoc lib/type-env
                                                        'in1 {:type :s-var :sym 'a}
                                                        'in2 {:type :s-var :sym 'a}
                                                        'in3 {:type :s-var :sym 'a})
                                      :dealiases lib/dealiases}))
        _ (is (= 'string? (:type type)))
        _ (println "REAL-AST: " ast)
        form (a/ast->form ast)
        _ (println "FORM: " form)
        func (eval `(fn [~'in1 ~'in2 ~'in3] ~form))]
    (is (= "14" (func 10 5 1)))))

; partial (partial1-fn2, partial1-fn3, partial2-fn3)
; works
; (broken) reverse schema order: (partial in1 * 100)
(deftest partial1-fn2-test
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

; broken, wrong form: ((partial * 100) in1)
; reverse schema order: ((partial in1 count *) 100)
(deftest partial1-fn3-test
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
(deftest partial2-fn3-test
  (println)
  (println "---STARTING PARTIAL 3 TEST---")
  (let [{::c/keys [ast type]} (:ast (c/push->ast
                                     {:push      [{:gene :local :idx 0}
                                                  {:gene :lit :val 100 :type {:type 'int?}}
                                                  {:gene :var :name '*}
                                                  {:gene :var :name 'count}
                                                  {:gene :var :name 'partial}
                                                  {:gene :apply}
                                                  {:gene :apply}]
                                      :locals    ['in1 'in2]
                                      :ret-type  (lib/s-var 'a)
                                      :type-env  (assoc lib/type-env
                                                        'in1 {:type :s-var :sym 'a}
                                                        'in2 {:type :s-var :sym 'a})
                                      :dealiases lib/dealiases}))
        _ (is (= 'int? (:type type)))
        _ (println "REAL-AST: " ast)
        form (a/ast->form ast)
        _ (println "FORM: " form)
        func (eval `(fn [~'in1 ~'in2] ~form))]
    (is (= [500 300] (func [1 2 3 4 5] [1 3 4])))))

;; Other 
;; remove-element, in?