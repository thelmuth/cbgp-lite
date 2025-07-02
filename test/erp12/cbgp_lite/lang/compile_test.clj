(ns erp12.cbgp-lite.lang.compile-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [clojure.walk :as w]
            [erp12.cbgp-lite.lang.ast :as a]
            [erp12.cbgp-lite.lang.compile :as c]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.lang.schema :as schema]
            [mb.hawk.core]
            [meander.epsilon :as m])
  (:import (java.io StringWriter)))

(defmacro matches?
  [template value]
  (let [template (w/postwalk (fn [node]
                               (if (and (symbol? node)
                                        (str/starts-with? (name node) "?"))
                                 `(m/pred some? ~node)
                                 node))
                             template)]
    `(m/match ~value
       ~template true
       ~(quote ?else) false)))

(deftest matches?-test
  (is (matches? {:op ?_ :name ?a}
                {:op :var :name 'foo}))
  (is (not (matches? {:op ?_ :name ?a}
                     {:op :var :var 'foo})))
  (is (not (matches? {:op :VAR :name ?a}
                     {:op :var :name 'foo})))
  (is (matches? {:a ?x :b ?x}
                {:a 1 :b 1 :c "extra"})))

(deftest nth-local-test
  (is (= (c/nth-local 100 {:locals ['x 'y 'z]}) 'y))
  (is (nil? (c/nth-local 100 {:locals []}))))

(deftest macro?-test
  (is (c/macro? {:op :var :var 'if}))
  (is (not (c/macro? {:op :var :var 'double-add}))))

(deftest pop-ast-test
  (is (= (c/pop-ast c/empty-state)
         {:ast :none :state c/empty-state}))
  (is (= (c/pop-ast {:asts [{::c/ast  :_
                             ::c/type {:type 'int?}}]})
         {:ast   {::c/ast :_ ::c/type {:type 'int?}}
          :state {:asts (list)}}))
  (testing "popping functions of any type"
    (is (= (c/pop-function-ast {:asts (list {::c/ast  :_
                                             ::c/type {:type 'boolean?}}
                                            {::c/ast  :_
                                             ::c/type {:type   :=>
                                                       :input  {:type     :cat
                                                                :children [{:type 'int?}]}
                                                       :output {:type 'string?}}})})
           {:ast   {::c/ast  :_
                    ::c/type {:type   :=>
                              :input  {:type     :cat
                                       :children [{:type 'int?}]}
                              :output {:type 'string?}}}
            :state {:asts (list {::c/ast  :_
                                 ::c/type {:type 'boolean?}})}}))

    (is (= (c/pop-function-ast {:asts (list {::c/ast  :_
                                             ::c/type {:type 'boolean?}}
                                            {::c/ast  :_
                                             ::c/type {:type   :scheme
                                                       :s-vars ['a]
                                                       :body   {:type   :=>
                                                                :input  {:type     :cat
                                                                         :children [{:type :s-var :sym 'a}]}
                                                                :output {:type :s-var :sym 'a}}}})})
           {:ast   {::c/ast  :_
                    ::c/type {:type   :scheme
                              :s-vars ['a]
                              :body   {:type   :=>
                                       :input  {:type     :cat
                                                :children [{:type :s-var :sym 'a}]}
                                       :output {:type :s-var :sym 'a}}}}
            :state {:asts (list {::c/ast  :_
                                 ::c/type {:type 'boolean?}})}})))

  (testing "popping function of specific type"
    (testing "monomorphic function"
      (is (= (c/pop-unifiable-ast {:type   :=>
                                   :input  {:type     :cat
                                            :children [{:type 'int?}]}
                                   :output {:type 'string?}}
                                  {:asts (list {::c/ast  :_
                                                ::c/type {:type 'boolean?}}
                                               {::c/ast  :_
                                                ::c/type {:type   :=>
                                                          :input  {:type     :cat
                                                                   :children [{:type 'int?}]}
                                                          :output {:type 'string?}}})})
             {:ast      {::c/ast  :_
                         ::c/type {:type   :=>
                                   :input  {:type     :cat
                                            :children [{:type 'int?}]}
                                   :output {:type 'string?}}}
              :state    {:asts (list {::c/ast  :_
                                      ::c/type {:type 'boolean?}})}
              :bindings {}})))
    (testing "failed polymorphic function"
      (let [init-state {:asts (list {::c/ast  :_
                                     ::c/type {:type 'boolean?}}
                                    {::c/ast  :_
                                     ::c/type {:type   :=>
                                               :input  {:type     :cat
                                                        :children [{:type :s-var :sym 'a}]}
                                               :output {:type :s-var :sym 'a}}})}]
        (is (= (c/pop-unifiable-ast {:type   :=>
                                     :input  {:type     :cat
                                              :children [{:type 'int?}]}
                                     :output {:type 'string?}}
                                    init-state)
               {:ast      :none
                :state    init-state
                :bindings {}}))))
    (testing "successful polymorphic function"
      (is (= (c/pop-unifiable-ast {:type   :=>
                                   :input  {:type     :cat
                                            :children [{:type 'int?}]}
                                   :output {:type 'string?}}
                                  {:asts (list {::c/ast  :_
                                                ::c/type {:type 'boolean?}}
                                               {::c/ast  :_
                                                ::c/type {:type   :=>
                                                          :input  {:type     :cat
                                                                   :children [{:type :s-var :sym 'a}]}
                                                          :output {:type :s-var :sym 'b}}})})
             {:ast      {::c/ast  :_
                         ::c/type {:type   :=>
                                   :input  {:type     :cat
                                            :children [{:type :s-var :sym 'a}]}
                                   :output {:type :s-var :sym 'b}}}
              :state    {:asts (list {::c/ast  :_
                                      ::c/type {:type 'boolean?}})}
              :bindings {'a {:type 'int?}
                         'b {:type 'string?}}}))))

  (testing "skip macros"
    (is (= (c/pop-ast {:asts (list {::c/ast {:op :var :var 'if} :type :_}
                                   {::c/ast {:op :var :var '+} :type :_})})
           {:ast   {::c/ast {:op :var :var '+} :type :_}
            :state {:asts (list {::c/ast {:op :var :var 'if} :type :_})}}))))

(deftest compile-step-test
  (testing "compile lit"
    (is (partial= {:asts   (list {::c/ast  {:op :const :val 1}
                                  ::c/type {:type 'int?}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :lit :val 1 :type {:type 'int?}}
                                   :state     c/empty-state
                                   :type-env  {}}))))
  (testing "compile var"
    #_{:clj-kondo/ignore [:invalid-arity :unresolved-symbol]}
    (is (matches? {:asts   ({::c/ast  {:op :var :var '=}
                             ::c/type {:type   :=>
                                       :input  {:type     :cat
                                                :children [{:type :s-var :sym ?a}
                                                           {:type :s-var :sym ?b}]}
                                       :output {:type 'boolean?}}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :var :name '=}
                                   :state     c/empty-state
                                   :type-env  lib/type-env}))))
  (testing "compile local"
    (is (= c/empty-state
           (c/compile-step {:push-unit {:gene :local :idx 3}
                            :state     c/empty-state
                            :type-env  lib/type-env})))
    (is (partial= {:asts   (list {::c/ast  {:op :local :name 'x}
                                  ::c/type {:type 'int?}})
                   :push   []
                   :locals ['x]}
                  (c/compile-step {:push-unit {:gene :local :idx 3}
                                   :state     (assoc c/empty-state :locals ['x])
                                   :type-env  {'x {:type 'int?}}}))))
  ;;math testing
  (testing "compile apply math"
    (is (partial= {:asts   (list {::c/ast  {:op   :invoke
                                            :fn   {:op :var :var `lib/>'}
                                            :args [{:op :const :val 2}
                                                   {:op :const :val 1}]}
                                  ::c/type {:type 'boolean?}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :apply}
                                   :state     (assoc c/empty-state
                                                     :asts (list {::c/ast  {:op :const :val 2}
                                                                  ::c/type {:type 'int?}}
                                                                 {::c/ast  {:op :const :val 1}
                                                                  ::c/type {:type 'int?}}
                                                                 {::c/ast  {:op :var :var `lib/>'}
                                                                  ::c/type (schema/instantiate (lib/type-env `lib/>'))}))
                                   :type-env  lib/type-env})))
    (is (partial= {:asts   (list {::c/ast  {:op   :invoke
                                            :fn   {:op :var :var '+}
                                            :args [{:op :const :val 2.0}
                                                   {:op :const :val 1.87}]}
                                  ::c/type {:type 'double?}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :apply}
                                   :state     (assoc c/empty-state
                                                     :asts (list {::c/ast  {:op :const :val 2.0}
                                                                  ::c/type {:type 'double?}}
                                                                 {::c/ast  {:op :const :val 1.87}
                                                                  ::c/type {:type 'double?}}
                                                                 {::c/ast  {:op :var :var '+}
                                                                  ::c/type (schema/instantiate (lib/type-env '+))}))
                                   :type-env  lib/type-env})))
    (is (partial= {:asts   (list {::c/ast  {:op   :invoke
                                            :fn   {:op :var :var '+}
                                            :args [{:op :const :val 2}
                                                   {:op :const :val 1}]}
                                  ::c/type {:type 'int?}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :apply}
                                   :state     (assoc c/empty-state
                                                     :asts (list {::c/ast  {:op :const :val 2}
                                                                  ::c/type {:type 'int?}}
                                                                 {::c/ast  {:op :const :val 1}
                                                                  ::c/type {:type 'int?}}
                                                                 {::c/ast  {:op :var :var '+}
                                                                  ::c/type (schema/instantiate (lib/type-env '+))}))
                                   :type-env  lib/type-env})))
    (is (partial= {:asts   (list {::c/ast  {:op   :invoke
                                            :fn   {:op :var :var 'dec}
                                            :args [{:op :const :val 2.0}]}
                                  ::c/type {:type 'double?}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :apply}
                                   :state     (assoc c/empty-state
                                                     :asts (list {::c/ast  {:op :const :val 2.0}
                                                                  ::c/type {:type 'double?}}
                                                                 {::c/ast  {:op :var :var 'dec}
                                                                  ::c/type (schema/instantiate (lib/type-env 'dec))}))
                                   :type-env  lib/type-env})))
    (is (partial= {:asts   (list {::c/ast  {:op   :invoke
                                            :fn   {:op :var :var 'inc}
                                            :args [{:op :const :val 3}]}
                                  ::c/type {:type 'int?}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :apply}
                                   :state     (assoc c/empty-state
                                                     :asts (list {::c/ast  {:op :const :val 3}
                                                                  ::c/type {:type 'int?}}
                                                                 {::c/ast  {:op :var :var 'inc}
                                                                  ::c/type (schema/instantiate (lib/type-env 'inc))}))
                                   :type-env  lib/type-env})))
    (is (partial= {:asts   (list {::c/ast  {:op   :invoke
                                            :fn   {:op :var :var `lib/neg}
                                            :args [{:op :const :val -3}]}
                                  ::c/type {:type 'int?}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :apply}
                                   :state     (assoc c/empty-state
                                                     :asts (list {::c/ast  {:op :const :val -3}
                                                                  ::c/type {:type 'int?}}
                                                                 {::c/ast  {:op :var :var `lib/neg}
                                                                  ::c/type (schema/instantiate (lib/type-env `lib/neg))}))
                                   :type-env  lib/type-env})))
    (is (partial= {:asts   (list {::c/ast  {:op   :invoke
                                            :fn   {:op :var :var 'abs}
                                            :args [{:op :const :val -3.4}]}
                                  ::c/type {:type 'double?}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :apply}
                                   :state     (assoc c/empty-state
                                                     :asts (list {::c/ast  {:op :const :val -3.4}
                                                                  ::c/type {:type 'double?}}
                                                                 {::c/ast  {:op :var :var 'abs}
                                                                  ::c/type (schema/instantiate (lib/type-env 'abs))}))
                                   :type-env  lib/type-env})))
    (is (partial= {:asts   (list {::c/ast  {:op   :invoke
                                            :fn   {:op :var :var '*}
                                            :args [{:op :const :val 2.0}
                                                   {:op :const :val 3.0}]}
                                  ::c/type {:type 'double?}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :apply}
                                   :state     (assoc c/empty-state
                                                     :asts (list {::c/ast  {:op :const :val 2.0}
                                                                  ::c/type {:type 'double?}}
                                                                 {::c/ast  {:op :const :val 3.0}
                                                                  ::c/type {:type 'double?}}
                                                                 {::c/ast  {:op :var :var '*}
                                                                  ::c/type (schema/instantiate (lib/type-env '*))}))
                                   :type-env  lib/type-env})))
    (is (partial= {:asts   (list {::c/ast  {:op   :invoke
                                            :fn   {:op :var :var 'quot}
                                            :args [{:op :const :val 2.0}
                                                   {:op :const :val 3.0}]}
                                  ::c/type {:type 'double?}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :apply}
                                   :state     (assoc c/empty-state
                                                     :asts (list {::c/ast  {:op :const :val 2.0}
                                                                  ::c/type {:type 'double?}}
                                                                 {::c/ast  {:op :const :val 3.0}
                                                                  ::c/type {:type 'double?}}
                                                                 {::c/ast  {:op :var :var 'quot}
                                                                  ::c/type (schema/instantiate (lib/type-env 'quot))}))
                                   :type-env  lib/type-env})))
    (is (partial= {:asts   (list {::c/ast  {:op   :invoke
                                            :fn   {:op :var :var '/}
                                            :args [{:op :const :val 2.0}
                                                   {:op :const :val 2.0}]}
                                  ::c/type {:type 'double?}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :apply}
                                   :state     (assoc c/empty-state
                                                     :asts (list {::c/ast  {:op :const :val 2.0}
                                                                  ::c/type {:type 'double?}}
                                                                 {::c/ast  {:op :const :val 2.0}
                                                                  ::c/type {:type 'double?}}
                                                                 {::c/ast  {:op :var :var '/}
                                                                  ::c/type (schema/instantiate (lib/type-env '/))}))
                                   :type-env  lib/type-env})))
    (is (partial= {:asts   (list {::c/ast  {:op   :invoke
                                            :fn   {:op :var :var '/}
                                            :args [{:op :const :val 2}
                                                   {:op :const :val 3}]}
                                  ::c/type {:type 'double?}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :apply}
                                   :state     (assoc c/empty-state
                                                     :asts (list {::c/ast  {:op :const :val 2}
                                                                  ::c/type {:type 'int?}}
                                                                 {::c/ast  {:op :const :val 3}
                                                                  ::c/type {:type 'int?}}
                                                                 {::c/ast  {:op :var :var '/}
                                                                  ::c/type (schema/instantiate (lib/type-env '/))}))
                                   :type-env  lib/type-env})))
    (is (partial= {:asts   (list {::c/ast  {:op   :invoke
                                            :fn   {:op :var :var `lib/>'}
                                            :args [{:op :const :val 2}
                                                   {:op :const :val 3}]}
                                  ::c/type {:type 'boolean?}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :apply}
                                   :state     (assoc c/empty-state
                                                     :asts (list {::c/ast  {:op :const :val 2}
                                                                  ::c/type {:type 'int?}}
                                                                 {::c/ast  {:op :const :val 3}
                                                                  ::c/type {:type 'int?}}
                                                                 {::c/ast  {:op :var :var `lib/>'}
                                                                  ::c/type (schema/instantiate (lib/type-env `lib/>'))}))
                                   :type-env  lib/type-env})))
    (is (partial= {:asts   (list {::c/ast  {:op   :invoke
                                            :fn   {:op :var :var `lib/>'}
                                            :args [{:op :const :val \e}
                                                   {:op :const :val \j}]}
                                  ::c/type {:type 'boolean?}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :apply}
                                   :state     (assoc c/empty-state
                                                     :asts (list {::c/ast  {:op :const :val \e}
                                                                  ::c/type {:type 'char?}}
                                                                 {::c/ast  {:op :const :val \j}
                                                                  ::c/type {:type 'char?}}
                                                                 {::c/ast  {:op :var :var `lib/>'}
                                                                  ::c/type (schema/instantiate (lib/type-env `lib/>'))}))
                                   :type-env  lib/type-env})))
    (is (partial= {:asts   (list {::c/ast  {:op   :invoke
                                            :fn   {:op :var :var `lib/>'}
                                            :args [{:op :const :val "strawberry"}
                                                   {:op :const :val "banana"}]}
                                  ::c/type {:type 'boolean?}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :apply}
                                   :state     (assoc c/empty-state
                                                     :asts (list {::c/ast  {:op :const :val "strawberry"}
                                                                  ::c/type {:type 'string?}}
                                                                 {::c/ast  {:op :const :val "banana"}
                                                                  ::c/type {:type 'string?}}
                                                                 {::c/ast  {:op :var :var `lib/>'}
                                                                  ::c/type (schema/instantiate (lib/type-env `lib/>'))}))
                                   :type-env  lib/type-env})))
    (is (partial= {:asts   (list {::c/ast  {:op   :invoke
                                            :fn   {:op :var :var '=}
                                            :args [{:op :const :val "strawberry"}
                                                   {:op :const :val "banana"}]}
                                  ::c/type {:type 'boolean?}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :apply}
                                   :state     (assoc c/empty-state
                                                     :asts (list {::c/ast  {:op :const :val "strawberry"}
                                                                  ::c/type {:type 'string?}}
                                                                 {::c/ast  {:op :const :val "banana"}
                                                                  ::c/type {:type 'string?}}
                                                                 {::c/ast  {:op :var :var '=}
                                                                  ::c/type (schema/instantiate (lib/type-env '=))}))
                                   :type-env  lib/type-env}))) 
    (is (partial= {:asts   (list {::c/ast  {:op   :invoke
                                            :fn   {:op :var :var 'not=}
                                            :args [{:op :const :val 15}
                                                   {:op :const :val 16}]}
                                  ::c/type {:type 'boolean?}})
                   :push   []
                   :locals []}
              (c/compile-step {:push-unit {:gene :apply}
                               :state     (assoc c/empty-state
                                                 :asts (list {::c/ast  {:op :const :val 15}
                                                              ::c/type {:type 'int?}}
                                                             {::c/ast  {:op :const :val 16}
                                                              ::c/type {:type 'int?}}
                                                             {::c/ast  {:op :var :var 'not=}
                                                              ::c/type (schema/instantiate (lib/type-env 'not=))}))
                               :type-env  lib/type-env})))
    (is (partial= {:asts   (list {::c/ast  {:op   :invoke
                                            :fn   {:op :var :var `lib/min'}
                                            :args [{:op :const :val 2}
                                                   {:op :const :val 3}]}
                                  ::c/type {:type 'int?}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :apply}
                                   :state     (assoc c/empty-state
                                                     :asts (list {::c/ast  {:op :const :val 2}
                                                                  ::c/type {:type 'int?}}
                                                                 {::c/ast  {:op :const :val 3}
                                                                  ::c/type {:type 'int?}}
                                                                 {::c/ast  {:op :var :var `lib/min'}
                                                                  ::c/type (schema/instantiate (lib/type-env `lib/min'))}))
                                   :type-env  lib/type-env})))
    (is (partial= {:asts   (list {::c/ast  {:op   :invoke
                                            :fn   {:op :var :var `lib/min'}
                                            :args [{:op :const :val \q}
                                                   {:op :const :val \w}]}
                                  ::c/type {:type 'char?}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :apply}
                                   :state     (assoc c/empty-state
                                                     :asts (list {::c/ast  {:op :const :val \q}
                                                                  ::c/type {:type 'char?}}
                                                                 {::c/ast  {:op :const :val \w}
                                                                  ::c/type {:type 'char?}}
                                                                 {::c/ast  {:op :var :var `lib/min'}
                                                                  ::c/type (schema/instantiate (lib/type-env `lib/min'))}))
                                   :type-env  lib/type-env})))
    (is (partial= {:asts   (list {::c/ast  {:op   :invoke
                                            :fn   {:op :var :var `lib/max'}
                                            :args [{:op :const :val 2.0}
                                                   {:op :const :val 3.4}]}
                                  ::c/type {:type 'double?}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :apply}
                                   :state     (assoc c/empty-state
                                                     :asts (list {::c/ast  {:op :const :val 2.0}
                                                                  ::c/type {:type 'double?}}
                                                                 {::c/ast  {:op :const :val 3.4}
                                                                  ::c/type {:type 'double?}}
                                                                 {::c/ast  {:op :var :var `lib/max'}
                                                                  ::c/type (schema/instantiate (lib/type-env `lib/max'))}))
                                   :type-env  lib/type-env})))
    (is (partial= {:asts   (list {::c/ast  {:op   :invoke
                                            :fn   {:op :var :var 'int}
                                            :args [{:op :const :val 5.4}]}
                                  ::c/type {:type 'int?}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :apply}
                                   :state     (assoc c/empty-state
                                                     :asts (list {::c/ast  {:op :const :val 5.4}
                                                                  ::c/type {:type 'double?}}
                                                                 {::c/ast  {:op :var :var 'int}
                                                                  ::c/type (schema/instantiate (lib/type-env 'int))}))
                                   :type-env  lib/type-env})))

    (is (partial= {:asts   (list {::c/ast  {:op   :invoke
                                            :fn   {:op :var :var 'int}
                                            :args [{:op :const :val \g}]}
                                  ::c/type {:type 'int?}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :apply}
                                   :state     (assoc c/empty-state
                                                     :asts (list {::c/ast  {:op :const :val \g}
                                                                  ::c/type {:type 'char?}}
                                                                 {::c/ast  {:op :var :var 'int}
                                                                  ::c/type (schema/instantiate (lib/type-env 'int))}))
                                   :type-env  lib/type-env}))))

  (testing "compile apply"
    (is (partial= {:asts   (list {::c/ast  {:op   :invoke
                                            :fn   {:op :var :var '-}
                                            :args [{:op :const :val 2}
                                                   {:op :const :val 1}]}
                                  ::c/type {:type 'int?}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :apply}
                                   :state     (assoc c/empty-state
                                                     :asts (list {::c/ast  {:op :const :val 2}
                                                                  ::c/type {:type 'int?}}
                                                                 {::c/ast  {:op :const :val 1}
                                                                  ::c/type {:type 'int?}}
                                                                 {::c/ast  {:op :var :var '-}
                                                                  ::c/type (schema/instantiate (lib/type-env '-))}))
                                   :type-env  lib/type-env})))
    (is (partial= {:asts   (list {::c/ast  {:op   :invoke
                                            :fn   {:op :var :var 'if}
                                            :args [{:op :local :name 'x}
                                                   {:op :const :val 1}
                                                   {:op :const :val -1}]}
                                  ::c/type {:type 'int?}})
                   :push   []
                   :locals ['x]}
                  (c/compile-step {:push-unit {:gene :apply}
                                   :state     (assoc c/empty-state
                                                     :asts (list {::c/ast  {:op :const :val 1}
                                                                  ::c/type {:type 'int?}}
                                                                 {::c/ast  {:op :const :val -1}
                                                                  ::c/type {:type 'int?}}
                                                                 {::c/ast  {:op :var :var 'if}
                                                                  ::c/type (schema/instantiate (lib/type-env 'if))}
                                                                 {::c/ast  {:op :local :name 'x}
                                                                  ::c/type {:type 'boolean?}})
                                                     :locals ['x])
                                   :type-env  (assoc lib/type-env
                                                     'x {:type 'boolean?})}))))
    ;; @todo Test when args are missing
    
  (testing "compile fn"
    #_{:clj-kondo/ignore [:invalid-arity :unresolved-symbol]}
    (is (matches? {:asts   ({::c/ast  {:op      :fn
                                       :methods [{:op     :fn-method
                                                  :params [{:op :binding :name ?a}]
                                                  :body   {:op :local :name ?a}}]}
                             ::c/type {:type   :=>
                                       :input  {:type     :cat
                                                :children [{:type int?}]}
                                       :output {:type int?}}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene      :fn
                                               :arg-types [lib/INT]
                                               :ret-type lib/INT}
                                   :state     (assoc c/empty-state
                                                     :asts (list)
                                                     :push [[{:gene :local :idx 1}]])
                                   :type-env  {}})))
    (testing "nullary fn"
      (is (partial= {:asts   (list {::c/ast  {:op      :fn
                                              :methods [{:op     :fn-method
                                                         :params []
                                                         :body   {:op :var :var 'x}}]}
                                    ::c/type {:type   :=>
                                              :input  {:type :cat :children []}
                                              :output {:type 'string?}}})
                     :push   []
                     :locals []}
                    (c/compile-step {:push-unit {:gene :fn :ret-type lib/STRING}
                                     :state     (assoc c/empty-state
                                                       :asts (list {::c/ast  {:op :var :var 'x}
                                                                    ::c/type {:type 'string?}})
                                                       :push [])
                                     :type-env  {'x {:type 'string?}}})))))
  (testing "compile let"
    #_{:clj-kondo/ignore [:invalid-arity :unresolved-symbol]}
    (is (matches? {:asts   ({::c/ast  {:op       :let
                                       :bindings [{:op   :binding
                                                   :name ?v
                                                   :init {:op :const :val 1}}]
                                       :body     {:op :local :name ?v}}
                             ::c/type {:type int?}})
                   :push   []
                   :locals []}
                  (c/compile-step {:push-unit {:gene :let}
                                   :state     (assoc c/empty-state
                                                     :asts (list {::c/ast  {:op :const :val 1}
                                                                  ::c/type {:type 'int?}})
                                                     :push [[{:gene :local :idx 1}]])
                                   :type-env  {}})))))

(deftest push->ast-test
  (testing "composing schemes"
    #_{:clj-kondo/ignore [:invalid-arity :unresolved-symbol]}
    (is (matches? {::c/ast  {:op   :invoke
                             :fn   {:op :var :var 'identity}
                             :args [{:op :var :var 'poly-f}]}
                   ::c/type {:type   :=>
                             :input  {:type     :cat
                                      :children [{:type :s-var :sym ?x}]}
                             :output {:type :s-var :sym ?y}}}
                  (:ast (c/push->ast {:push     [{:gene :var :name 'poly-f}
                                                 {:gene :var :name 'identity}
                                                 {:gene :apply}]
                                      :locals   []
                                      :ret-type {:type   :scheme
                                                 :s-vars ['a 'b]
                                                 :body   {:type   :=>
                                                          :input  {:type     :cat
                                                                   :children [{:type :s-var :sym 'a}]}
                                                          :output {:type :s-var :sym 'b}}}
                                      :type-env {'identity {:type   :scheme
                                                            :s-vars ['c]
                                                            :body   {:type   :=>
                                                                     :input  {:type     :cat
                                                                              :children [{:type :s-var :sym 'c}]}
                                                                     :output {:type :s-var :sym 'c}}}
                                                 'poly-f   {:type   :scheme
                                                            :s-vars ['d 'e]
                                                            :body   {:type   :=>
                                                                     :input  {:type     :cat
                                                                              :children [{:type :s-var :sym 'd}]}
                                                                     :output {:type :s-var :sym 'e}}}}})))))
  (testing "pruning unused function args"
    (is (= (:ast (c/push->ast {:push     [{:gene      :fn
                                           :arg-types [{:type 'int?}
                                                       {:type 'string?}]
                                           :ret-type lib/INT}
                                          [{:gene :var :name 'x}]]
                               :locals   []
                               :ret-type {:type   :=>
                                          :input  {:type :cat :children []}
                                          :output {:type 'int?}}
                               :type-env {'x {:type 'int?}}}))
           {::c/ast  {:op      :fn
                      :methods [{:op     :fn-method
                                 :params []
                                 :body   {:op :var :var 'x}}]}
            ::c/type {:type   :=>
                      :input  {:type :cat :children []}
                      :output {:type 'int?}}})))
  (testing "polymorphic thunk"
    #_{:clj-kondo/ignore [:invalid-arity :unresolved-symbol]}
    (is (matches? {::c/ast  {:op      :fn
                             :methods [{:op     :fn-method
                                        :params []
                                        :body   {:op :var :var identity}}]}
                   ::c/type {:type   :=>
                             :input  {:type :cat :children []}
                             :output {:type   :=>
                                      :input  {:type :cat :children [{:type :s-var :sym ?a}]}
                                      :output {:type :s-var :sym ?a}}}}
                  (:ast (c/push->ast {:push     [{:gene :var :name 'identity}
                                                 {:gene :fn :ret-type (lib/fn-of [(lib/s-var 'a)] (lib/s-var 'a))}]
                                      :locals   []
                                      :ret-type {:type   :=>
                                                 :input  {:type :cat :children []}
                                                 :output {:type   :=>
                                                          :input  {:type :cat :children [{:type 'int?}]}
                                                          :output {:type 'int?}}}
                                      :type-env {'identity {:type   :scheme
                                                            :s-vars ['a]
                                                            :body   {:type   :=>
                                                                     :input  {:type :cat :children [{:type :s-var :sym 'a}]}
                                                                     :output {:type :s-var :sym 'a}}}}}))))))

(deftest simple-math-test
  ;; Add 100 to input.
  (let [{::c/keys [ast type]} (:ast (c/push->ast {:push      [{:gene :lit :val 100 :type {:type 'int?}}
                                                              {:gene :local :idx 0}
                                                              {:gene :var :name '+}
                                                              {:gene :apply}]
                                                  :locals    ['in1]
                                                  :ret-type  {:type 'int?}
                                                  :type-env  (assoc lib/type-env
                                                                    'in1 {:type 'int?})
                                                  :dealiases lib/dealiases}))
        _ (is (= type {:type 'int? :typeclasses #{:number}}))
        form (a/ast->form ast)
        _ (is (= form '(+ in1 100)))
        func (eval `(fn [~'in1] ~form))]
    (is (= 100 (func 0)))
    (is (= 101 (func 1)))))

(deftest simple-double-math-test
  ;; Add 100.0 to input.
  (let [{::c/keys [ast type]} (:ast (c/push->ast {:push      [{:gene :lit :val 100.0 :type {:type 'double?}}
                                                              {:gene :local :idx 0}
                                                              {:gene :var :name '+}
                                                              {:gene :apply}]
                                                  :locals    ['in1]
                                                  :ret-type  {:type 'double?}
                                                  :type-env  (assoc lib/type-env
                                                                    'in1 {:type 'double?})
                                                  :dealiases lib/dealiases}))
        _ (is (= type {:type 'double? :typeclasses #{:number}}))
        form (a/ast->form ast)
        _ (is (= form '(+ in1 100.0)))
        func (eval `(fn [~'in1] ~form))]
    (is (= 100.0 (func 0.0)))
    (is (= 101.0 (func 1.0)))))

(deftest conditional-logic-test
  ;; If input < 1000, return "small" else "large".
  (let [{::c/keys [ast type]} (:ast (c/push->ast {:push      [{:gene :lit :val "large" :type {:type 'string?}}
                                                              {:gene :lit :val "small" :type {:type 'string?}}
                                                              {:gene :lit :val 1000 :type {:type 'int?}}
                                                              {:gene :local :idx 0}
                                                              {:gene :var :name `lib/<'}
                                                              {:gene :apply}
                                                              {:gene :var :name 'if}
                                                              {:gene :apply}]
                                                  :locals    ['in1]
                                                  :ret-type  {:type 'string?}
                                                  :type-env  (assoc lib/type-env
                                                                    'in1 {:type 'int?})
                                                  :dealiases lib/dealiases}))
        _ (is (= type {:type 'string?}))
        form (a/ast->form ast)
        _ (is (= form '(if (erp12.cbgp-lite.lang.lib/<' in1 1000) "small" "large")))
        func (eval `(fn [~'in1] ~form))]
    (is (= (func 0) "small"))
    (is (= (func 1000) "large"))
    (is (= (func 2000) "large"))))

(deftest conditional-eq-logic-test
  ;; If input == 1000, return "same" else "different".
  (let [{::c/keys [ast type]} (:ast (c/push->ast {:push      [{:gene :lit :val "different" :type {:type 'string?}}
                                                              {:gene :lit :val "same" :type {:type 'string?}}
                                                              {:gene :lit :val 1000 :type {:type 'int?}}
                                                              {:gene :local :idx 0}
                                                              {:gene :var :name '=}
                                                              {:gene :apply}
                                                              {:gene :var :name 'if}
                                                              {:gene :apply}]
                                                  :locals    ['in1]
                                                  :ret-type  {:type 'string?}
                                                  :type-env  (assoc lib/type-env
                                                                    'in1 {:type 'int?})
                                                  :dealiases lib/dealiases}))
        _ (is (= type {:type 'string?}))
        form (a/ast->form ast)
        _ (is (= form '(if (= in1 1000) "same" "different")))
        func (eval `(fn [~'in1] ~form))]
    (is (= (func 0) "different"))
    (is (= (func 1000) "same"))
    (is (= (func 2000) "different"))))

(deftest let-binding-test
  ;; Square and then double the input.
  (let [{::c/keys [ast type]} (:ast (c/push->ast {:push      [{:gene :local :idx 0}
                                                              {:gene :local :idx 0}
                                                              {:gene :var :name '*}
                                                              {:gene :apply}
                                                              {:gene :let}
                                                              [{:gene :local :idx 1}
                                                               {:gene :local :idx 1}
                                                               {:gene :var :name '+}
                                                               {:gene :apply}]]
                                                  :locals    ['in1]
                                                  :ret-type  {:type 'int?}
                                                  :type-env  (assoc lib/type-env
                                                                    'in1 {:type 'int?})
                                                  :dealiases lib/dealiases}))
        _ (is (= type {:type 'int? :typeclasses #{:number}}))
        form (a/ast->form ast)
        _ (is
           #_{:clj-kondo/ignore [:unresolved-symbol]}
           (matches? (let [?v (* in1 in1)]
                       (+ ?v ?v))
                     form))
        func (eval `(fn [~'in1] ~form))]
    (is (= (func 0) 0))
    (is (= (func 2) 8))
    (is (= (func -1) 2))))

(deftest hof-with-anonymous-fn-test
  ;; Map `inc` over the elements of a vector
  (let [{::c/keys [ast type]} (:ast (c/push->ast {:push      [{:gene :lit :val [1 2 3] :type {:type :vector :child {:type 'int?}}}
                                                              {:gene :fn :arg-types [lib/INT] :ret-type lib/INT}
                                                              [{:gene :local :idx 0}
                                                               {:gene :var :name 'inc}
                                                               {:gene :apply}]
                                                              {:gene :var :name 'mapv}
                                                              {:gene :apply}]
                                                  :locals    []
                                                  :ret-type  {:type :vector :child {:type 'int?}}
                                                  :type-env  lib/type-env
                                                  :dealiases lib/dealiases}))
        _ (is (= type {:type :vector :child {:type 'int? :typeclasses #{:number}}}))
        form (a/ast->form ast)
        _ (is (matches? (mapv (fn [?a] (inc ?a)) [1 2 3])
                        form))
        func (eval `(fn [] ~form))]
    (is (= [2 3 4] (func)))))

(deftest nullary-fn-test
  ;; Generate a vector of 5 random doubles.
  (let [{::c/keys [ast type]} (:ast (c/push->ast {:push      [{:gene :var :name 'rand}
                                                              {:gene :apply}
                                                              {:gene :fn :ret-type lib/DOUBLE}
                                                              {:gene :lit :val 5 :type {:type 'int?}}
                                                              {:gene :var :name 'repeatedly}
                                                              {:gene :apply}]
                                                  :locals    []
                                                  :ret-type  {:type :vector :child {:type 'double?}}
                                                  :type-env  {'rand       {:type   :=>
                                                                           :input  {:type :cat :children []}
                                                                           :output {:type 'double?}}
                                                              'repeatedly {:type   :scheme
                                                                           :s-vars ['a]
                                                                           :body   {:type   :=>
                                                                                    :input  {:type     :cat
                                                                                             :children [{:type 'int?}
                                                                                                        {:type   :=>
                                                                                                         :input  {:type :cat :children []}
                                                                                                         :output {:type :s-var :sym 'a}}]}
                                                                                    :output {:type  :vector
                                                                                             :child {:type :s-var :sym 'a}}}}}
                                                  :dealiases {}}))
        _ (is (= type {:type :vector :child {:type 'double?}}))
        form (a/ast->form ast)
        _ (is (= form '(repeatedly 5 (fn [] (rand)))))
        func (eval `(fn [] ~form))]
    (doseq [x (func)]
      (is (double? x)))))

(deftest side-effects-test
  ;; Print hello world and return 0
  (let [{::c/keys [ast type]} (:ast (c/push->ast {:push      [{:gene :lit :val 0 :type {:type 'int?}}
                                                              {:gene :lit :val "Hello world!" :type {:type 'string?}}
                                                              {:gene :var :name 'println}
                                                              {:gene :apply}
                                                              {:gene :var :name 'do2}
                                                              {:gene :apply}]
                                                  :locals    []
                                                  :ret-type  {:type 'int?}
                                                  :type-env  lib/type-env
                                                  :dealiases lib/dealiases}))
        _ (is (= type {:type 'int?}))
        form (a/ast->form ast)
        _ (is (= form '(do (println "Hello world!") 0)))
        func (eval `(fn [] ~form))
        s (new StringWriter)
        newl (new StringWriter)]
    (binding [*out* s]
      (is (= (func) 0))
      (is (= (str s) 
             (binding [*out* newl]
               (println)
               (str "Hello world!" newl)))))))

(deftest replace-space-with-newline-test
  (let [{::c/keys [ast type]} (:ast (c/push->ast {:push      [{:gene :lit :val \newline :type {:type 'char?}}
                                                              {:gene :lit :val \space :type {:type 'char?}}
                                                              {:gene :local :idx 0}
                                                              {:gene :var :name `lib/replace'}
                                                              {:gene :apply}
                                                              {:gene :let}
                                                              [;; This vector contains the body of the `let`
                                                         ;; Starting with the second clause of the `do`
                                                               {:gene :lit :val \newline :type {:type 'char?}}
                                                               {:gene :local :idx 1}
                                                               {:gene :var :name `lib/replace'}
                                                               {:gene :apply}
                                                               {:gene :var :name 'count}
                                                               {:gene :apply}
                                                         ;; Followed by the first clause of the `do`
                                                               {:gene :local :idx 1}
                                                               {:gene :var :name 'println}
                                                               {:gene :apply}
                                                         ;; Invoke the do
                                                               {:gene :var :name 'do2}
                                                               {:gene :apply}]]
                                                  :locals    ['in1]
                                                  :ret-type  {:type 'int?}
                                                  :type-env  (assoc lib/type-env
                                                                    'in1 {:type 'string?})
                                                  :dealiases lib/dealiases}))
        _ (is (= type {:type 'int?}))
        form (a/ast->form ast)
        _ (is
           #_{:clj-kondo/ignore [:unresolved-symbol :redundant-do]}
           (matches? (let [?v (erp12.cbgp-lite.lang.lib/replace' in1 \space \newline)]
                       (do (println ?v)
                           (count (erp12.cbgp-lite.lang.lib/remove ?v \newline))))
                     form))
        func (eval `(fn [~'in1] ~form))
        s (new StringWriter)]
    (binding [*out* s]
      (is (= (func "a bat CANDLE") 10))
      (is (= (str s) "a\nbat\nCANDLE\n")))))

(deftest polymorphic-output-test
  (let [{::c/keys [ast type]} (:ast
                               (c/push->ast {:push      [{:gene :local :idx 0}
                                                         {:gene :local :idx 1}
                                                         {:gene :lit :val true :type {:type 'boolean?}}
                                                         {:gene :var :name 'if}
                                                         {:gene :apply}]
                                             :locals    ['x 'y]
                                             :ret-type  (lib/s-var 'a)
                                             :type-env  (assoc lib/type-env
                                                               'x (lib/s-var 'a)
                                                               'y (lib/s-var 'a))
                                             :dealiases lib/dealiases}))
        _ (is (= :s-var (:type type)))
        form (a/ast->form ast)
        _ (is (= '(if true y x) form))
        func (eval `(fn [~'x ~'y] ~form))]
    (is (= 1 (func 0 1)))
    (is (= "that" (func "this" "that")))))
