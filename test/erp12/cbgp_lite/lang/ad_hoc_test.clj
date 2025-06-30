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

;; Combining collections
;; Conj, Concat, join
(deftest conj-test
  (testing "Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val [1 2 3 4] :type {:type :vector :child {:type 'int?}}}
                                               {:gene :lit :val 5 :type {:type 'int?}}
                                               {:gene :var :name `lib/conj'}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type :vector :child {:type 'int?}}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type :vector :child {:type 'int?}} type))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [] ~form))
          _ (println "FUNC:" func)]
      (is (= [1 2 3 4 5] (func)))))

  (testing "Strings"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val #{\h \g \e \t} :type {:type :set :child {:type 'char?}}}
                                               {:gene :lit :val \n :type {:type 'char?}}
                                               {:gene :var :name `lib/conj'}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type :set :child {:type 'char?}}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type :set :child {:type 'char?}} type))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [] ~form))
          _ (println "FUNC:" func)]
      (is (= #{\h \g \e \t \n} (func))))))

(deftest concat-test
  (testing "Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val [4.4 5.5] :type {:type :vector :child {:type 'double?}}}
                                               {:gene :lit :val [1.1 2.2 3.3] :type {:type :vector :child {:type 'double?}}}
                                               {:gene :var :name `lib/concat'}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type :vector :child {:type 'double?}}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type :vector :child {:type 'double?}} type))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [] ~form))
          _ (println "FUNC:" func)]
      (is (= [1.1 2.2 3.3 4.4 5.5] (func)))))
  (testing "String"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val "College" :type {:type 'string?}}
                                               {:gene :lit :val "Hamilton " :type {:type 'string?}}
                                               {:gene :var :name `lib/concat'}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type 'string?}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'string?} type))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [] ~form))
          _ (println "FUNC:" func)]
      (is (= "Hamilton College" (func))))))

;; Modifying collections
;; Replace
(deftest replace-test
  (testing "Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val 101 :type {:type 'int?}}
                                               {:gene :lit :val 3 :type {:type 'int?}}
                                               {:gene :lit :val [1 2 3 3 4 2 3] :type {:type :vector :child {:type 'int?}}}
                                               {:gene :var :name `lib/replace'}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type :vector :child {:type 'int?}}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type :vector :child {:type 'int?}} type))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [] ~form))
          _ (println "FUNC:" func)]
      (is (= [1 2 101 101 4 2 101] (func)))))
  (testing "String"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val \a :type {:type 'char?}}
                                               {:gene :lit :val \e :type {:type 'char?}}
                                               {:gene :lit :val "Here" :type {:type 'string?}}
                                               {:gene :var :name `lib/replace'}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type 'string?}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'string?} type))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [] ~form))
          _ (println "FUNC:" func)]
      (is (= "Hara" (func))))
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val "troll" :type {:type 'string?}}
                                               {:gene :lit :val "ut" :type {:type 'string?}}
                                               {:gene :lit :val "Computer" :type {:type 'string?}}
                                               {:gene :var :name `lib/replace'}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type 'string?}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'string?} type))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [] ~form))
          _ (println "FUNC:" func)]
      (is (= "Comptroller" (func))))))
(deftest replace-first-test
  (testing "vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val 64 :type {:type 'int?}}
                                               {:gene :lit :val 3 :type {:type 'int?}}
                                               {:gene :lit :val [1 2 3 3 3 4 5] :type {:type :vector :child {:type 'int?}}}
                                               {:gene :var :name `lib/replace-first'}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type :vector :child {:type 'int?}}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type :vector :child {:type 'int?}} type))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [] ~form))
          _ (println "FUNC:" func)]
      (is (= [1 2 64 3 3 4 5] (func)))))
  (testing "string"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val \a :type {:type 'char?}}
                                               {:gene :lit :val \e :type {:type 'char?}}
                                               {:gene :lit :val "Here" :type {:type 'string?}}
                                               {:gene :var :name `lib/replace-first'}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type 'string?}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'string?} type))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [] ~form))
          _ (println "FUNC:" func)]
      (is (= "Hare" (func))))))

;; Higher Order functions
;; Reduce, Filter, Remove
(deftest reduce-test
  (testing "Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val 0 :type {:type 'int?}}
                                               {:gene :lit :val [1 2 3 4 5] :type {:type :vector :child {:type 'int?}}}
                                               {:gene :var :name '+}
                                               {:gene :var :name 'reduce}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type 'int?}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'int? :typeclasses #{:number}} type))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [] ~form))
          _ (println "FUNC:" func)]
      (is (= 15 (func)))))
  (testing "Set"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val 0 :type {:type 'int?}}
                                               {:gene :lit :val #{1 2 3 4 5} :type {:type :set :child {:type 'int?}}}
                                               {:gene :var :name '+}
                                               {:gene :var :name 'reduce}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type 'int?}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'int? :typeclasses #{:number}} type))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [] ~form))
          _ (println "FUNC:" func)]
      (is (= 15 (func)))))
  ;; REDUCE WITH MAP does not work
  #_(testing "Map"
      (let [{::c/keys [ast type]} (:ast (c/push->ast
                                         {:push [{:gene :lit :val 0 :type {:type 'int?}}
                                                 {:gene :lit :val {\a 1 \b 2 \c 3 \d 4} :type {:type :map-of :key {:type 'char?} :value {:type 'int?}}}
                                                 {:gene :fn :arg-types [(lib/tuple-of lib/CHAR lib/INT)] :ret-type (lib/tuple-of lib/CHAR lib/INT)}
                                                 [{:gene :lit :val 0 :type {:type 'int?}}
                                                  {:gene :local :idx 0}
                                                  {:gene :var :name 'first}
                                                  {:gene :apply}
                                                  {:gene :var :name 'int}
                                                  {:gene :apply}
                                                  {}]
                                                 {:gene :var :name 'reduce}
                                                 {:gene :apply}]
                                          :locals []
                                          :ret-type {:type 'int?}
                                          :type-env lib/type-env
                                          :dealiases lib/dealiases}))
            _ (is (= {:type 'int? :typeclasses #{:number}} type))
            _ (println "REAL-AST: " ast)
            form (a/ast->form ast)
            _ (println "FORM: " form)
            func (eval `(fn [] ~form))
            _ (println "FUNC:" func)]
        (is (= 15 (func))))))

(deftest fold-test
  (testing "Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val 0 :type {:type 'int?}}
                                               {:gene :lit :val [1 2 3 4 5] :type {:type :vector :child {:type 'int?}}}
                                               {:gene :var :name '+}
                                               {:gene :var :name 'fold}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type 'int?}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'int? :typeclasses #{:number}} type))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [] ~form))
          _ (println "FUNC:" func)]
      (is (= 15 (func)))))
  (testing "Set"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push [{:gene :lit :val 0 :type {:type 'int?}}
                                               {:gene :lit :val #{1 2 3 4 5} :type {:type :set :child {:type 'int?}}}
                                               {:gene :var :name '+}
                                               {:gene :var :name 'fold}
                                               {:gene :apply}]
                                        :locals []
                                        :ret-type {:type 'int?}
                                        :type-env lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'int? :typeclasses #{:number}} type))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [] ~form))
          _ (println "FUNC:" func)]
      (is (= 15 (func))))))

;; Other 
;; in?

