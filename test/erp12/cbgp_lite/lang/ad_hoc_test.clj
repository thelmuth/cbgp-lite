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

;; countable
(deftest count-test
  (let [{::c/keys [ast type]} (:ast (c/push->ast {:push      [{:gene :local :idx 0}
                                                              {:gene :var :name 'count}
                                                              {:gene :apply}]
                                                  :locals    ['in1]
                                                  :ret-type  {:type 'int?}
                                                  :type-env  (assoc lib/type-env
                                                                    'in1 {:type :s-var :sym 'c :typeclasses #{:countable}})
                                                  :dealiases lib/dealiases}))
        _ (is (= type {:type 'int?}))
        form (a/ast->form ast)
        func (eval `(fn [~'in1] ~form))]
    (is (= (func ["a" "b" "c"]) 3))
    (is (= (func [1 3 4 5 10]) 5))
    (is (= (func []) 0))
    (is (= (func #{1 2 3}) 3))
    (is (= (func {1 "hi" 2 "world"}) 2))
    (is (= (func "testing!") 8))
    #_(is (= (func 4) 1))))

(deftest empty-test
  (let [{::c/keys [ast type]} (:ast (c/push->ast {:push      [{:gene :local :idx 0}
                                                              {:gene :var :name 'empty?}
                                                              {:gene :apply}]
                                                  :locals    ['in1]
                                                  :ret-type  {:type 'boolean?}
                                                  :type-env  (assoc lib/type-env
                                                                    'in1 {:type :s-var :sym 'c :typeclasses #{:countable}})
                                                  :dealiases lib/dealiases}))
        _ (is (= type {:type 'boolean?}))
        form (a/ast->form ast)
        func (eval `(fn [~'in1] ~form))]
    (is (= (func ["a" "b" "c"]) false))
    (is (= (func [1 3 4 5 10]) false))
    (is (= (func []) true))
    (is (= (func #{1 2 3}) false))
    (is (= (func {1 "hi" 2 "world"}) false))
    (is (= (func "testing!") false))
    (is (= (func "") true)) 
    (is (= (func #{}) true))))

#_(deftest index-of-test
  (let [{::c/keys [ast type]} (:ast (c/push->ast {:push      [{:gene :local :idx 0}
                                                              {:gene :var :name `lib/index-of}
                                                              {:gene :apply}]
                                                  :locals    ['in1]
                                                  :ret-type  {:type 'boolean?}
                                                  :type-env  (assoc lib/type-env
                                                                    'in1 {:type :s-var :sym 'c :typeclasses #{:countable}})
                                                  :dealiases lib/dealiases}))
        _ (is (= type {:type 'boolean?}))
        form (a/ast->form ast)
        func (eval `(fn [~'in1] ~form))]
    (is (= (func ["a" "b" "c"]) false))
    (is (= (func [1 3 4 5 10]) false))
    (is (= (func []) true))
    (is (= (func #{1 2 3}) false))
    (is (= (func {1 "hi" 2 "world"}) false))
    (is (= (func "testing!") false))
    (is (= (func "") true))
    (is (= (func #{}) true))))

;;indexable (:vector 'string?)
;;first
(deftest vector-first-test
  (let [{::c/keys [ast type]} (:ast (c/push->ast
                                     {:push      [{:gene :lit :val 42 :type {:type 'int?}}
                                                  {:gene :lit :val [55 66 77] :type {:type :vector :child {:type 'int?}}}
                                                  {:gene :var :name 'first}
                                                  {:gene :apply}]
                                      :locals    []
                                      :ret-type  {:type 'int?}
                                      :type-env  lib/type-env
                                      :dealiases lib/dealiases}))
        _ (is (= type {:type 'int?}))
        _ (println "REAL-AST: " ast)
        form (a/ast->form ast)
        _ (println "FORM: " form)
        func (eval `(fn [] ~form))]
    (is (= 55 (func)))))

(deftest string-first-test
  (let [{::c/keys [ast type]} (:ast (c/push->ast
                                     {:push      [{:gene :lit :val \j :type {:type 'char?}}
                                                  {:gene :lit :val "Hello" :type {:type 'string?}}
                                                  {:gene :var :name 'first}
                                                  {:gene :apply}]
                                      :locals    []
                                      :ret-type  {:type 'char?}
                                      :type-env  lib/type-env
                                      :dealiases lib/dealiases}))
        _ (is (= type {:type 'char?}))
        _ (println "REAL-AST: " ast)
        form (a/ast->form ast)
        _ (println "FORM: " form)
        func (eval `(fn [] ~form))]
    (is (= \H (func)))))

(deftest set-first-test
  (let [{::c/keys [ast type]} (:ast (c/push->ast
                                     {:push      [{:gene :lit :val 42 :type {:type 'int?}}
                                                  {:gene :lit :val #{1 2 3 4} :type {:type :set :child {:type 'int?}}}
                                                  {:gene :var :name 'first}
                                                  {:gene :apply}]
                                      :locals    []
                                      :ret-type  {:type 'int?}
                                      :type-env  lib/type-env
                                      :dealiases lib/dealiases}))
        _ (is (= type {:type 'int?}))
        _ (println "REAL-AST: " ast)
        form (a/ast->form ast)
        _ (println "FORM: " form)
        func (eval `(fn [] ~form))]
    (is (= 42 (func)))))

(deftest map-first-test
  (let [{::c/keys [ast type]} (:ast (c/push->ast
                                     {:push      [{:gene :lit :val 42 :type {:type 'int?}}
                                                  {:gene :lit :val #{1 2 3 4} :type {:type :map-of :key {:type 'int?} :value {:type 'int?}}}
                                                  {:gene :var :name 'first}
                                                  {:gene :apply}]
                                      :locals    []
                                      :ret-type  {:type 'int?}
                                      :type-env  lib/type-env
                                      :dealiases lib/dealiases}))
        _ (is (= type {:type 'int?}))
        _ (println "REAL-AST: " ast)
        form (a/ast->form ast)
        _ (println "FORM: " form)
        func (eval `(fn [] ~form))]
    (is (= 42 (func)))))

;;last
(deftest vector-last-test
  (let [{::c/keys [ast type]} (:ast (c/push->ast
                                     {:push      [{:gene :lit :val 42 :type {:type 'int?}}
                                                  {:gene :lit :val [55 66 77] :type {:type :vector :child {:type 'int?}}}
                                                  {:gene :var :name 'last}
                                                  {:gene :apply}]
                                      :locals    []
                                      :ret-type  {:type 'int?}
                                      :type-env  lib/type-env
                                      :dealiases lib/dealiases}))
        _ (is (= type {:type 'int?}))
        _ (println "REAL-AST: " ast)
        form (a/ast->form ast)
        _ (println "FORM: " form)
        func (eval `(fn [] ~form))]
    (is (= 77 (func)))))

(deftest string-last-test
  (let [{::c/keys [ast type]} (:ast (c/push->ast
                                     {:push      [{:gene :lit :val \j :type {:type 'char?}}
                                                  {:gene :lit :val "Hello" :type {:type 'string?}}
                                                  {:gene :var :name 'last}
                                                  {:gene :apply}]
                                      :locals    []
                                      :ret-type  {:type 'char?}
                                      :type-env  lib/type-env
                                      :dealiases lib/dealiases}))
        _ (is (= type {:type 'char?}))
        _ (println "REAL-AST: " ast)
        form (a/ast->form ast)
        _ (println "FORM: " form)
        func (eval `(fn [] ~form))]
    (is (= \o (func)))))

;;rest [!!]
(deftest vector-rest-test
  (let [{::c/keys [ast type]} (:ast (c/push->ast
                                     {:push      [{:gene :lit :val [89 98] :type {:type 'vector?}}
                                                  {:gene :lit :val [55 66 77] :type {:type :vector :child {:type 'int?}}}
                                                  {:gene :var :name 'rest}
                                                  {:gene :apply}]
                                      :locals    []
                                      :ret-type  {:type 'vector?} ;;[!!]
                                      :type-env  lib/type-env
                                      :dealiases lib/dealiases}))
        _ (is (= type {:type 'vector?}))
        _ (println "REAL-AST: " ast)
        form (a/ast->form ast)
        _ (println "FORM: " form)
        func (eval `(fn [] ~form))]
    (is (= [66 77] (func)))))

(deftest string-rest-test
  (let [{::c/keys [ast type]} (:ast (c/push->ast
                                     {:push      [{:gene :lit :val "hi" :type {:type 'string?}}
                                                  {:gene :lit :val "Hello" :type {:type 'string?}}
                                                  {:gene :var :name 'rest}
                                                  {:gene :apply}]
                                      :locals    []
                                      :ret-type  {:type 'string?}
                                      :type-env  lib/type-env
                                      :dealiases lib/dealiases}))
        _ (is (= type {:type 'string?}))
        _ (println "REAL-AST: " ast)
        form (a/ast->form ast)
        _ (println "FORM: " form)
        func (eval `(fn [] ~form))]
    (is (= "ello" (func)))))

;;butlast[!!]
(deftest vector-butlast-test
  (let [{::c/keys [ast type]} (:ast (c/push->ast
                                     {:push      [{:gene :lit :val [89 98] :type {:type 'vector?}}
                                                  {:gene :lit :val [55 66 77] :type {:type :vector :child {:type 'int?}}}
                                                  {:gene :var :name 'butlast}
                                                  {:gene :apply}]
                                      :locals    []
                                      :ret-type  {:type 'vector?} ;;[!!]
                                      :type-env  lib/type-env
                                      :dealiases lib/dealiases}))
        _ (is (= type {:type 'vector?}))
        _ (println "REAL-AST: " ast)
        form (a/ast->form ast)
        _ (println "FORM: " form)
        func (eval `(fn [] ~form))]
    (is (= "66 77" (func)))))

(deftest string-butlast-test
  (let [{::c/keys [ast type]} (:ast (c/push->ast
                                     {:push      [{:gene :lit :val "hi" :type {:type 'string?}}
                                                  {:gene :lit :val "Hello" :type {:type 'string?}}
                                                  {:gene :var :name 'butlast}
                                                  {:gene :apply}]
                                      :locals    []
                                      :ret-type  {:type 'string?}
                                      :type-env  lib/type-env
                                      :dealiases lib/dealiases}))
        _ (is (= type {:type 'string?}))
        _ (println "REAL-AST: " ast)
        form (a/ast->form ast)
        _ (println "FORM: " form)
        func (eval `(fn [] ~form))]
    (is (= "ello" (func)))))

;;nth 
(deftest vector-nth-test
  (let [{::c/keys [ast type]} (:ast (c/push->ast
                                     {:push      [{:gene :lit :val 99 :type {:type 'int?}}
                                                  {:gene :lit :val [55 66 77] :type {:type :vector :child {:type 'int?}}}
                                                  {:gene :lit :val 2 :type {:type 'int?}}
                                                  {:gene :var :name `lib/safe-nth}
                                                  {:gene :apply}]
                                      :locals    []
                                      :ret-type  {:type 'int?} ;;[!!]
                                      :type-env  lib/type-env
                                      :dealiases lib/dealiases}))
        _ (is (= {:type 'int?} type))
        _ (println "REAL-AST: " ast)
        form (a/ast->form ast)
        _ (println "FORM: " form)
        func (eval `(fn [] ~form))]
    (is (= 77 (func)))))

(deftest string-nth-test
  (let [{::c/keys [ast type]} (:ast (c/push->ast
                                     {:push      [{:gene :lit :val \k :type {:type 'char?}}
                                                  {:gene :lit :val "Hello there" :type {:type 'string?}}
                                                  {:gene :lit :val 4 :type {:type 'int?}}
                                                  {:gene :var :name `lib/safe-nth}
                                                  {:gene :apply}]
                                      :locals    []
                                      :ret-type  {:type 'char?}
                                      :type-env  lib/type-env
                                      :dealiases lib/dealiases}))
        _ (is (= {:type 'char?} type))
        _ (println "REAL-AST: " ast)
        form (a/ast->form ast)
        _ (println "FORM: " form)
        func (eval `(fn [] ~form))]
    (is (= \o (func)))))

;; contable 

(deftest vector-remove-test
  (let [{::c/keys [ast type]} (:ast (c/push->ast
                                     {:push      [{:gene :lit :val [6] :type {:type :vector :child {:type 'int?}}}
                                                  {:gene :lit :val [55 66 0 77 0] :type {:type :vector :child {:type 'int?}}}
                                                  {:gene :var :name 'zero?}
                                                  {:gene :var :name `lib/remove'}
                                                  {:gene :apply}]
                                      :locals    []
                                      :ret-type  {:type :vector :child {:type 'int?}} ;;[!!]
                                      :type-env  lib/type-env
                                      :dealiases lib/dealiases}))
        _ (is (= :vector (:type type)))
        _ (println "REAL-AST: " ast)
        form (a/ast->form ast)
        _ (println "FORM: " form)
        func (eval `(fn [] ~form))]
    (is (= [55 66 77] (func)))))

(deftest set-remove-test
  (let [{::c/keys [ast type]} (:ast (c/push->ast
                                     {:push      [{:gene :lit :val #{6} :type {:type :set :child {:type 'int?}}}
                                                  {:gene :lit :val #{55 66 0 77 8} :type {:type :set :child {:type 'int?}}}
                                                  {:gene :var :name 'zero?}
                                                  {:gene :var :name `lib/remove'}
                                                  {:gene :apply}]
                                      :locals    []
                                      :ret-type  {:type :set :child {:type 'int?}} ;;[!!]
                                      :type-env  lib/type-env
                                      :dealiases lib/dealiases}))
        _ (is (= :set (:type type)))
        _ (println "REAL-AST: " ast)
        form (a/ast->form ast)
        _ (println "FORM: " form)
        func (eval `(fn [] ~form))]
    (is (= #{55 66 77 8} (func)))))

#_(deftest map-remove-test
  (let [{::c/keys [ast type]} (:ast (c/push->ast
                                     {:push      [{:gene :lit :val {1 4 98 3} :type {:type :map-of :key {:type 'int?} :value {:type 'int?}}}
                                                  {:gene :lit :val {1 2 3 4 0 0} :type {:type :map-of :key {:type 'int?} :value {:type 'int?}}}
                                                   {:gene :fn :arg-types [lib/tuple-of lib/INT lib/INT] :ret-type lib/BOOLEAN}
                                                  [{:gene :local :idx 0}
                                                   {:gene :local :idx 1}
                                                   {:gene :var :name '>}
                                                   {:gene :apply}]
                                                  {:gene :var :name `lib/remove'}
                                                  {:gene :apply}]
                                      :locals    []
                                      :ret-type  {:type :map-of :key {:type 'int?} :value {:type 'int?}} ;;[!!]
                                      :type-env  lib/type-env
                                      :dealiases lib/dealiases}))
        _ (is (= :map-of (:type type)))
        _ (println "REAL-AST: " ast)
        form (a/ast->form ast)
        _ (println "FORM: " form)
        func (eval `(fn [] ~form))]
    (is (= {1 2} (func)))))

(deftest string-remove-test
  (let [{::c/keys [ast type]} (:ast (c/push->ast
                                     {:push      [{:gene :lit :val \k :type {:type 'char?}}
                                                  {:gene :lit :val "hi there isabelle" :type {:type 'string?}} 
                                                  {:gene :fn :arg-types [lib/CHAR] :ret-type lib/BOOLEAN}
                                                  [{:gene :local :idx 0}
                                                   {:gene :lit :val \i :type {:type 'char?}}
                                                   {:gene :var :name '=}
                                                   {:gene :apply}]
                                                  {:gene :var :name `lib/remove'}
                                                  {:gene :apply}]
                                      :locals    []
                                      :ret-type  {:type 'string?}
                                      :type-env  lib/type-env
                                      :dealiases lib/dealiases}))
        _ (is (= type {:type 'string?}))
        _ (println "REAL-AST: " ast)
        form (a/ast->form ast)
        _ (println "FORM: " form)
        func (eval `(fn [] ~form))]
    (is (= "h there sabelle" (func)))))

;; remvoe element 

#_(deftest vector-remove-element-test
  (let [{::c/keys [ast type]} (:ast (c/push->ast
                                     {:push      [{:gene :lit :val [6] :type {:type :vector :child {:type 'int?}}}
                                                  {:gene :lit :val [55 66 0 77 0] :type {:type :vector :child {:type 'int?}}}
                                                  {:gene :lit :val 77 :type 'int?}
                                                  {:gene :var :name `lib/remove-element}
                                                  {:gene :apply}]
                                      :locals    []
                                      :ret-type  {:type :vector :child {:type 'int?}}
                                      :type-env  lib/type-env
                                      :dealiases lib/dealiases}))
        _ (is (= :vector (:type type)))
        _ (println "REAL-AST: " ast)
        form (a/ast->form ast)
        _ (println "FORM: " form)
        func (eval `(fn [] ~form))]
    (is (= [55 66 77] (func)))))


#_(deftest string-remove-element-test
  (let [{::c/keys [ast type]} (:ast (c/push->ast
                                     {:push      [{:gene :lit :val "hello there" :type 'string?}
                                                  {:gene :lit :val "hello world!!" :type 'string?}
                                                  {:gene :lit :val \w  :type {:type 'char?}}
                                                  {:gene :var :name `lib/remove-element}
                                                  {:gene :apply}]
                                      :locals    []
                                      :ret-type  {:type 'string?}
                                      :type-env  lib/type-env
                                      :dealiases lib/dealiases}))
        _ (is (= 'string? (:type type)))
        _ (println "REAL-AST: " ast)
        form (a/ast->form ast)
        _ (println "FORM: " form)
        func (eval `(fn [] ~form))]
    (is (= "hello orld!!" (func)))))

;; Collection Conversion
;; Vec, Set, ->map

;; Combining collections
;; Conj, Concat, join

;; Higher Order functions
;; Reduce, Filter, Remove

;; Other 
;; remove-element, in?