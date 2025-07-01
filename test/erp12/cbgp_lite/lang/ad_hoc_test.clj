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
(deftest first-test
  (testing "vector-first-test"
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

  (testing "string-first-test"
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

  (testing "set-first-test"
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

  (testing "map-first-test"
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
      (is (= 42 (func))))))

;;last
(deftest last-test
  (testing "vector-last-test"
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

  (testing "string-last-test"
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
      (is (= \o (func))))))

;;rest [!!]
(deftest rest-test
  (testing "vector-rest-test"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val [89 98] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :lit :val [55 66 77] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :var :name `lib/rest'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :vector :child {:type 'int?}}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= type {:type :vector :child {:type 'int?}}))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [] ~form))]
      (is (= [66 77] (func)))))

  (testing "string-rest-test"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val "hi" :type {:type 'string?}}
                                                    {:gene :lit :val "Hello" :type {:type 'string?}}
                                                    {:gene :var :name `lib/rest'}
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
      (is (= "ello" (func))))))

;;butlast[!!]
(deftest butlast-test
  (testing "vector-butlast-test"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val [89 98] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :lit :val [55 66 77] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :var :name `lib/butlast'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :vector :child {:type 'int?}}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= type {:type :vector :child {:type 'int?}}))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [] ~form))]
      (is (= [55 66] (func)))))

  (testing "string-butlast-test"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val "hi" :type {:type 'string?}}
                                                    {:gene :lit :val "Hello" :type {:type 'string?}}
                                                    {:gene :var :name `lib/butlast'}
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
      (is (= "Hell" (func))))))

;;nth 
(deftest nth-test
  (testing "vector-nth-test"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val 99 :type {:type 'int?}}
                                                    {:gene :lit :val [55 66 77] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :lit :val 2 :type {:type 'int?}}
                                                    {:gene :var :name `lib/safe-nth}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'int?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'int?} type))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [] ~form))]
      (is (= 77 (func))))) 
  (testing "string-nth-test"
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
      (is (= \o (func))))))

(deftest take-test
  (testing "Take String"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val "Hello there" :type {:type 'string?}}
                                                    {:gene :lit :val 5 :type {:type 'int?}}
                                                    {:gene :var :name `lib/take'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'string?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'string?} type))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [] ~form))]
      (is (= "Hello" (func)))))
  (testing "Take Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val [1 2 3 4 5 6 7 8 9] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :lit :val 5 :type {:type 'int?}}
                                                    {:gene :var :name `lib/take'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :vector :child {:type 'int?}}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type :vector :child {:type 'int?}} type))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [] ~form))]
      (is (= [1 2 3 4 5] (func))))))

(deftest reverse-test
  (testing "Reverse Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val [1 2 3 4 5] :type {:type :vector :child {:type 'int?}}} 
                                                    {:gene :var :name `lib/reverse'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :vector :child {:type 'int?}}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type :vector :child {:type 'int?}} type))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [] ~form))]
      (is (= [5 4 3 2 1] (func)))))
  (testing "Reverse String"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val "notlimaH" :type {:type 'string?}} 
                                                    {:gene :var :name `lib/reverse'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'string?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'string?} type))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [] ~form))]
      (is (= "Hamilton" (func))))))

(deftest safe-sub-test
  (testing "safe-substring test"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val 3 :type {:type 'int?}}
                                                    {:gene :lit :val 0 :type {:type 'int?}}
                                                    {:gene :lit :val "Hamilton" :type {:type 'string?}} 
                                                    {:gene :var :name `lib/safe-sub}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'string?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'string?} type))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [] ~form))]
      (is (= "Ham" (func))))))
;; remove element 

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
<<<<<<< HEAD
         (func "test")))))     
=======
         (func "test")))))
     
>>>>>>> schen/llmgp/ad-hoc
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

(deftest remove-test
  (testing "vector-remove-test"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val [6] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :lit :val [55 66 0 77 0] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :var :name 'zero?}
                                                    {:gene :var :name `lib/remove'}
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

  (testing "set-remove-test"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val #{6} :type {:type :set :child {:type 'int?}}}
                                                    {:gene :lit :val #{55 66 0 77 8} :type {:type :set :child {:type 'int?}}}
                                                    {:gene :var :name 'zero?}
                                                    {:gene :var :name `lib/remove'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :set :child {:type 'int?}}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= :set (:type type)))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [] ~form))]
      (is (= #{55 66 77 8} (func)))))

  #_(testing "map-remove-test"
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

  (testing "string-remove-test"
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
      (is (= "h there sabelle" (func))))))

(deftest filter-test
  (testing "Filter Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val [6] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :lit :val [55 66 0 77 0] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :var :name 'zero?}
                                                    {:gene :var :name `lib/filter'}
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
      (is (= [0 0] (func)))))
  (testing "Filter Set"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val #{6} :type {:type :set :child {:type 'int?}}}
                                                    {:gene :lit :val #{55 66 0 77 8} :type {:type :set :child {:type 'int?}}}
                                                    {:gene :var :name 'zero?}
                                                    {:gene :var :name `lib/filter'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type :set :child {:type 'int?}}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= :set (:type type)))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [] ~form))]
      (is (= #{0} (func)))))
  
  #_(testing "map-remove-test"
      (let [{::c/keys [ast type]} (:ast (c/push->ast
                                         {:push      [{:gene :lit :val {1 4 98 3} :type {:type :map-of :key {:type 'int?} :value {:type 'int?}}}
                                                      {:gene :lit :val {1 2 3 4 0 0} :type {:type :map-of :key {:type 'int?} :value {:type 'int?}}}
                                                      {:gene :fn :arg-types [lib/tuple-of lib/INT lib/INT] :ret-type lib/BOOLEAN}
                                                      [{:gene :local :idx 0}
                                                       {:gene :local :idx 1}
                                                       {:gene :var :name '>}
                                                       {:gene :apply}]
                                                      {:gene :var :name `lib/filter'}
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
  
  (testing "string-remove-test"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val \k :type {:type 'char?}}
                                                    {:gene :lit :val "hi there isabelle" :type {:type 'string?}}
                                                    {:gene :fn :arg-types [lib/CHAR] :ret-type lib/BOOLEAN}
                                                    [{:gene :local :idx 0}
                                                     {:gene :lit :val \i :type {:type 'char?}}
                                                     {:gene :var :name '=}
                                                     {:gene :apply}]
                                                    {:gene :var :name `lib/filter'}
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
      (is (= "ii" (func))))))

(deftest mapcat-vector-test
  (testing "mapcat Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val [6 5 9] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :lit :val [[1 2 3] [55 6 98]] :type {:type :vector :child {:type :vector :child {:type 'int?}}}}
                                                    {:gene :var :name `lib/reverse'}
                                                    {:gene :var :name `lib/mapcat'}
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
      (is (= [3 2 1 98 6 55] (func))))))

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

;; ; broken, wrong form: ((partial * 100) in1)
;; ; reverse schema order: ((partial in1 count *) 100)
;; (deftest partial1-fn3-test
;;   (println)
;;   (println "---STARTING PARTIAL 2 TEST---")
;;   (let [{::c/keys [ast type]} (:ast (c/push->ast
;;                                      {:push      [{:gene :local :idx 0}
;;                                                   {:gene :lit :val 100 :type {:type 'int?}}
;;                                                   {:gene :var :name '*}
;;                                                   {:gene :var :name 'count}
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
;;     (is (= 500 (func [1 2 3 4 5])))))

;; ; broken, wrong form: ((partial * 100) in1)
;; ; reverse schema order: ((partial in1 count *) 100)
;; (deftest partial2-fn3-test
;;   (println)
;;   (println "---STARTING PARTIAL 3 TEST---")
;;   (let [{::c/keys [ast type]} (:ast (c/push->ast
;;                                      {:push      [{:gene :local :idx 0}
;;                                                   {:gene :lit :val 100 :type {:type 'int?}}
;;                                                   {:gene :var :name '*}
;;                                                   {:gene :var :name 'count}
;;                                                   {:gene :var :name 'partial}
;;                                                   {:gene :apply}
;;                                                   {:gene :apply}]
;;                                       :locals    ['in1 'in2]
;;                                       :ret-type  (lib/s-var 'a)
;;                                       :type-env  (assoc lib/type-env
;;                                                         'in1 {:type :s-var :sym 'a}
;;                                                         'in2 {:type :s-var :sym 'a})
;;                                       :dealiases lib/dealiases}))
;;         _ (is (= 'int? (:type type)))
;;         _ (println "REAL-AST: " ast)
;;         form (a/ast->form ast)
;;         _ (println "FORM: " form)
;;         func (eval `(fn [~'in1 ~'in2] ~form))]
;;     (is (= [500 300] (func [1 2 3 4 5] [1 3 4])))))

;; Other 
;; in?

(deftest sort-test
  (testing "Sort Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val [6] :type {:type :vector :child {:type 'int?}}}
                                                    {:gene :lit :val [9 4 1 4 3 15] :type {:type :vector :child {:type 'int?}}} 
                                                    {:gene :var :name `lib/sort'}
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
      (is (= [1 3 4 4 9 15] (func)))))
  (testing "Sort Vector"
    (let [{::c/keys [ast type]} (:ast (c/push->ast
                                       {:push      [{:gene :lit :val "fledcbxa" :type {:type 'string?}}
                                                    {:gene :var :name `lib/sort'}
                                                    {:gene :apply}]
                                        :locals    []
                                        :ret-type  {:type 'string?}
                                        :type-env  lib/type-env
                                        :dealiases lib/dealiases}))
          _ (is (= {:type 'string?} type))
          _ (println "REAL-AST: " ast)
          form (a/ast->form ast)
          _ (println "FORM: " form)
          func (eval `(fn [] ~form))]
      (is (= "abcdeflx" (func))))))
