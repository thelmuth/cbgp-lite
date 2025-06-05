(ns erp12.cbgp-lite.lang.decompile-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.tools.analyzer.jvm :as ana.jvm]
            [erp12.cbgp-lite.lang.decompile :as de]))

;; To test only this file:
;; clj -X:test :only erp12.cbgp-lite.lang.decompile-test 

(deftest decompile-constants-test
  (is (= (de/decompile-ast (ana.jvm/analyze 5))
         '({:gene :lit, :type {:type int?}, :val 5})))
  (is (= (de/decompile-ast (ana.jvm/analyze true))
         '({:gene :lit, :type {:type boolean?}, :val true})))
  (is (= (de/decompile-ast (ana.jvm/analyze nil))
         '({:gene :lit, :type {:type nil?}, :val nil})))
  (is (= (de/decompile-ast (ana.jvm/analyze 43.12))
         '({:gene :lit, :type {:type double?}, :val 43.12})))

  ;; Vectors
  (is (= '({:gene :lit, :val [1 2 7], :type {:type :vector :child {:type int?}}})
         (de/decompile-ast (ana.jvm/analyze [1 2 7]))))
  (is (= '({:gene :lit, :val [6.1 32.003], :type {:type :vector :child {:type double?}}})
         (de/decompile-ast (ana.jvm/analyze [6.1 32.003]))))
  (is (=
       '({:gene :lit, :val ["hi" "there" "everyone"], :type {:type :vector :child {:type string?}}})
       (de/decompile-ast (ana.jvm/analyze ["hi" "there" "everyone"]))))
  (is (=
       '({:gene :lit, :val [[[true false] [false] [true true]] [[true]] [[true true true] [false false false]]], :type {:type :vector :child {:type :vector :child {:type :vector :child {:type boolean?}}}}})
       (de/decompile-ast (ana.jvm/analyze [[[true false] [false] [true true]] [[true]] [[true true true] [false false false]]]))))

  ;; Sets
  (is (=
       (list {:gene :lit, :val #{8 2 0}, :type {:type :set :child {:type 'int?}}})
       (de/decompile-ast (ana.jvm/analyze #{8 2 0}))))

  ;; Maps
  (is (=
       (list {:gene :lit, :type {:key {:type 'int?}, :type :map-of, :value {:type 'string?}}, :val {1 "asd", 5 "asdfff"}})
       (de/decompile-ast (ana.jvm/analyze {1 "asd" 5 "asdfff"}))))

  ;; Treat quoted lists as vectors
  (is (=
       (de/decompile-ast (ana.jvm/analyze '(quote ("string" "hi"))))
       (de/decompile-ast (ana.jvm/analyze  ''("string" "hi")))
       '({:gene :lit, :type {:child {:type string?}, :type :vector}, :val ["string" "hi"]}))))

(deftest decompile-recompile-constants-test
  (is (= (map #(de/compile-debugging (de/decompile-ast (ana.jvm/analyze %))
                                     {:type %2})
              '(-5 0 2999 true nil 43.12)
              '(int? int? int? boolean? nil? double?))
         '(-5 0 2999 true nil 43.12)))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze [1 2 7]))
                               {:type :vector :child {:type 'int?}})
         [1 2 7]))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze [[[true false] [false] [true true]] [[true]] [[true true true] [false false false]]]))
                               {:type :vector :child {:type :vector :child {:type :vector :child {:type 'boolean?}}}})
         [[[true false] [false] [true true]] [[true]] [[true true true] [false false false]]]))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze #{8 2 0}))
                               {:type :set :child {:type 'int?}})
         #{8 2 0}))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze {1 "asd" 5 "asdfff"}))
                               {:key {:type 'int?}, :type :map-of, :value {:type 'string?}})
         {1 "asd" 5 "asdfff"}))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(quote (4 6))))
                               {:child {:type 'int?}, :type :vector})
         [4 6])))

(deftest decompile-function-calls-test
  (is (= (de/decompile-ast (ana.jvm/analyze '(+ 22 33)))
         '({:gene :lit, :type {:type int?}, :val 33}
           {:gene :lit, :type {:type int?}, :val 22}
           {:gene :var, :name int-add}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(+ 22.2 33.3)))
         '({:gene :lit, :type {:type double?}, :val 33.3}
           {:gene :lit, :type {:type double?}, :val 22.2}
           {:gene :var, :name double-add}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(+ 22 (+ 33 44))))
         '({:gene :lit, :type {:type int?}, :val 44}
           {:gene :lit, :type {:type int?}, :val 33}
           {:gene :var, :name int-add}
           {:gene :apply}
           {:gene :lit, :type {:type int?}, :val 22}
           {:gene :var, :name int-add}
           {:gene :apply}))))

(deftest decompile-recompile-function-calls-test
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(+ 22 33)))
                               {:type 'int?})
         55))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(+ 22 (+ 33 44))))
                               {:type 'int?})
         99))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(+ 22.2 33.3)))
                               {:type 'double?})
         55.5)))

(deftest decompile-str-vec-test
  (is (= (de/decompile-ast (ana.jvm/analyze '(first [1 2 3])))
         '({:gene :lit, :type {:child {:type int?}, :type :vector}, :val [1 2 3]} {:gene :var, :name first} {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(first "Hello")))
         '({:gene :lit, :type {:type string?}, :val "Hello"} {:gene :var, :name first-str} {:gene :apply}))) 
  (is (= (de/decompile-ast (ana.jvm/analyze '(empty? [])))
         '({:gene :lit, :type {:child {:sym T, :type :s-var}, :type :vector}, :val []} {:gene :var, :name empty?} {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(last [1 2 3])))
      '({:gene :lit, :type {:child {:type int?}, :type :vector}, :val [1 2 3]} {:gene :var, :name last} {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(last "String")))
         '({:gene :lit, :type {:type string?}, :val "String"} {:gene :var, :name last-str} {:gene :apply})))
)


(deftest decompile-recompile-str-vec-test
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(first [1.1 2.2 3.3])))
                               {:type 'double?})
         1.1))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(first "Hello"))) 
                               {:type 'char?})
         \H))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(empty? ["Hi" "Hello" "Hey"])))
                               {:type 'boolean?})
         false))
  
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(empty? [])))
                               {:type 'boolean?})
         true))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(empty? ""))) 
                               {:type 'boolean?})
         true))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(last [\C \a \t]))) {:type 'char?})
         \t)) 
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(last "String")))
                            {:type 'char?})
         \g))
  )