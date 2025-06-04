(ns erp12.cbgp-lite.lang.decompile-test
  (:require [clojure.test :refer [deftest is testing]]
            [erp12.cbgp-lite.lang.decompile :as de]
            [clojure.tools.analyzer.jvm :as ana.jvm]))

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
         '({:gene :lit, :type {:type double?}, :val 43.12}))))


(deftest decompile-recompile-constants-test
  (is (= (map #(de/compile-debugging (de/decompile-ast (ana.jvm/analyze %))
                                     {:type %2})
              '(-5 0 2999 true nil 43.12)
              '(int? int? int? boolean? nil? double?))
         '(-5 0 2999 true nil 43.12))))

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