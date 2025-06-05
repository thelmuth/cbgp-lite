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
  (is (= (de/decompile-ast (ana.jvm/analyze "hello"))
         '({:gene :lit, :type {:type string?}, :val "hello"})))
  (is (= (de/decompile-ast (ana.jvm/analyze \d))
         '({:gene :lit, :type {:type char?}, :val \d})))
  
;; Vectors
  (is (= '({:gene :lit, :val [1 2 7], :type {:type :vector :child {:type int?}}})
         (de/decompile-ast (ana.jvm/analyze [1 2 7]))))
  (is (= '({:gene :lit, :val [6.1 32.003], :type {:type :vector :child {:type double?}}})
         (de/decompile-ast (ana.jvm/analyze [6.1 32.003]))))
  (is (= (de/decompile-ast (ana.jvm/analyze ["hi" "there" "everyone"]))
         '({:gene :lit, :val ["hi" "there" "everyone"], :type {:type :vector :child {:type string?}}})))
  (is (= (de/decompile-ast (ana.jvm/analyze [[[true false] [false] [true true]] [[true]] [[true true true] [false false false]]]))
         '({:gene :lit, :val [[[true false] [false] [true true]] [[true]] [[true true true] [false false false]]], :type {:type :vector :child {:type :vector :child {:type :vector :child {:type boolean?}}}}})))
  (is (= (de/decompile-ast (ana.jvm/analyze []))
         '({:gene :lit, :type {:child {:sym T, :type :s-var}, :type :vector}, :val []})))

  ;; Sets
  (is (= (de/decompile-ast (ana.jvm/analyze #{8 2 0}))
         (list {:gene :lit, :val #{8 2 0}, :type {:type :set :child {:type 'int?}}})))
  (is (=
       (de/decompile-ast (ana.jvm/analyze #{}))
       '({:gene :lit, :type {:child {:sym T, :type :s-var}, :type :set}, :val #{}})))

  ;; Maps
  (is (=
       (list {:gene :lit, :type {:key {:type 'int?}, :type :map-of, :value {:type 'string?}}, :val {1 "asd", 5 "asdfff"}})
       (de/decompile-ast (ana.jvm/analyze {1 "asd" 5 "asdfff"}))))
  (is (=
       (de/decompile-ast (ana.jvm/analyze {}))
       '({:gene :lit, :type {:key {:sym T, :type :s-var}, :type :map-of, :value {:sym S, :type :s-var}}, :val {}})))
  
  ;; Treat quoted lists as vectors
  (is (=
       (de/decompile-ast (ana.jvm/analyze '(quote ("string" "hi"))))
       (de/decompile-ast (ana.jvm/analyze  ''("string" "hi")))
       '({:gene :lit, :type {:child {:type string?}, :type :vector}, :val ["string" "hi"]})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(quote ())))
         '({:gene :lit, :type {:child {:sym T, :type :s-var}, :type :vector}, :val []})))
  )

(deftest decompile-recompile-constants-test
  (is (= (map #(de/compile-debugging (de/decompile-ast (ana.jvm/analyze %))
                                     {:type %2})
              '(-5 0 2999 true nil 43.12 \d "hey hey")
              '(int? int? int? boolean? nil? double? char? string?))
         '(-5 0 2999 true nil 43.12 \d "hey hey")))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze [1 2 7]))
                               {:type :vector :child {:type 'int?}})
         [1 2 7])) 
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze ["hey" "hello" "yo!"]))
                               {:type :vector :child {:type 'string?}})
         ["hey" "hello" "yo!"])) 
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze [1.3 2.45 7.0098]))
                               {:type :vector :child {:type 'double?}})
         [1.3 2.45 7.0098])) 
  
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze [1 2 7]))
                               {:type :vector :child {:type 'int?}})
         [1 2 7]))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze [[[true false] [false] [true true]] [[true]] [[true true true] [false false false]]]))
                               {:type :vector :child {:type :vector :child {:type :vector :child {:type 'boolean?}}}})
         [[[true false] [false] [true true]] [[true]] [[true true true] [false false false]]]))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze #{8 2 0}))
                               {:type :set :child {:type 'int?}})
         #{8 2 0}))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(quote (4 6))))
                               {:child {:type 'int?}, :type :vector})
         [4 6]))
  

  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze {1 "asd" 5 "asdfff"}))
                               {:key {:type 'int?}, :type :map-of, :value {:type 'string?}}) 
         {1 "asd" 5 "asdfff"}))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze {[[7 8] [9 30]] #{"this" "is"} [[45 67][7 0]] #{ "absolutely" "insane"}}))
                               {:key {:type :vector :child {:type :vector :child {:type 'int?}}}, :type :map-of, :value {:type :set :child {:type 'string?}}})
         {[[7 8] [9 30]] #{"this" "is"} [[45 67] [7 0]] #{"absolutely" "insane"}}))
  
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze {[1] "asd" [5] "asdfff"})) 
                                    {:key {:type :vector :child {:type 'int?}}, :type :map-of, :value {:type 'string?}})
              {[1] "asd" [5] "asdfff"})) 
  )

(deftest decompile-function-calls-test
  ;; mathematical operations
  (is (= (de/decompile-ast (ana.jvm/analyze '(+ 22 33)))
         '({:gene :lit, :type {:type int?}, :val 33}
           {:gene :lit, :type {:type int?}, :val 22}
           {:gene :var, :name int-add}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(* 3 4)))
         '({:gene :lit, :type {:type int?}, :val 4}
           {:gene :lit, :type {:type int?}, :val 3}
           {:gene :var, :name int-mult}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(/ 3 4)))
         '({:gene :lit, :type {:type int?}, :val 4}
           {:gene :lit, :type {:type int?}, :val 3}
           {:gene :var, :name int-div}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(/ 3.5 7.0)))
         '({:gene :lit, :type {:type double?}, :val 7.0}
           {:gene :lit, :type {:type double?}, :val 3.5}
           {:gene :var, :name double-div}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(* 3.2 4.4)))
         '({:gene :lit, :type {:type double?}, :val 4.4}
           {:gene :lit, :type {:type double?}, :val 3.2}
           {:gene :var, :name double-mult}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(quot 80 7)))
         '({:gene :lit, :type {:type int?}, :val 7}
           {:gene :lit, :type {:type int?}, :val 80}
           {:gene :var, :name int-quot}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(quot 80.3 7.8)))
         '({:gene :lit, :type {:type double?}, :val 7.8}
           {:gene :lit, :type {:type double?}, :val 80.3}
           {:gene :var, :name double-quot}
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
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(- 22 33)))
         '({:gene :lit, :type {:type int?}, :val 33}
           {:gene :lit, :type {:type int?}, :val 22}
           {:gene :var, :name int-sub}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(- 22.5 33.5)))
         '({:gene :lit, :type {:type double?}, :val 33.5}
           {:gene :lit, :type {:type double?}, :val 22.5}
           {:gene :var, :name double-sub}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(- 22 (- 33 44))))
         '({:gene :lit, :type {:type int?}, :val 44}
           {:gene :lit, :type {:type int?}, :val 33}
           {:gene :var, :name int-sub}
           {:gene :apply}
           {:gene :lit, :type {:type int?}, :val 22}
           {:gene :var, :name int-sub}
           {:gene :apply})))

  ;; comparison (<, <=, >, >=)
  (is (= (de/decompile-ast (ana.jvm/analyze '(< 4 4)))
         '({:gene :lit, :type {:type int?}, :val 4}
           {:gene :lit, :type {:type int?}, :val 4}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/<'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(< \a \b)))
         '({:gene :lit, :type {:type char?}, :val \b}
           {:gene :lit, :type {:type char?}, :val \a}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/<'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(< "hi" "there")))
         '({:gene :lit, :type {:type string?}, :val "there"}
           {:gene :lit, :type {:type string?}, :val "hi"}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/<'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(<= 4 4)))
         '({:gene :lit, :type {:type int?}, :val 4}
           {:gene :lit, :type {:type int?}, :val 4}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/<='}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(<= \a \a)))
         '({:gene :lit, :type {:type char?}, :val \a}
           {:gene :lit, :type {:type char?}, :val \a}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/<='}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(< "you there" "hi!")))
         '({:gene :lit, :type {:type string?}, :val "hi!"}
           {:gene :lit, :type {:type string?}, :val "you there"}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/<'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(> 5 4)))
         '({:gene :lit, :type {:type int?}, :val 4}
           {:gene :lit, :type {:type int?}, :val 5}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/>'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(> \a \b)))
         '({:gene :lit, :type {:type char?}, :val \b}
           {:gene :lit, :type {:type char?}, :val \a}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/>'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(> "hi" "there")))
         '({:gene :lit, :type {:type string?}, :val "there"}
           {:gene :lit, :type {:type string?}, :val "hi"}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/>'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(>= 5 5)))
         '({:gene :lit, :type {:type int?}, :val 5}
           {:gene :lit, :type {:type int?}, :val 5}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/>='}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(>= \a \b)))
         '({:gene :lit, :type {:type char?}, :val \b}
           {:gene :lit, :type {:type char?}, :val \a}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/>='}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(>= "hi" "there")))
         '({:gene :lit, :type {:type string?}, :val "there"}
           {:gene :lit, :type {:type string?}, :val "hi"}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/>='}
           {:gene :apply})))
  
  ;;max and min
  (is (= (de/decompile-ast (ana.jvm/analyze '(max 1 2 3)))
       '({:gene :lit, :type {:type int?}, :val 3}
         {:gene :lit, :type {:type int?}, :val 2}
         {:gene :lit, :type {:type int?}, :val 1}
         {:gene :var, :name erp12.cbgp-lite.lang.lib/max'}
         {:gene :apply}
         {:gene :var, :name erp12.cbgp-lite.lang.lib/max'}
         {:gene :apply}))) 
  (is (= (de/decompile-ast (ana.jvm/analyze '(max 1.3 2.2 3.9)))
         '({:gene :lit, :type {:type double?}, :val 3.9}
           {:gene :lit, :type {:type double?}, :val 2.2}
           {:gene :lit, :type {:type double?}, :val 1.3}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/max'}
           {:gene :apply}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/max'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(max \a \g \v)))
         '({:gene :lit, :type {:type char?}, :val \v}
           {:gene :lit, :type {:type char?}, :val \g}
           {:gene :lit, :type {:type char?}, :val \a}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/max'}
           {:gene :apply}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/max'} 
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(max "ai" "smile" "go")))
       '({:gene :lit, :type {:type string?}, :val "go"}
         {:gene :lit, :type {:type string?}, :val "smile"}
         {:gene :lit, :type {:type string?}, :val "ai"}
         {:gene :var, :name erp12.cbgp-lite.lang.lib/max'}
         {:gene :apply} 
         {:gene :var, :name erp12.cbgp-lite.lang.lib/max'} 
         {:gene :apply})))
  
  (is (= (de/decompile-ast (ana.jvm/analyze '(min 1 2 3)))
         '({:gene :lit, :type {:type int?}, :val 3}
           {:gene :lit, :type {:type int?}, :val 2}
           {:gene :lit, :type {:type int?}, :val 1}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/min'}
           {:gene :apply}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/min'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(min 1.3 2.2 3.9)))
         '({:gene :lit, :type {:type double?}, :val 3.9}
           {:gene :lit, :type {:type double?}, :val 2.2}
           {:gene :lit, :type {:type double?}, :val 1.3}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/min'}
           {:gene :apply}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/min'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(min \a \g \v)))
         '({:gene :lit, :type {:type char?}, :val \v}
           {:gene :lit, :type {:type char?}, :val \g}
           {:gene :lit, :type {:type char?}, :val \a}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/min'}
           {:gene :apply}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/min'}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(min "ai" "smile" "go")))
         '({:gene :lit, :type {:type string?}, :val "go"}
           {:gene :lit, :type {:type string?}, :val "smile"}
           {:gene :lit, :type {:type string?}, :val "ai"}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/min'}
           {:gene :apply}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/min'}
           {:gene :apply})))
  
  ;; not
  (is (= (de/decompile-ast (ana.jvm/analyze '(not true)))
         '({:gene :lit, :type {:type boolean?}, :val true}
           {:gene :var, :name not}
           {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(not (< 4 5))))
         '({:gene :lit, :type {:type int?}, :val 5}
           {:gene :lit, :type {:type int?}, :val 4}
           {:gene :var, :name erp12.cbgp-lite.lang.lib/<'}
           {:gene :apply}
           {:gene :var, :name not}
           {:gene :apply})))

  ;;math with 4+ arguements 
  (is (= (de/decompile-ast (ana.jvm/analyze '(+ 1 2 3 4)))
         '({:gene :lit, :type {:type int?}, :val 4}
           {:gene :lit, :type {:type int?}, :val 3}
           {:gene :lit, :type {:type int?}, :val 2}
           {:gene :lit, :type {:type int?}, :val 1}
           {:gene :var, :name int-add}
           {:gene :apply}
           {:gene :var, :name int-add}
           {:gene :apply}
           {:gene :var, :name int-add}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(* 1 2 3 4)))
         '({:gene :lit, :type {:type int?}, :val 4}
           {:gene :lit, :type {:type int?}, :val 3}
           {:gene :lit, :type {:type int?}, :val 2}
           {:gene :lit, :type {:type int?}, :val 1}
           {:gene :var, :name int-mult}
           {:gene :apply}
           {:gene :var, :name int-mult}
           {:gene :apply}
           {:gene :var, :name int-mult}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(- 1 2 3 4)))
         '({:gene :lit, :type {:type int?}, :val 4}
           {:gene :lit, :type {:type int?}, :val 3}
           {:gene :lit, :type {:type int?}, :val 2}
           {:gene :lit, :type {:type int?}, :val 1}
           {:gene :var, :name int-sub}
           {:gene :apply}
           {:gene :var, :name int-sub}
           {:gene :apply}
           {:gene :var, :name int-sub}
           {:gene :apply})))
  
  (is (= (de/decompile-ast (ana.jvm/analyze '(/ 1 2 3 4)))
         '({:gene :lit, :type {:type int?}, :val 4}
          {:gene :lit, :type {:type int?}, :val 3}
          {:gene :lit, :type {:type int?}, :val 2}
          {:gene :lit, :type {:type int?}, :val 1}
          {:gene :var, :name int-div}
          {:gene :apply}
          {:gene :var, :name int-div}
          {:gene :apply}
          {:gene :var, :name int-div}
          {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(+ 1.1 2.2 3.3 4.4)))
         '({:gene :lit, :type {:type double?}, :val 4.4}
           {:gene :lit, :type {:type double?}, :val 3.3}
           {:gene :lit, :type {:type double?}, :val 2.2}
           {:gene :lit, :type {:type double?}, :val 1.1}
           {:gene :var, :name double-add}
           {:gene :apply}
           {:gene :var, :name double-add}
           {:gene :apply}
           {:gene :var, :name double-add}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(* 1.1 2.2 3.3 4.4)))
         '({:gene :lit, :type {:type double?}, :val 4.4}
           {:gene :lit, :type {:type double?}, :val 3.3}
           {:gene :lit, :type {:type double?}, :val 2.2}
           {:gene :lit, :type {:type double?}, :val 1.1}
           {:gene :var, :name double-mult}
           {:gene :apply}
           {:gene :var, :name double-mult}
           {:gene :apply}
           {:gene :var, :name double-mult}
           {:gene :apply})))

  (is (= (de/decompile-ast (ana.jvm/analyze '(- 1.1 2.2 3.3 4.4)))
         '({:gene :lit, :type {:type double?}, :val 4.4}
           {:gene :lit, :type {:type double?}, :val 3.3}
           {:gene :lit, :type {:type double?}, :val 2.2}
           {:gene :lit, :type {:type double?}, :val 1.1}
           {:gene :var, :name double-sub}
           {:gene :apply}
           {:gene :var, :name double-sub}
           {:gene :apply}
           {:gene :var, :name double-sub}
           {:gene :apply})))
  
  (is (= (de/decompile-ast (ana.jvm/analyze '(/ 1.1 2.2 3.3 4.4)))
       '({:gene :lit, :type {:type double?}, :val 4.4}
         {:gene :lit, :type {:type double?}, :val 3.3}
         {:gene :lit, :type {:type double?}, :val 2.2}
         {:gene :lit, :type {:type double?}, :val 1.1}
         {:gene :var, :name double-div}
         {:gene :apply}
         {:gene :var, :name double-div}
         {:gene :apply}
         {:gene :var, :name double-div}
         {:gene :apply})))
  

  ;;inc and dec
  (is (= (de/decompile-ast (ana.jvm/analyze '(inc 4)))
         '({:gene :lit, :type {:type int?}, :val 4} {:gene :var, :name int-inc} {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(dec 4)))
         '({:gene :lit, :type {:type int?}, :val 4} {:gene :var, :name int-dec} {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(inc 4.2)))
         '({:gene :lit, :type {:type double?}, :val 4.2} {:gene :var, :name double-inc} {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(dec 4.2)))
         '({:gene :lit, :type {:type double?}, :val 4.2} {:gene :var, :name double-dec} {:gene :apply}))))

(deftest decompile-recompile-function-calls-test
  ;; mathematical operations
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(+ 22 33)))
                               {:type 'int?})
         55)) 
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(+ 22 (+ 33 44))))
                               {:type 'int?})
         99)) 
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(+ 22.2 33.3)))
                               {:type 'double?})
         55.5))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(- 22 33)))
                               {:type 'int?})
         -11))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(- 22.5 33.5)))
                               {:type 'double?})
         -11.0))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(- 22 (- 33 44))))
                               {:type 'int?})
         33))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(* 3 4)))
                               {:type 'int?})
         12))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(* 3.2 4.2)))
                               {:type 'double?})
         13.440000000000001))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(quot 80 7)))
                               {:type 'int?})
         11))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(quot 80.3 7.8)))
                               {:type 'double?})
         10.0))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(/ 3 4)))
                               {:type 'double?})
         3/4))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(/ 3.5 7.0)))
                               {:type 'double?})
         0.5))

  ;; comparison (<, <=, >, >=)
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(< 4 4)))
                               {:type 'boolean?})
         false))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(< \a \b)))
                               {:type 'boolean?})
         true))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(< "hi" "there")))
                               {:type 'boolean?})
         true))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(<= 4 4)))
                               {:type 'boolean?})
         true))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(<= \a \a)))
                               {:type 'boolean?})
         true))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(< "you there" "hi!")))
                               {:type 'boolean?})
         false))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(> 5 4)))
                               {:type 'boolean?})
         true))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(> \a \b)))
                               {:type 'boolean?})
         false))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(> "hi" "there")))
                               {:type 'boolean?})
         false))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(>= 5 5)))
                               {:type 'boolean?})
         true))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(>= \a \b)))
                               {:type 'boolean?})
         false))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(>= "hi" "there")))
                               {:type 'boolean?})
         false))

  ;;max and min
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(max "hi" "yellow" "there")))
                               {:type 'string?})
         "yellow"))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(max \d \f \a)))
                               {:type 'char?})
         \f))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(max 2 98 5)))
                               {:type 'int?})
         98))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(max 9.7 98.3 5.0974)))
                               {:type 'double?})
         98.3))

  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(min "hi" "yellow" "there")))
                               {:type 'string?})
         "hi"))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(min \d \f \a)))
                               {:type 'char?})
         \a))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(min 2 98 5)))
                               {:type 'int?})
         2))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(min 9.7 98.3 5.0974)))
                               {:type 'double?})
         5.0974))

  ;; not
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(not true)))
                               {:type 'boolean?})
         false))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(not (< 4 5))))
                               {:type 'boolean?})
         false))

  ;;inc and dec
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(dec 5)))
                               {:type 'int?})
         4))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(dec 5.4)))
                               {:type 'double?})
         4.4))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(inc 5)))
                               {:type 'int?})
         6))

  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(inc 5.4)))
                               {:type 'double?})
         6.4))

  ;;math with 4+ arguemnts
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(- 1.1 2.2 3.3 4.4)))
                               {:type 'double?})
         -8.8))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(+ 1.1 2.2 3.3 4.4)))
                               {:type 'double?})
         11.0))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(* 1.1 2.2 3.3 4.4)))
                               {:type 'double?})
         35.138400000000004))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(/ 1.1 2.2 3.3 4.4)))
                               {:type 'double?})
         0.03443526170798898))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(* 1 2 3 4)))
                               {:type 'int?})
         24))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(- 1 2 3 4)))
                               {:type 'int?})
         -8))
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(+ 1 2 3 4)))
                               {:type 'int?})
         10))
  ;;doesn't work
;;   (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(/ 1 2 3 4)))
;;                                {:type 'double?})
;;          1/24))
  
  ;; working through div for more the 2
  (is (= (de/compile-debugging (de/decompile-ast (ana.jvm/analyze '(/ 3 4)))
                               {:type 'double?})
     3/4))
  (de/compile-debugging '({:gene :lit, :type {:type int?}, :val 3}
                        {:gene :lit, :type {:type int?}, :val 2}
                        {:gene :lit, :type {:type int?}, :val 1}
                        {:gene :var, :name int-div}
                        {:gene :apply}
                        {:gene :var, :name int-div}
                        {:gene :apply}
                        {:gene :var, :name int-div}
                        {:gene :apply})
                      {:type 'int?}) 
  
   (is (= (de/decompile-ast (ana.jvm/analyze '(/ (/ 1 2) 3)))

         '({:gene :lit, :type {:type int?}, :val 3}
           {:gene :lit, :type {:type int?}, :val 2}
           {:gene :lit, :type {:type int?}, :val 1}
           {:gene :var, :name int-div}
           {:gene :apply}
           {:gene :var, :name int-div} 
           {:gene :apply})))
)

(deftest decompile-collections-test
  ;; empty collections 
  

  ;; strings & vectors
  (is (= (de/decompile-ast (ana.jvm/analyze '(first [1 2 3])))
         '({:gene :lit, :type {:child {:type int?}, :type :vector}, :val [1 2 3]} {:gene :var, :name first} {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(first "Hello")))
         '({:gene :lit, :type {:type string?}, :val "Hello"} {:gene :var, :name first-str} {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(empty? [])))
         '({:gene :lit, :type {:child {:sym T, :type :s-var}, :type :vector}, :val []} {:gene :var, :name empty?} {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(last [1 2 3])))
         '({:gene :lit, :type {:child {:type int?}, :type :vector}, :val [1 2 3]} {:gene :var, :name last} {:gene :apply})))
  (is (= (de/decompile-ast (ana.jvm/analyze '(last "String")))
         '({:gene :lit, :type {:type string?}, :val "String"} {:gene :var, :name last-str} {:gene :apply}))))

(deftest decompile-recompile-collections-test
  ;; empty collections 

  ; broken test; returns [], not [5]
;;   (is (= (de/compile-debugging
;;           (concat (de/decompile-ast (ana.jvm/analyze []))
;;                   ;; (list {:gene :lit, :val [], :type {:type :vector :child (`lib/s-var 'T)}})
;;                   (list {:gene :var :name `lib/conj-vec}
;;                         {:gene :lit :val 5 :type {:type 'int?}}
;;                         {:gene :apply}))
;;           {:type :vector :child {:type 'int?}}) 
;;          [5]))


  ;; strings & vectors
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
         \g)))