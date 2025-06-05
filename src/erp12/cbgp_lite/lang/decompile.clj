(ns erp12.cbgp-lite.lang.decompile
  (:require [clojure.tools.analyzer.jvm :as ana.jvm]
            [erp12.cbgp-lite.lang.ast :as ast]
            [erp12.cbgp-lite.lang.compile :as co]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.search.plushy :as pl]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; Compilation testing

(defn compile-debugging
  ([genome ret-type]
   (compile-debugging genome ret-type false))
  ([genome ret-type verbose]
   (let [_ (when verbose (println "PLUSHY:" genome))
         push (pl/plushy->push genome)
         _ (when verbose (println "PUSH:" push))
         ast (::co/ast (co/push->ast {:push push
                                      :ret-type ret-type
                                      :type-env lib/type-env}))
         _ (when verbose (println "AST:" ast))
         form (ast/ast->form ast)
         _ (when verbose (println "FORM:" form))
         func (ast/form->fn [] form)]
     (func))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Below here is work on decompiling

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Broken instructions
;; < is broken for any number of arguments != 2

;;;; Broken because they're macros
;;  'and `lib/and
;;  'or `lib/or

(def work-without-change
  ['not
   'not=])

(def ast-aliasing
  {'if 'if

   'lt `lib/<'
   'lte `lib/<='
   'gt `lib/>'
   'gte `lib/>='
   'equiv '=

   'max `lib/max'
   'min `lib/min'
   'doubleCast 'double

   'sqrt `lib/safe-sqrt ; doesn't work??
   'sin `lib/sin
   'cos `lib/cos
   'tan `lib/tan
   'asin `lib/safe-asin
   'acos `lib/safe-acos
   'atan `lib/atan
   'log10 `lib/log10
   'ceil `lib/ceil
   'floor `lib/floor
   ;; CBGP has log2 and square, which don't exist in clojure

   'charCast `lib/int->char
   'isWhitespace `lib/whitespace?
   'isDigit `lib/digit?
   'isLetter `lib/letter?})

(def ast-number-aliasing
  {'add "add"
   'minus "sub"
   'multiply "mult"
   'divide "div"
   'quotient "quot"
   'mod "mod"
   'inc "inc"
   'dec "dec"
   ; 'minus "neg" ; minus w/ one arg
   'abs "abs"

   ;;; below methods need `lib/ (eg. `lib/int-pow)
   ; 'pow "pow" 
   ; 'pow "square" ; square is just (Math/pow x 2)
   ; 'ceil "ceil", implemented for doubles
   ; 'floor "floor", implemented for doubles

   ;;; other adhoc polymorphic methods
   ; 'intCast 'int
   ; 'intCast 'char->int
   })

(def ast-str-vec-aliasing
  {'first "first"
   'last "last"
  ;; nth has strange naming conventions, 
  ;; nth-str and nth-or-else.
  ;;'nth
   'empty? "empty"

  ;; Same Namespace issues as in ast-number-aliasing
  ;; Make a new dictionary for namespace qualified (ns-q)
  ;; symbols that are type dependent,  
  ;; because ns-q vector symbols append v
  ;; whereas other non-ns-q append nothing or vec 
  ;; 'rest "`lib/rest"
   })


;; int-pow does not work because tag is a double, 
;; potentially a reason to change find type to check tags
;; instead of the type of val
;; Concat does not work yet because it is supposed to 
;; return a lazySeq
(def ast-namespace-qualified-type-aliased
  {'pow "pow"
  ;;  'ceil "ceil"
  ;;  'floor "floor"
   'rest "rest" 
  ;;  'concat "concat"
   })

(defn get-fn-symbol
  "Finds the CBGP function name for this ast-fn-name"
  [ast-fn-name tag args]
  (cond
    ;;; note: we might need this: (or (= "long" (str tag)) (= java.lang.Long tag))
    (contains? ast-number-aliasing ast-fn-name)
    (symbol (str (if (= (str tag) "double")
                   "double-"
                   "int-")
                 (get ast-number-aliasing ast-fn-name)))
    (contains? ast-str-vec-aliasing ast-fn-name)
    (symbol (str (get ast-str-vec-aliasing ast-fn-name)
                 (if (= (:tag (first args)) java.lang.String)
                   "-str"
                   "")
                 (if (= 'empty? ast-fn-name)
                   "?"
                   "")))
    (contains? ast-namespace-qualified-type-aliased ast-fn-name)
    (symbol "erp12.cbgp-lite.lang.lib" (cond (get ast-namespace-qualified-type-aliased ast-fn-name)
                                             (cond
                                               (= (str tag) "double")
                                               (str "double-" (get ast-namespace-qualified-type-aliased ast-fn-name))
                                               (= (:tag (first args)) java.lang.String)
                                               (str (get ast-namespace-qualified-type-aliased ast-fn-name) "-str")
                                               (= (:tag (first args)) clojure.lang.PersistentVector) 
                                               (str (get ast-namespace-qualified-type-aliased ast-fn-name) "v")
                                               :else (str "int-" (get ast-namespace-qualified-type-aliased ast-fn-name)))))
    (contains? ast-aliasing ast-fn-name)
    (get ast-aliasing ast-fn-name)
    

    :else ast-fn-name))

(defn find-type
  "Returns the type of val in the given ast"
  [val ast]
  (cond
    ;; Ground types
    (integer? val) {:type 'int?}
    (number? val) {:type 'double?}
    (boolean? val) {:type 'boolean?}
    (string? val) {:type 'string?}
    (char? val) {:type 'char?}
    (keyword? val) {:type 'keyword?}
    (nil? val) {:type 'nil?}

    ;; Vectors and sets
    (or (vector? val)
        (set? val))
    (let [child-type (if (empty? val)
                       (lib/s-var 'T)
                       (find-type (first val)
                                  ast))]
      {:type (:type ast) :child child-type})

    ;; Maps
    (map? val)
    (let [key-type (if (empty? val)
                     (lib/s-var 'T)
                     (find-type (first (first val))
                                (ana.jvm/analyze (first (first val)))))
          val-type (if (empty? val)
                     (lib/s-var 'S)
                     (find-type (second (first val))
                                (ana.jvm/analyze (second (first val)))))]
      {:type :map-of :key key-type :value val-type})

              :else (throw (Exception.
                            (str "AST contains a type that shouldn't be possible: "
                                 ast)))))

(defn decompile-ast
  "Decompiles AST into a CBGP genome."
  [{:keys [op val tag args children] :as ast}]
  (cond
    ;; Handle constants
    (= :const op)
    (list {:gene :lit
           :val val
           :type (find-type val ast)})

    ;; Handle static method or invoke
    (or (= op :static-call)
        (= op :invoke))
    (let [ast-fn-name (if (= op :static-call)
                        (:method ast)
                        (-> ast :fn :form))
          raw-decompiled-args (map decompile-ast args)
          decompiled-args (flatten (reverse raw-decompiled-args))]
      (concat decompiled-args
              (list {:gene :var :name (get-fn-symbol ast-fn-name tag args)}
                    {:gene :apply})))

    ;; Handle quote for lists; translate into vector
    (= op :quote)
    (let [the-vector (vec (-> ast :expr :val))]
      (list {:gene :lit
             :val the-vector
             :type (find-type the-vector (assoc ast :type :vector))}))

    ;; Handle if
    ; could be merged w/ the static/invoke handling?
    (= op :if)
    (let [ast-fn-name op
          raw-decompiled-args (map decompile-ast (map ast children))
          decompiled-args (flatten (reverse raw-decompiled-args))]
      (concat decompiled-args
              (list {:gene :var :name (get-fn-symbol ast-fn-name tag args)}
                    {:gene :apply})))

    :else
    (do
      (println "not handled yet AST op:" op)
      nil)))

(comment

  (decompile-ast (ana.jvm/analyze '(- 22 33)))

  (compile-debugging (decompile-ast (ana.jvm/analyze '(+ 22.2 33.3)))
                     {:type 'double?})

  (compile-debugging (decompile-ast (ana.jvm/analyze '(- 22 33)))
                     {:type 'int?})

  (decompile-ast (ana.jvm/analyze '(< 4 5)))

  (decompile-ast (ana.jvm/analyze '(<= 4 5)))

  (compile-debugging (decompile-ast (ana.jvm/analyze '(not true)))
                     {:type 'boolean?})

  (compile-debugging (decompile-ast (ana.jvm/analyze '(< 4 5)))
                     {:type 'boolean?})

  (compile-debugging (decompile-ast (ana.jvm/analyze '(< \a \c)))
                     {:type 'boolean?})

  (compile-debugging (decompile-ast (ana.jvm/analyze '(< "hi" "true")))
                     {:type 'boolean?})

  (decompile-ast (ana.jvm/analyze '(< "hi" "there")))

  (compile-debugging (decompile-ast (ana.jvm/analyze '(not (< 4 5))))
                     {:type 'boolean?})

  (compile-debugging (decompile-ast (ana.jvm/analyze '(* 8 -5)))
                     {:type 'int?})

  (compile-debugging (decompile-ast (ana.jvm/analyze '(quot 80 7)))
                     {:type 'int?})

  (compile-debugging (decompile-ast (ana.jvm/analyze '(/ 6 2)))
                     {:type 'double?})

  (compile-debugging (decompile-ast (ana.jvm/analyze '(/ 80 7)))
                     {:type 'double?})

  (compile-debugging (decompile-ast (ana.jvm/analyze '(/ 80.3 7.1)))
                     {:type 'double?})

  (compile-debugging (decompile-ast (ana.jvm/analyze '(dec 5)))
                     {:type 'int?})

  (compile-debugging (decompile-ast (ana.jvm/analyze '(inc 5.3)))
                     {:type 'double?})

  (compile-debugging (decompile-ast (ana.jvm/analyze '(or true false)))
                     {:type 'boolean?})

  (compile-debugging (decompile-ast (ana.jvm/analyze '(+ 1 2 3 4)))
                     {:type 'int?})

  (compile-debugging (decompile-ast (ana.jvm/analyze '(* 1 2 3 4)))
                     {:type 'int?})

  (decompile-ast (ana.jvm/analyze '(+ 1 2 3 4)))

  (compile-debugging (decompile-ast (ana.jvm/analyze '(< 4 5 8)))
                     {:type 'boolean?})

;;;; Works with empty vectors (and empty maps!)
  (=
   '({:gene :lit, :type {:child {:sym T, :type :s-var}, :type :vector}, :val []})
   (decompile-ast (ana.jvm/analyze [])))

  (decompile-ast (ana.jvm/analyze []))

  (compile-debugging
   (concat
    (decompile-ast (ana.jvm/analyze []))
    ;; (list {:gene :lit, :val [], :type {:type :vector :child (`lib/s-var 'T)}})
    (list {:gene :var :name `lib/conj-vec}
          {:gene :lit :val 5 :type {:type 'int?}}
          {:gene :apply}))
   {:type :vector :child {:type 'int?}}
   true)
  ; test works here, but not in decompile_test
  

  ;; maps
  (=
   (decompile-ast (ana.jvm/analyze {}))
   '({:gene :lit, :type {:key {:sym T, :type :s-var}, :type :map-of, :value {:sym S, :type :s-var}}, :val {}}))

  (compile-debugging
   (concat
    (decompile-ast (ana.jvm/analyze {}))
    (list {:gene :lit :val 5 :type {:type 'int?}}
          {:gene :lit :val "hi" :type {:type 'string?}}
          {:gene :var :name 'assoc}
          {:gene :apply}))
   {:key {:type 'string?}, :type :map-of, :value {:type 'int?}}
   true)

  (compile-debugging
   (concat
    (decompile-ast (ana.jvm/analyze {"apples" 17}))
    (list {:gene :lit :val 5 :type {:type 'int?}}
          {:gene :lit :val "hi" :type {:type 'string?}}
          {:gene :var :name 'assoc}
          {:gene :apply}))
   {:key {:type 'string?}, :type :map-of, :value {:type 'int?}}
   true)

   ;;; Recompiling works with maps!

  (ana.jvm/analyze {1 "asd" 5 "asdfff"})

  (first (first {1 "asd" 5 "asdfff"}))

  (decompile-ast (ana.jvm/analyze {1 "asd" 5 "asdfff"}))

  (=
   (list {:gene :lit, :type {:key {:type 'int?}, :type :map-of, :value {:type 'string?}}, :val {1 "asd", 5 "asdfff"}})
   (decompile-ast (ana.jvm/analyze {1 "asd" 5 "asdfff"})))

  (compile-debugging (decompile-ast (ana.jvm/analyze {1 "asd" 5 "asdfff"}))
                     {:key {:type 'int?}, :type :map-of, :value {:type 'string?}}
                     true)

  (compile-debugging (list {:gene :lit, :type {:key {:type 'int?}, :type :map-of, :val {:type 'string?}}, :val {1 "asd", 5 "asdfff"}}
                           {:gene :lit, :type {:type 'int?}, :val 6})
                     {:type 'int}
                     true)

  (compile-debugging (decompile-ast (ana.jvm/analyze '(max 5 7)))
                     {:type 'int?})

  (compile-debugging (decompile-ast (ana.jvm/analyze '(abs 1000.0)))
                     {:type 'double?})

  ;; NEW STUFF - Sydney
  ; testing abs
  (decompile-ast (ana.jvm/analyze '(abs -10)))
  (compile-debugging (decompile-ast (ana.jvm/analyze '(abs -10))) {:type 'int?})

  (decompile-ast (ana.jvm/analyze '(abs -10.5)))
  (compile-debugging (decompile-ast (ana.jvm/analyze '(abs -10.5))) {:type 'double?})

  ; testing sqrt
  ; (why does this not work tf)
  (compile-debugging (decompile-ast (ana.jvm/analyze '(Math/sqrt 9.0))) {:type 'double?})

  ;; testing if 
  ; !! does not work without the else condition 
  ;
  ;--test case 1 (false, w/ literals)
  (compile-debugging
   '({:gene :lit, :type {:type int?}, :val 2}
     {:gene :lit, :type {:type int?}, :val 5}
     {:gene :lit, :type {:type boolean?}, :val false}
     {:gene :var, :name if}
     {:gene :apply})
   {:type 'int?})
  (decompile-ast (ana.jvm/analyze '(if false 5 2)))
  (compile-debugging (decompile-ast (ana.jvm/analyze '(if false 5 2))) {:type 'int?})
  ; problem: the above compile-decompile test does not compile correctly (defaults to true)
  ;          even though it gives the exact same (manually typed) genome as seen in the 
  ;          compile test (??? why.)

  ;--test case 2 (false, w/ methods) [! currently broken]
  (compile-debugging
   '({:gene :lit, :type {:type int?}, :val 2}
     {:gene :lit, :type {:type int?}, :val 11}
     {:gene :lit, :type {:type int?}, :val 10}
     {:gene :var, :name erp12.cbgp-lite.lang.lib/max'}
     {:gene :apply}
     {:gene :lit, :type {:type int?}, :val 1}
     {:gene :lit, :type {:type int?}, :val 2}
     {:gene :var, :name =} ; this section definitely evaluates correctly (tested on (= 1 2) w/ {:type 'boolean?})
     {:gene :apply}
     {:gene :var, :name :if}
     {:gene :apply})
   {:type 'int?})
  ; problem: always evaluates to the true condition...
  ;          may be a return type issue? i hope not

  (decompile-ast (ana.jvm/analyze '(if (= 0 99) (max 10 11) 2)))
  (compile-debugging (decompile-ast (ana.jvm/analyze '(if (= 1 2) (max 10 11) 12))) {:type 'int?})

  ;--test case 3 (false, w/ method in if, same typing)
  (compile-debugging
   '({:gene :lit, :type {:type boolean?}, :val false}
     {:gene :lit, :type {:type boolean?}, :val true}
     {:gene :lit, :type {:type int?}, :val 1}
     {:gene :lit, :type {:type int?}, :val 0} ; change to 1 to check true
     {:gene :var, :name =}
     {:gene :apply}
     {:gene :var, :name :if}
     {:gene :apply})
   {:type 'boolean?})
  ; okay this DOES work. so it IS a typing issue ahhghhghhh

  (decompile-ast (ana.jvm/analyze '(if (= 0 1) true false)))
  (compile-debugging (decompile-ast (ana.jvm/analyze '(if (= 0 1) true false))) {:type 'boolean?})

;; String and Vector Functions
  (ana.jvm/analyze '(first "Hello"))
  (decompile-ast (ana.jvm/analyze '(rest "Hello")))
  (decompile-ast (ana.jvm/analyze '(rest [1 2 3])))
  (compile-debugging (decompile-ast (ana.jvm/analyze '(rest [1 2 3])))
                     {:child {:type 'int?} :type 'vector?})

  (decompile-ast (ana.jvm/analyze '(empty? [])))
  (compile-debugging (decompile-ast (ana.jvm/analyze '(empty? "")))
                     {:type 'boolean?})
  (decompile-ast (ana.jvm/analyze '(empty? "")))
  (compile-debugging (decompile-ast (ana.jvm/analyze '(empty? [])))
                     {:type 'boolean?})

  (decompile-ast (ana.jvm/analyze '(last [1 2 3])))
  (compile-debugging (decompile-ast (ana.jvm/analyze '(last "String")))
                     {:type 'char?})
  (= (decompile-ast (ana.jvm/analyze '(last [1 2 3])))
     '({:gene :lit, :type {:child {:type int?}, :type :vector}, :val [1 2 3]} {:gene :var, :name last} {:gene :apply}))
  (compile-debugging (decompile-ast (ana.jvm/analyze '(last [1 2 3]))) {:type 'int?})
  (= (compile-debugging (decompile-ast (ana.jvm/analyze '(last [\C \a \t]))) {:type 'char?})
     \t)

  (decompile-ast (ana.jvm/analyze '(nth [1 2 3] 2 5)))
  (decompile-ast (ana.jvm/analyze '(char 60)))
  (ana.jvm/analyze '(print "Hello" "World"))
  (compile-debugging (decompile-ast (ana.jvm/analyze '(print "Hello" "World"))) {:type 'nil?})

  (ana.jvm/analyze '(Math/pow 2 2))
  (decompile-ast (ana.jvm/analyze '(Math/pow 2 3)))
  (ana.jvm/analyze '(Math/pow 2.0 3.0))
  (compile-debugging (decompile-ast (ana.jvm/analyze '(Math/pow 2.0 3.0))) {:type 'double?})
  (decompile-ast (ana.jvm/analyze '(rest [1 2 3])))
  (ana.jvm/analyze '(rest [1 2 3]))
  (compile-debugging (decompile-ast (ana.jvm/analyze '(rest [1 2 3]))) {:child {:type 'int?} :type :vector})
  (compile-debugging (decompile-ast (ana.jvm/analyze '(- 0 1))) {:type 'int?})
  (ana.jvm/analyze '(- 1))

  (decompile-ast (ana.jvm/analyze '(concat [1 2] [3 4])))
  (concat "Hello " "there")
  (compile-debugging (decompile-ast (ana.jvm/analyze '(concat "Hello " "there"))) {:child {:type 'char?} :type '?} true)
  (decompile-ast (ana.jvm/analyze '(zero? 0)))
  (decompile-ast (ana.jvm/analyze #{1 2 3}))

  (compile-debugging (decompile-ast (ana.jvm/analyze {[1 2] #{1 2} [3 4] #{3 4}}))
                     '{:key {:child {:type int?}, :type :vector}, :type :map-of, :value {:child {:type int?}, :type :set}})
  (ana.jvm/analyze {[1 2] #{1 2} {3 4} #{3 4}})

  (= 
   (compile-debugging (decompile-ast (ana.jvm/analyze '(Math/ceil 4.5))) {:type 'double?})
     5.0)
  (= (compile-debugging (decompile-ast (ana.jvm/analyze '(Math/floor 4.5))) {:type 'double?})
     4.0)
  
  (Math/pow 2.5 2.5)
  )
