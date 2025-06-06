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
;; / with more than 2 arguments

;;;; Broken because they're macros
;;  'and `lib/and
;;  'or `lib/or

(def fns-llm-won't-use
  [`lib/int-ceil
   `lib/int-floor
   `lib/safe-log2
   `lib/double-square
   `lib/int-square
   `lib/int-pow])

(def work-without-change
  ['not
   'not=
   'if
   'merge])

(def ast-aliasing
  {'lt `lib/<'
   'lte `lib/<='
   'gt `lib/>'
   'gte `lib/>='
   'equiv '=

   'max `lib/max'
   'min `lib/min'
   'doubleCast 'double

   'isZero 'zero-int?
   'sqrt `lib/safe-sqrt
   'pow `lib/double-pow
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
   'isLetter `lib/letter?

   'concat `lib/concatv
   'vals `lib/vals-vec
   ;; There is a keys-set, which returns as a set
   ;; but I don't think Clojure has this functionality
   'keys `lib/keys-vec})

(def ast-number-aliasing
  {'add "add"
   'sub "sub"
   'multiply "mult"
   'divide "div"
   'quotient "quot"
   'mod "mod"
   'inc "inc"
   'dec "dec"
   'neg "neg" ; minus w/ one arg
   'abs "abs"

   ;;; below methods need `lib/ (eg. `lib/int-pow)
   ; 'pow "pow" 
   ; 'pow "square" ; square is just (Math/pow x 2)
   ; 'ceil "ceil", implemented for doubles
   ; 'floor "floor", implemented for doubles

   ;;; other adhoc polymorphic methods
   'intCast 'int
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

(def ast-collection-aliasing
  {'count "count"
  ;;  'reduce "reduce"
  ;;  'fold "fold"
   })

(def ast-arity-aliasing
  {'str {1 'str
         2 `lib/concat-str
         :default `lib/concat-str}
   'minus {1 'neg
           2 'sub}
  ;;  'reduce {2 'reduce
  ;;           3 'fold}
   })

;; Concat does not work yet because it is supposed to 
;; return a lazySeq
(def ast-namespace-qualified-type-aliasing
  {'rest "rest"
  ;;  'concat "concat"
   })

(defn get-fn-symbol
  "Finds the CBGP function name for this ast-fn-name"
  [ast-fn-name tag args]
  (cond
    ;; Because of the phrasing, this needs to be hard coded
    (= ast-fn-name 'intCast)
    (if (= (str (:tag (first args))) "char")
      'char->int
      'int)
    ;; functions with multiple arities to support
    (contains? ast-arity-aliasing ast-fn-name)
    (let [arity-map (get ast-arity-aliasing ast-fn-name)
          fn-symbol (get arity-map
                         (count args)
                         (get arity-map :default))]
      (if (contains? ast-number-aliasing fn-symbol)
        (get-fn-symbol fn-symbol tag args)
        fn-symbol))

    ;; numbers
    (contains? ast-number-aliasing ast-fn-name)
    (symbol (str (if (= (str tag) "double")
                   "double-" 
                   "int-")
                 (get ast-number-aliasing ast-fn-name)))

    ;; Vector stuff
    (contains? ast-str-vec-aliasing ast-fn-name)
    (symbol (str (get ast-str-vec-aliasing ast-fn-name)
                 (if (= (:tag (first args)) java.lang.String)
                   "-str"
                   "")
                 (if (= 'empty? ast-fn-name)
                   "?"
                   "")))
    
    ;;Vector-set-map
    (contains? ast-collection-aliasing ast-fn-name)
    (symbol (str (get ast-collection-aliasing ast-fn-name)
                 (cond 
                   (vector? (:val (first args)))
                   "-vec"
                   (set? (:val (first args)))
                   "-set"
                   (map? (:val (first args)))
                   "-map")))
    ;; rest only right now?
    (contains? ast-namespace-qualified-type-aliasing ast-fn-name)
    (symbol "erp12.cbgp-lite.lang.lib" (cond (get ast-namespace-qualified-type-aliasing ast-fn-name)
                                             (cond
                                               (= (str tag) "double")
                                               (str "double-" (get ast-namespace-qualified-type-aliasing ast-fn-name))

                                               (= (:tag (first args)) java.lang.String)
                                               (str (get ast-namespace-qualified-type-aliasing ast-fn-name) "-str")

                                               (= (:tag (first args)) clojure.lang.PersistentVector)
                                               (str (get ast-namespace-qualified-type-aliasing ast-fn-name) "v")

                                               :else (str "int-" (get ast-namespace-qualified-type-aliasing ast-fn-name)))))

;; main aliasing
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
    (symbol? val) {:type 'symbol?}
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
    (= op :if)
    (let [ast-fn-name 'if
          raw-decompiled-args (map decompile-ast (map ast children))
          decompiled-args (flatten (reverse raw-decompiled-args))]
      (concat decompiled-args
              (list {:gene :var :name (get-fn-symbol ast-fn-name tag args)}
                    {:gene :apply})))

    :else
    (do
      (println "not handled yet AST op:" op)
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing

(comment
;;;; Works with empty vectors (and empty maps!)
  (=
   '({:gene :lit, :type {:child {:sym T, :type :s-var}, :type :vector}, :val []})
   (decompile-ast (ana.jvm/analyze [])))

  (decompile-ast (ana.jvm/analyze []))

  (compile-debugging
   (concat
    (decompile-ast (ana.jvm/analyze []))
    ;; (list {:gene :lit, :val [], :type {:type :vector :child (lib/s-var 'T)}})
    (list {:gene :var :name `lib/conj-vec}
          {:gene :lit :val 5 :type {:type 'int?}}
          {:gene :apply}))
   {:type :vector :child {:type 'int?}}
   true)

  ;; maps

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

  (compile-debugging (decompile-ast (ana.jvm/analyze '(abs 1000.0)))
                     {:type 'double?})

;;;; ADD THESE AS TESTS concat
  (decompile-ast (ana.jvm/analyze '(concat [1 2] [3 4])))

  (compile-debugging
   (decompile-ast (ana.jvm/analyze '(concat [1 2] [3 4])))
   {:child {:type 'int?} :type :vector})

  (decompile-ast (ana.jvm/analyze '(concat (rest [1 2 3]) [3 4])))

  (compile-debugging
   (decompile-ast (ana.jvm/analyze '(concat (rest [1 2 3]) [3 4])))
   {:child {:type 'int?} :type :vector})

  ;;; ADD TESTS for str
  (decompile-ast (ana.jvm/analyze '(str "hello" "world")))

  (decompile-ast (ana.jvm/analyze '(str "hello" "world" "yay")))

  (decompile-ast (ana.jvm/analyze '(str (+ 2 3))))

  (ana.jvm/analyze '(str "hello" "world"))

;; minus ADD TESTS
  (decompile-ast (ana.jvm/analyze '(- 4 5)))

  (decompile-ast (ana.jvm/analyze '(- 4)))

  (decompile-ast (ana.jvm/analyze '(- 4.4)))

  (decompile-ast (ana.jvm/analyze '(- 4.4 93.0)))

  (decompile-ast (ana.jvm/analyze '(- 4 5 2)))

  (decompile-ast (ana.jvm/analyze '(- 4 5 (- 2))))

;;;; THESE DON'T WORK

  (decompile-ast (ana.jvm/analyze '(nth [1 2 3] 2 5)))

  (compile-debugging (decompile-ast (ana.jvm/analyze '(or true false)))
                     {:type 'boolean?})

  (compile-debugging (decompile-ast (ana.jvm/analyze '(< 4 5 8)))
                     {:type 'boolean?})

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

  (compile-debugging (decompile-ast (ana.jvm/analyze '(if true 5 2)))
                     {:type 'int?})

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

  (decompile-ast (ana.jvm/analyze '(if (= 0 1) false true)))

  (compile-debugging (decompile-ast (ana.jvm/analyze '(if (= 0 1) false true)))
                     {:type 'boolean?})

  (ana.jvm/analyze '(fn [x] (+ x 5)))

  (ana.jvm/analyze (read-string "(defn count-vowels [s]
     (count (filter #(contains? #{\\a \\e \\i \\o \\u} (Character/toLowerCase %)) s)))"))

  
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   (compile-debugging
;;    (list {:gene :local :idx 0}
;;         {:gene :fn :arg-types [lib/INT] :ret-type lib/BOOLEAN}
;;         {:gene :lit :val 2 :type {:type 'int?}}
;;         {:gene :local :idx 1}
;;         {:gene :var :name 'int-mod}
;;         {:gene :apply}
;;         {:gene :lit :val 1 :type {:type 'int?}}
;;         {:gene :var :name '=}
;;         {:gene :apply}
;;         {:gene :close}
;;         {:gene :var :name 'filterv}
;;         {:gene :apply}
;;         {:gene :var :name 'count-vec}
;;         {:gene :apply})
;;    {:type 'boolean?})

;;   (defn count-odds [x]
;;     ((count (filter (fn [y] (mod y 2)) x))))
  
  (compile-debugging (decompile-ast (ana.jvm/analyze '(keys {1 #{1 2} 2 #{3 4}})))
                     {:type :vector :child {:type 'int?}})
  
  (decompile-ast (ana.jvm/analyze '(str "Hi" "hey")))
  (compile-debugging (decompile-ast (ana.jvm/analyze '(merge {:a 1} {:b 2})))
                     {:type :map-of :key {:type 'keyword?}
                      :value {:type 'int?}})
  
  (decompile-ast (ana.jvm/analyze '(merge {[\a \a \a] 1} {[\b \b \b] 2}))) 
  
  
  
  )




