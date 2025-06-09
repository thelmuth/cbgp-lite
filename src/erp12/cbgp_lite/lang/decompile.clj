(ns erp12.cbgp-lite.lang.decompile
  (:require [clojure.tools.analyzer.jvm :as ana.jvm]
            [clojure.walk :as walk]
            [erp12.cbgp-lite.lang.ast :as ast]
            [erp12.cbgp-lite.lang.compile :as co]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.search.plushy :as pl]
            [erp12.cbgp-lite.task :as tsk]
            [clojure.tools.analyzer :as ana]))

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

(defn compile-debugging2
  ([genome task]
   (compile-debugging2 genome task []))

  ([genome task args]
   (compile-debugging2 genome task args false))

  ([genome task args verbose]
   (let [enhanced-task (tsk/enhance-task task)
         locals (:arg-symbols enhanced-task)
         _ (when verbose (println "PLUSHY:" genome))
         push (pl/plushy->push genome)
         _ (when verbose (println "PUSH:" push))
         ast (::co/ast (co/push->ast
                        (assoc
                         enhanced-task
                         :locals locals
                         :push push
                         :type-env (merge (:type-env enhanced-task)
                                          lib/type-env))))
         _ (when verbose (println "AST:" ast))
         form (ast/ast->form ast)
         _ (when verbose (println "FORM:" form))
         func (ast/form->fn locals form)]
     (apply func args))))

(comment

  ;;; Test for Count Odds problem
  (let [task {:input->type {'input1 {:type :vector :child {:type 'int?}}}
              :ret-type {:type 'int?}}
        genome (list {:gene :local ;; arg to the function (vec)
                      :idx 0}
                     {:gene :fn    ;; create an anon fn
                      :arg-types [lib/INT]
                      :ret-type lib/BOOLEAN}
                     {:gene :lit   ;; 2 (in anon fn)
                      :val 2
                      :type {:type 'int?}}
                     {:gene :local ;; arg to the anon fn (int)
                      :idx 1}
                     {:gene :var   ;; mod in anon fn
                      :name 'int-mod}
                     {:gene :apply} ;; apply mod
                     {:gene :lit ;;; 1
                      :val 1
                      :type {:type 'int?}}
                     {:gene :var  ;; = (of modded input and 1)
                      :name '=}
                     {:gene :apply} ;; apply =
                     {:gene :close} ;; end anon fn
                     {:gene :var
                      :name 'filterv} ;; call filter on anon fn and vector input
                     {:gene :apply} ;; apply filter
                     {:gene :var
                      :name 'count-vec} ;; count the filtered vector
                     {:gene :apply}) ;; apply count
        ]
    (compile-debugging2 genome
                        task
                        [[8 3 2 5 7 0 11]]
                        true))
  
  ;;; Test for  Smallest problem 
  (let [task {:input->type {'input1 {:type 'int?}
                            'input2 {:type 'int?}
                            'input3 {:type 'int?}
                            'input4 {:type 'int?}}
              :ret-type {:type 'int?}}
        genome [{:gene :local
                 :idx 0}
                {:gene :local
                 :idx 1}
                {:gene :var
                 :name `lib/min'}
                {:gene :apply}
                {:gene :local
                 :idx 2}
                {:gene :var
                 :name `lib/min'}
                {:gene :apply}
                {:gene :local
                 :idx 3}
                {:gene :var
                 :name `lib/min'}
                {:gene :apply}]
        ]
    (compile-debugging2 genome
                        task
                        [5 6 -33 9]
                        true))
  
  ;; Test for Number IO
  (let [task {:input->type {'input1 {:type 'double?}
                            'input2 {:type 'int?}}
              :ret-type {:type 'string?}}
        genome (list {:gene :local
                      :idx 1}
                     {:gene :var
                      :name 'double}
                     {:gene :apply}
                     {:gene :local
                      :idx 0}
                     {:gene :var
                      :name 'double-add}
                     {:gene :apply}
                     {:gene :var
                      :name 'str}
                     {:gene :apply})]
    (compile-debugging2 genome
                        task
                        [100.23 33]
                        true)) 
  )


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
   `lib/int-pow
   'zero-double?])

(def work-without-change
  ['not
   'not=
   'if
   'print
   'println
   'assoc
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
   'log10 `lib/safe-log10
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
   'nth 'nth
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
           2 'sub
           :default 'sub}
   ; Does not work on strings
   'nth {2 `lib/safe-nth
         3 'nth-or-else}
   
  ;;  'reduce {2 'reduce
  ;;           3 'fold}
   })

(def ast-namespace-qualified-type-aliasing
  {'rest "rest"
  ;;  'concat "concat"
   })

(defn find-local
  "Takes a map or vec and recursively looks through it to find a map
   with a key of :op and value of :local"
  [map-or-vec]
  ;; (println (:tag map-or-vec))
  (cond
    (and (map? map-or-vec)
         (= (:op map-or-vec) :local))
    (do 
      ;; (println "local found! type whatever: " (:tag map-or-vec))
      map-or-vec)

    (map? map-or-vec)
    (first (filter #(not (nil? %))
                   (map find-local
                        (vals map-or-vec))))

    (vector? map-or-vec)
    (first (filter #(not (nil? %))
                   (map find-local
                        map-or-vec)))

    :else
    nil))

(defn get-fn-symbol
  "Finds the CBGP function name for this ast-fn-name"
  [ast-fn-name tag args task]
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
        (get-fn-symbol fn-symbol tag args task)
        fn-symbol))

    ;; numbers
    (contains? ast-number-aliasing ast-fn-name)
    (symbol (str (cond
                   (= (str tag) "double") "double-"
                   (= (str tag) "long") "int-"
                   (= tag java.lang.Long) "int-"
                   (= tag java.lang.Integer) "int-"
                   (= ast-fn-name 'div) "int-"

                   ;; Handle local making it so we don't know the type
                   ;; Note; defaults to int? even when the type isn't
                   ;; double? or int? This may occasionally break things
                   :else
                   (if (= 'double?
                          (:type (get (:input->type task)
                                      (:form (find-local args)))))
                     "double-"
                     "int-"))
                 (get ast-number-aliasing ast-fn-name)))

    ;; Vector stuff
    (contains? ast-str-vec-aliasing ast-fn-name)
    (symbol (str (get ast-str-vec-aliasing ast-fn-name)
                 (cond 
                   (= (:tag (first args)) java.lang.String) "-str"
                   (= (:tag (first args)) clojure.lang.APersistentVector) ""
                   :else 
                   (if (= 'string?
                          (:type (get (:input->type task)
                                      (:form (find-local args)))))
                     "-str"
                     "")
                   )
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
  ([ast] (decompile-ast ast {}))
  ([{:keys [op val tag args children] :as ast} task]
   (cond
    ;; Handle constants
     (= :const op)
     (list {:gene :lit
            :val val
            :type (find-type val ast)})

     ;; Handle locals
     (= :local op)
     (list {:gene :local 
            :idx (:arg-id ast)})

    ;; Handle static method or invoke
     (or (= op :static-call)
         (= op :invoke))
     (let [ast-fn-name (if (= op :static-call)
                         (:method ast)
                         (-> ast :fn :form))
           raw-decompiled-args (map decompile-ast args)
           decompiled-args (flatten (reverse raw-decompiled-args))]
       (concat decompiled-args
               (list {:gene :var :name (get-fn-symbol ast-fn-name tag args task)}
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
               (list {:gene :var :name (get-fn-symbol ast-fn-name tag args task)}
                     {:gene :apply})))

    ;; Handle anonymous function abstraction
     (= op :fn)
     nil

     (= op :def)
     (decompile-ast (-> ast
                        :init
                        :expr
                        :methods
                        first
                        :body)
                    task)

     :else
     (do
       (println "not handled yet AST op:" op)
       nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing

(comment

  (compile-debugging (decompile-ast (ana.jvm/analyze '(nth [1.0 2.0 3.0] 10 4.04)))
                     {:type 'double?})
;;;; THESE DON'T WORK

  (decompile-ast (ana.jvm/analyze '(nth [1 2 3] 2 5)))

  (compile-debugging (decompile-ast (ana.jvm/analyze '(or true false)))
                     {:type 'boolean?})

  (compile-debugging (decompile-ast (ana.jvm/analyze '(< 4 5 8)))
                     {:type 'boolean?})

  ;;; misc stuff

  (ana.jvm/analyze '(fn [x] (+ x 1)))

  (map (fn [x] (+ x 1))
       '(3 5 1))

  (decompile-ast
   (->
    (ana.jvm/analyze '(defn help [input1] (inc input1)))
    :init
    :expr
    :methods
    first
    :body))

  (->
   (ana.jvm/analyze '(defn help [input1 input2 input3] (+ input3 input2)))
   :init
   :expr
   :methods
   first
   :body)

  (macroexpand-1 '(defn help [input1] (inc input1)))

  (decompile-ast
   (ana.jvm/analyze '(defn help [input1] (inc input1)))
   {:input->type {'input1 {:type 'int?}}
    :ret-type {:type 'int?}})

  (decompile-ast
   (ana.jvm/analyze '(defn help [input1 input2] (+ input1 input2)))
   {:input->type {'input1 {:type 'int?}
                  'input2 {:type 'int?}}
    :ret-type {:type 'int?}})

  ;;; strangely broken - because it needs to call find-local since it doesn't know the type of input2
  ;;; but it finds input1 first
  (decompile-ast
   (ana.jvm/analyze '(defn help [input1 input2] (+ (int input1) input2)))
   {:input->type {'input1 {:type 'double?}
                  'input2 {:type 'int?}}
    :ret-type {:type 'int?}})

  ;; this works
  (decompile-ast
   (ana.jvm/analyze '(defn help [input1 input2] (+ (int input1) (int input2))))
   {:input->type {'input1 {:type 'double?}
                  'input2 {:type 'double?}}
    :ret-type {:type 'int?}})
  
  (decompile-ast
   (ana.jvm/analyze '(defn help [input1 input2] (+ (count input1) input2)))
   {:input->type {'input1 {:type 'string?}
                  'input2 {:type 'int?}}
    :ret-type {:type 'int?}})

  (compile-debugging2 (decompile-ast
                       (ana.jvm/analyze '(defn help [input1 input2] (+ input1 input2)))
                       {:input->type {'input1 {:type 'double?}
                                      'input2 {:type 'double?}}
                        :ret-type {:type 'double?}})
                      {:input->type {'input1 {:type 'double?}
                                     'input2 {:type 'double?}}
                       :ret-type {:type 'double?}}
                      [5.2 10.0])

  '(+ (+ input1 input2) (- input1 input2))

  (decompile-ast (ana.jvm/analyze '(+ (+ 2.2 3.3) (+ 4.4 5.5))))

  (ana.jvm/analyze '(+ (+ 2.2 3.3) (+ 4.4 5.5)))

  (ana.jvm/analyze '(defn my-first [x] (first x)))

  (ana.jvm/analyze '(defn my-first [x] (+ (/ x 2) 5.5)))

  (ana.jvm/analyze '(defn what [input1zzz] (map (fn [xzzz] (inc xzzz))
                                                input1zzz)))

  '{:children [:meta :init],
    :meta
    {:op :const,
     :env
     {:context :ctx/expr,
      :locals {},
      :ns erp12.cbgp-lite.lang.decompile,
      :column 21,
      :line 450,
      :file "/Users/thelmuth/Documents/Clojure/cbgp-lite/src/erp12/cbgp_lite/lang/decompile.clj"},
     :form
     {:arglists '([input1]),
      :column 21,
      :line 450,
      :file "/Users/thelmuth/Documents/Clojure/cbgp-lite/src/erp12/cbgp_lite/lang/decompile.clj"},
     :val
     {:arglists '([input1]),
      :column 21,
      :line 450,
      :file "/Users/thelmuth/Documents/Clojure/cbgp-lite/src/erp12/cbgp_lite/lang/decompile.clj"},
     :type :map,
     :literal? true,
     :o-tag clojure.lang.PersistentArrayMap,
     :tag clojure.lang.PersistentArrayMap},
    :return-tag java.lang.Number,
    :init
    {:children [:meta :expr],
     :meta
     {:op :const,
      :env
      {:context :ctx/expr,
       :locals {},
       :ns erp12.cbgp-lite.lang.decompile,
       :file "/Users/thelmuth/Documents/Clojure/cbgp-lite/src/erp12/cbgp_lite/lang/decompile.clj",
       :column 21,
       :line 450},
      :form {:rettag nil},
      :val {:rettag nil},
      :type :map,
      :literal? true,
      :o-tag clojure.lang.PersistentArrayMap,
      :tag clojure.lang.PersistentArrayMap},
     :return-tag java.lang.Number,
     :op :with-meta,
     :env
     {:context :ctx/expr,
      :locals {},
      :ns erp12.cbgp-lite.lang.decompile,
      :column 21,
      :line 450,
      :file "/Users/thelmuth/Documents/Clojure/cbgp-lite/src/erp12/cbgp_lite/lang/decompile.clj"},
     :o-tag java.lang.Object,
     :expr
     {:children [:methods],
      :return-tag java.lang.Number,
      :op :fn,
      :env
      {:context :ctx/expr,
       :locals {},
       :ns erp12.cbgp-lite.lang.decompile,
       :file "/Users/thelmuth/Documents/Clojure/cbgp-lite/src/erp12/cbgp_lite/lang/decompile.clj",
       :column 21,
       :line 450},
      :o-tag clojure.lang.AFunction,
      :variadic? false,
      :methods
      [{:children [:params :body],
        :loop-id loop_19137,
        :arglist [input1],
        :params
        [{:name input1__#0,
          :op :binding,
          :env
          {:context :ctx/expr,
           :locals {},
           :ns erp12.cbgp-lite.lang.decompile,
           :once false,
           :file "/Users/thelmuth/Documents/Clojure/cbgp-lite/src/erp12/cbgp_lite/lang/decompile.clj",
           :column 21,
           :line 450},
          :o-tag java.lang.Object,
          :variadic? false,
          :arg-id 0,
          :form input1,
          :tag java.lang.Object,
          :atom :FAIL ; #<Atom@35523300: {:tag java.lang.Object}>,
          :local :arg}],
        :fixed-arity 1,
        :op :fn-method,
        :env
        {:context :ctx/expr,
         :locals {},
         :ns erp12.cbgp-lite.lang.decompile,
         :once false,
         :file "/Users/thelmuth/Documents/Clojure/cbgp-lite/src/erp12/cbgp_lite/lang/decompile.clj",
         :column 21,
         :line 450},
        :o-tag java.lang.Object,
        :variadic? false,
        :form ([input1] (inc input1)),
        :tag java.lang.Object,
        :body
        {:args
         [{:children [],
           :name input1__#0,
           :op :local,
           :env
           {:loop-locals 1,
            :locals {input1 {:form input1, :name input1, :variadic? false, :op :binding, :arg-id 0, :local :arg}},
            :ns erp12.cbgp-lite.lang.decompile,
            :loop-id loop_19137,
            :file "/Users/thelmuth/Documents/Clojure/cbgp-lite/src/erp12/cbgp_lite/lang/decompile.clj",
            :column 41,
            :line 450,
            :once false,
            :context :ctx/expr},
           :o-tag java.lang.Object,
           :variadic? false,
           :arg-id 0,
           :form input1,
           :tag java.lang.Object,
           :atom :FAIL ; #<Atom@35523300: {:tag java.lang.Object}>,
           :local :arg,
           :assignable? false}],
         :children [:args],
         :body? true,
         :method inc,
         :op :static-call,
         :env
         {:loop-locals 1,
          :locals {input1 {:form input1, :name input1, :variadic? false, :op :binding, :arg-id 0, :local :arg}},
          :ns erp12.cbgp-lite.lang.decompile,
          :loop-id loop_19137,
          :file "/Users/thelmuth/Documents/Clojure/cbgp-lite/src/erp12/cbgp_lite/lang/decompile.clj",
          :column 41,
          :line 450,
          :once false,
          :context :ctx/return},
         :o-tag java.lang.Number,
         :class clojure.lang.Numbers,
         :form (. clojure.lang.Numbers (inc input1)),
         :tag java.lang.Number,
         :validated? true,
         :raw-forms ((do (inc input1)) (inc input1))}}],
      :once false,
      :max-fixed-arity 1,
      :form (fn* ([input1] (inc input1))),
      :tag clojure.lang.AFunction,
      :arglists ([input1])},
     :form (fn* ([input1] (inc input1))),
     :tag clojure.lang.AFunction,
     :arglists ([input1]),
     :raw-forms ((clojure.core/fn ([input1] (inc input1))))},
    :name help,
    :op :def,
    :env
    {:context :ctx/expr,
     :locals {},
     :ns erp12.cbgp-lite.lang.decompile,
     :column 21,
     :line 450,
     :file "/Users/thelmuth/Documents/Clojure/cbgp-lite/src/erp12/cbgp_lite/lang/decompile.clj"},
    :o-tag clojure.lang.Var,
    :var #'erp12.cbgp-lite.lang.decompile/help,
    :top-level true,
    :form (def help (clojure.core/fn ([input1] (inc input1)))),
    :tag clojure.lang.Var,
    :arglists ([input1]),
    :raw-forms ((defn help [input1] (inc input1)))}

  ;; :int :expr
  '{:children [:methods],
    :return-tag java.lang.Number,
    :op :fn,
    :env
    {:context :ctx/expr,
     :locals {},
     :ns erp12.cbgp-lite.lang.decompile,
     :file "/Users/thelmuth/Documents/Clojure/cbgp-lite/src/erp12/cbgp_lite/lang/decompile.clj",
     :column 22,
     :line 455},
    :o-tag clojure.lang.AFunction,
    :variadic? false,
    :methods
    [{:children [:params :body],
      :loop-id loop_19896,
      :arglist [input1],
      :params
      [{:name input1__#0,
        :op :binding,
        :env
        {:context :ctx/expr,
         :locals {},
         :ns erp12.cbgp-lite.lang.decompile,
         :once false,
         :file "/Users/thelmuth/Documents/Clojure/cbgp-lite/src/erp12/cbgp_lite/lang/decompile.clj",
         :column 22,
         :line 455},
        :o-tag java.lang.Object,
        :variadic? false,
        :arg-id 0,
        :form input1,
        :tag java.lang.Object,
        :atom :FAIL ; #<Atom@f7f4d65: {:tag java.lang.Object}>,
        :local :arg}],
      :fixed-arity 1,
      :op :fn-method,
      :env
      {:context :ctx/expr,
       :locals {},
       :ns erp12.cbgp-lite.lang.decompile,
       :once false,
       :file "/Users/thelmuth/Documents/Clojure/cbgp-lite/src/erp12/cbgp_lite/lang/decompile.clj",
       :column 22,
       :line 455},
      :o-tag java.lang.Object,
      :variadic? false,
      :form ([input1] (inc input1)),
      :tag java.lang.Object,
      :body
      {:args
       [{:children [],
         :name input1__#0,
         :op :local,
         :env
         {:loop-locals 1,
          :locals {input1 {:form input1, :name input1, :variadic? false, :op :binding, :arg-id 0, :local :arg}},
          :ns erp12.cbgp-lite.lang.decompile,
          :loop-id loop_19896,
          :file "/Users/thelmuth/Documents/Clojure/cbgp-lite/src/erp12/cbgp_lite/lang/decompile.clj",
          :column 42,
          :line 455,
          :once false,
          :context :ctx/expr},
         :o-tag java.lang.Object,
         :variadic? false,
         :arg-id 0,
         :form input1,
         :tag java.lang.Object,
         :atom :FAIL ;#<Atom@f7f4d65: {:tag java.lang.Object}>,
         :local :arg,
         :assignable? false}],
       :children [:args],
       :body? true,
       :method inc,
       :op :static-call,
       :env
       {:loop-locals 1,
        :locals {input1 {:form input1, :name input1, :variadic? false, :op :binding, :arg-id 0, :local :arg}},
        :ns erp12.cbgp-lite.lang.decompile,
        :loop-id loop_19896,
        :file "/Users/thelmuth/Documents/Clojure/cbgp-lite/src/erp12/cbgp_lite/lang/decompile.clj",
        :column 42,
        :line 455,
        :once false,
        :context :ctx/return},
       :o-tag java.lang.Number,
       :class clojure.lang.Numbers,
       :form (. clojure.lang.Numbers (inc input1)),
       :tag java.lang.Number,
       :validated? true,
       :raw-forms ((do (inc input1)) (inc input1))}}],
    :once false,
    :max-fixed-arity 1,
    :form (fn* ([input1] (inc input1))),
    :tag clojure.lang.AFunction,
    :arglists ([input1])}

  ;; :init :expr :methods first
  '{:children [:params :body],
    :loop-id loop_19913,
    :arglist [input1],
    :params
    [{:name input1__#0,
      :op :binding,
      :env
      {:context :ctx/expr,
       :locals {},
       :ns erp12.cbgp-lite.lang.decompile,
       :once false,
       :file "/Users/thelmuth/Documents/Clojure/cbgp-lite/src/erp12/cbgp_lite/lang/decompile.clj",
       :column 22,
       :line 455},
      :o-tag java.lang.Object,
      :variadic? false,
      :arg-id 0,
      :form input1,
      :tag java.lang.Object,
      :atom :f ;#<Atom@719d22ab: {:tag java.lang.Object}>,
      :local :arg}],
    :fixed-arity 1,
    :op :fn-method,
    :env
    {:context :ctx/expr,
     :locals {},
     :ns erp12.cbgp-lite.lang.decompile,
     :once false,
     :file "/Users/thelmuth/Documents/Clojure/cbgp-lite/src/erp12/cbgp_lite/lang/decompile.clj",
     :column 22,
     :line 455},
    :o-tag java.lang.Object,
    :variadic? false,
    :form ([input1] (inc input1)),
    :tag java.lang.Object,
    :body
    {:args
     [{:children [],
       :name input1__#0,
       :op :local,
       :env
       {:loop-locals 1,
        :locals {input1 {:form input1, :name input1, :variadic? false, :op :binding, :arg-id 0, :local :arg}},
        :ns erp12.cbgp-lite.lang.decompile,
        :loop-id loop_19913,
        :file "/Users/thelmuth/Documents/Clojure/cbgp-lite/src/erp12/cbgp_lite/lang/decompile.clj",
        :column 42,
        :line 455,
        :once false,
        :context :ctx/expr},
       :o-tag java.lang.Object,
       :variadic? false,
       :arg-id 0,
       :form input1,
       :tag java.lang.Object,
       :atom :f ; #<Atom@719d22ab: {:tag java.lang.Object}>,
       :local :arg,
       :assignable? false}],
     :children [:args],
     :body? true,
     :method inc,
     :op :static-call,
     :env
     {:loop-locals 1,
      :locals {input1 {:form input1, :name input1, :variadic? false, :op :binding, :arg-id 0, :local :arg}},
      :ns erp12.cbgp-lite.lang.decompile,
      :loop-id loop_19913,
      :file "/Users/thelmuth/Documents/Clojure/cbgp-lite/src/erp12/cbgp_lite/lang/decompile.clj",
      :column 42,
      :line 455,
      :once false,
      :context :ctx/return},
     :o-tag java.lang.Number,
     :class clojure.lang.Numbers,
     :form (. clojure.lang.Numbers (inc input1)),
     :tag java.lang.Number,
     :validated? true,
     :raw-forms ((do (inc input1)) (inc input1))}}

  (compile-debugging (decompile-ast (ana.jvm/analyze '(nth [1 2 3 4] 6 10)))
                     {:type 'int?})

  (->
   (ana.jvm/analyze '(defn help [input1] (inc input1)))
   :init
   :expr
   :methods)
  (->
   (ana.jvm/analyze '(defn help [input1] (inc (+ 1 input1))))
   :init
   :expr
   :methods
   first
   :body
   :args)

  (defn example [x] (+ (/ x 2) 5.0))
  (example 3)
  
  


  (ana.jvm/analyze '(defn help [input1 input2]
    (+ (int input1) input2)))
  
  (-> (ana.jvm/analyze '(defn help [input1 input2]
                      (+ (int input1) input2)))
      :init
      :expr
      :methods
      first
      :body
      :args
      first)
  

  (compile-debugging2 (decompile-ast (ana.jvm/analyze '(defn my-first [input1] (first input1)))
                 {:input->type {'input1 {:type 'string?}}
                  :ret-type {:type 'char?}})
                      {:input->type {'input1 {:type 'string?}}
                       :ret-type {:type 'char?}}
                      ["Hello"])
  

  (compile-debugging2 
   (decompile-ast (ana.jvm/analyze '(defn my-first [input1] (first input1)))
                 {:input->type {'input1 {:type :vector :child {:type 'int?}}}
                  :ret-type {:type 'int?}}) 
                      {:input->type {'input1 {:type :vector :child {:type 'int?}}}
                       :ret-type {:type 'int?}} 
                      [[5 4 3 6 81]])
  )

