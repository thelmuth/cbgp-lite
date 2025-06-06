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
  
  )
