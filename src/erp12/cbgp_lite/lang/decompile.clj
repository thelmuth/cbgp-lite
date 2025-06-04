(ns erp12.cbgp-lite.lang.decompile
  (:require [erp12.cbgp-lite.search.plushy :as pl]
            [erp12.cbgp-lite.lang.ast :as ast]
            [erp12.cbgp-lite.lang.compile :as co]
            [erp12.cbgp-lite.lang.lib :as lib]
            [clojure.tools.analyzer.jvm :as ana.jvm]))

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
  {'lt `lib/<'
   'lte `lib/<='
   'gt `lib/>'
   'gte `lib/>='
   'equiv '=

   'max `lib/max'
   'min `lib/min'
   'doubleCast 'double

   'sin `lib/sin
   'cos `lib/cos
   'tan `lib/tan

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
   'dec "dec"})

(defn get-fn-symbol
  "Finds the CBGP function name for this ast-fn-name"
  [ast-fn-name tag]
  (cond

    ;;; note: we might need this: (or (= "long" (str tag)) (= java.lang.Long tag))
    (contains? ast-number-aliasing ast-fn-name)
    (symbol (str (if (= (str tag) "double")
                   "double-"
                   "int-")
                 (get ast-number-aliasing ast-fn-name)))

    (contains? ast-aliasing ast-fn-name)
    (get ast-aliasing ast-fn-name)

    :else ast-fn-name))

(defn decompile-ast
  "Decompiles AST into a CBGP genome."
  [{:keys [op val tag method args] :as ast}]
  (cond
    ;; Handle constants
    (= :const op)
    (list (assoc {} :gene :lit
                 :val val
                 :type {:type (cond
                                (integer? val) 'int?
                                (number? val) 'double?
                                (boolean? val) 'boolean?
                                (string? val) 'string?
                                (char? val) 'char?
                                (nil? val) 'nil?
                                :else (throw (Exception.
                                              (str "AST contains a type that shouldn't be possible: "
                                                   ast))))}))
    ;; Handle static method or invoke
    (or (= op :static-call)
        (= op :invoke))
    (let [ast-fn-name (if (= op :static-call)
                        (:method ast)
                        (-> ast :fn :form))
          raw-decompiled-args (map decompile-ast args)
          decompiled-args (flatten (reverse raw-decompiled-args))]
      (concat decompiled-args
              (list {:gene :var :name (get-fn-symbol ast-fn-name tag)}
                    {:gene :apply})))

    :else
    (do
      (println "not handled yet AST op:" op)
      nil)))

(comment

  (decompile-ast (ana.jvm/analyze [1 2]))

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


  ;; vectors and sets etc

  )


