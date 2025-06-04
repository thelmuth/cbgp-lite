(ns erp12.cbgp-lite.lang.decompile
  (:require [erp12.cbgp-lite.search.pluhsy :as pl]
            [erp12.cbgp-lite.lang.ast :as ast]
            [erp12.cbgp-lite.lang.compile :as co]
            [erp12.cbgp-lite.lang.lib :as lib]
            [clojure.tools.analyzer.jvm :as ana.jvm]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; Compilation testing

(defn compile-debugging
  [genome ret-type]
  (let [_ (println "PLUSHY:" genome)
        push (pl/plushy->push genome)
        _ (println "PUSH:" push)
        ast (::co/ast (co/push->ast {:push push
                                     :ret-type ret-type
                                     :type-env lib/type-env}))
        _ (println "AST:" ast)
        form (ast/ast->form ast)
        _ (println "FORM:" form)
        func (ast/form->fn [] form)]
    (func)))

(comment

  ;;; I don't think we'll need this, but might as well keep it around in case
  (let [genes [{:gene :var :name 'int-add}
               {:gene :var :name 'int-sub}
               {:gene :var :name 'int-times}
               {:gene :var :name 'int-div}
               {:gene :lit :val 1}
               {:gene :lit :val 2}
               {:gene :lit :val 10}
               {:gene :lit-generator :fn #(rand-int 100)}
               {:gene :local}
               {:gene :apply}
               {:gene :close}
               {:gene :let}
               {:gene :fn}]
        gene+prob (pl/prob-by-gene-kind genes
                                        {:close         0.1
                                         :local         0.1
                                         :var           0.1
                                         :lit           0.1
                                         :lit-generator 0.1
                                         :fn            0.2
                                         :let           0.2
                                         :apply         0.1})
        source (pl/make-genetic-source gene+prob)]
    (pl/random-plushy-genome {:min-genome-size 10 :max-genome-size 20 :genetic-source source}))

  ;; This is a genome, so we can see what one from random generation looks like
  (let [genome '({:gene :var, :name erp12.cbgp-lite.lang.lib/whitespace?} {:gene :lit, :val 2, :type {:type int?}} {:gene :lit, :val -425, :type {:type int?}} {:gene :lit, :val 0, :type {:type int?}} {:gene :var, :name map2-vec} {:gene :local, :idx 1065480610} {:gene :lit, :val 0, :type {:type int?}} {:gene :var, :name merge} {:gene :var, :name int-mult} {:gene :lit, :val -638, :type {:type int?}} {:gene :apply} {:gene :var, :name erp12.cbgp-lite.lang.lib/letter?} {:gene :local, :idx 1352188821} {:gene :local, :idx 871989827} {:gene :local, :idx 859981457} {:gene :local, :idx 737988616} {:gene :apply} {:gene :var, :name partial1-fn3} {:gene :var, :name int-quot} {:gene :apply} {:gene :lit, :val -965, :type {:type int?}} {:gene :fn, :arg-types [{:type boolean?}], :ret-type {:type :vector, :child {:type int?}}} {:gene :apply} {:gene :var, :name disj} {:gene :let} {:gene :lit, :val 0, :type {:type int?}} {:gene :local, :idx 850399289} {:gene :var, :name range3} {:gene :var, :name comp3-fn1} {:gene :lit, :val 2, :type {:type int?}} {:gene :apply} {:gene :var, :name left} {:gene :apply} {:gene :local, :idx 1749329675} {:gene :var, :name erp12.cbgp-lite.lang.lib/and} {:gene :apply} {:gene :close} {:gene :var, :name double} {:gene :close} {:gene :lit, :val 2, :type {:type int?}} {:gene :apply} {:gene :local, :idx 333663282} {:gene :var, :name get} {:gene :lit, :val 156, :type {:type int?}} {:gene :lit, :val 531, :type {:type int?}} {:gene :apply} {:gene :var, :name char-occurrences} {:gene :apply} {:gene :apply} {:gene :local, :idx 2087044041} {:gene :local, :idx 398212854} {:gene :var, :name fold-map} {:gene :apply} {:gene :local, :idx 2122008347} {:gene :lit, :val 0, :type {:type int?}} {:gene :close} {:gene :var, :name erp12.cbgp-lite.lang.lib/min'} {:gene :lit, :val 2, :type {:type int?}} {:gene :lit, :val 1, :type {:type int?}} {:gene :var, :name ->map3} {:gene :lit, :val -581, :type {:type int?}} {:gene :lit, :val 2, :type {:type int?}} {:gene :lit, :val 2, :type {:type int?}} {:gene :var, :name erp12.cbgp-lite.lang.lib/sortv-by} {:gene :lit, :val 687, :type {:type int?}} {:gene :close} {:gene :apply} {:gene :local, :idx 1146912485} {:gene :var, :name vec->set} {:gene :close} {:gene :local, :idx 542973907} {:gene :var, :name empty?} {:gene :local, :idx 1566252483} {:gene :lit, :val 0, :type {:type int?}} {:gene :close} {:gene :var, :name double} {:gene :close} {:gene :apply} {:gene :lit, :val 60, :type {:type int?}} {:gene :lit, :val 5, :type {:type int?}} {:gene :close} {:gene :fn, :arg-types [{:type boolean?} {:type boolean?}], :ret-type {:type int?}} {:gene :local, :idx 1265823840} {:gene :local, :idx 2015509806} {:gene :lit, :val 1, :type {:type int?}} {:gene :local, :idx 1393945392} {:gene :local, :idx 1504341923} {:gene :lit, :val 1, :type {:type int?}} {:gene :apply} {:gene :fn, :arg-types [{:type int?} {:type :vector, :child {:type int?}}], :ret-type {:type :vector, :child {:type int?}}} {:gene :lit, :val 1, :type {:type int?}} {:gene :apply} {:gene :lit, :val 0, :type {:type int?}} {:gene :local, :idx 1895097073} {:gene :apply} {:gene :local, :idx 985170710} {:gene :apply} {:gene :apply} {:gene :lit, :val 2, :type {:type int?}} {:gene :apply} {:gene :var, :name ->set2} {:gene :local, :idx 31457261} {:gene :apply} {:gene :apply} {:gene :lit, :val -438, :type {:type int?}} {:gene :lit, :val -723, :type {:type int?}} {:gene :var, :name set-contains?} {:gene :local, :idx 375721227} {:gene :local, :idx 1406167461} {:gene :lit, :val -459, :type {:type int?}} {:gene :local, :idx 1263908444} {:gene :let} {:gene :var, :name erp12.cbgp-lite.lang.lib/digit?} {:gene :lit, :val 1, :type {:type int?}} {:gene :var, :name clojure.set/union} {:gene :var, :name range1} {:gene :local, :idx 1853088482} {:gene :lit, :val 1, :type {:type int?}} {:gene :fn, :arg-types [{:type boolean?} {:type int?}], :ret-type {:type boolean?}} {:gene :var, :name count-vec} {:gene :var, :name first} {:gene :lit, :val 1, :type {:type int?}} {:gene :close} {:gene :local, :idx 2132048629} {:gene :apply} {:gene :var, :name erp12.cbgp-lite.lang.lib/keys-set} {:gene :lit, :val 2, :type {:type int?}} {:gene :apply} {:gene :apply} {:gene :local, :idx 283249942} {:gene :var, :name int-mult} {:gene :lit, :val 848, :type {:type int?}})]
    genome)

  ;; This goes trough the compilation process, which we want to reverse to
  ;; decompile
  (compile-debugging '({:gene :lit :val 33 :type {:type int?}}
                       {:gene :lit :val 22 :type {:type int?}}
                       {:gene :var :name int-add}
                       {:gene :apply}
                       ;;  {:gene :lit :val 5 :type {:type int?}}
                       )
                     {:type 'int?})

  (compile-debugging '({:gene :lit :val 33.2 :type {:type double?}}
                       {:gene :lit :val 22.33333 :type {:type double?}}
                       {:gene :var :name double-add}
                       {:gene :apply}
                       ;;  {:gene :lit :val 5 :type {:type int?}}
                       )
                     {:type 'double?})

  ;; Our first step for decompiling
  (read-string "(- 22 33)"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Below here is work on decompiling

(defn my-add
  [x y]
  (+ x y))

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

(def ast-multi-type-aliasing
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

    (contains? ast-multi-type-aliasing ast-fn-name)
    (symbol (str (if (= (str tag) "double")
                   "double-"
                   "int-")
                 (get ast-multi-type-aliasing ast-fn-name)))

    (contains? ast-aliasing ast-fn-name)
    (get ast-aliasing ast-fn-name)

    :else ast-fn-name))

(defn decompile-ast
  "Decompiles AST into a CBGP genome."
  [{:keys [op val tag method args] :as ast}]
  ;; (println "AST HERE:" ast)
  ;; (println)
  (cond
    ;; Handle constants
    (= :const op) (list (assoc {} :gene :lit
                               :val val
                               :type {:type (cond
                                              (integer? val) 'int?
                                              (number? val) 'double?
                                              (boolean? val) 'boolean?
                                              (string? val) 'string?
                                              (char? val) 'char?
                                              (nil? val) 'nil?
                                              :else (throw (Exception. (str "AST contains a type that shouldn't be possible: "
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

  (ana.jvm/analyze '(first [1 2]))

  (ana.jvm/analyze '(+ 1.2 2.3))

  (ana.jvm/analyze '(+ 1 2))

  (ana.jvm/analyze '(+ 22 (+ 33 44)))


  (ana.jvm/analyze '(+ 1 2.2))

  (ana.jvm/analyze '(+ 1.2 2))

  (ana.jvm/analyze '(inc 5))

  (ana.jvm/analyze '(map inc '(1 2 3)))

  (ana.jvm/analyze '(my-add 22 33))

  (ana.jvm/analyze '(not true))

  (ana.jvm/analyze true)

  (ana.jvm/analyze 5N)

  (ana.jvm/analyze '(or true false))

  (ana.jvm/analyze nil)


  (ana.jvm/analyze '(not= 2 3))

  (integer? 4)

  (float? 5M)

  (decompile-ast (ana.jvm/analyze 5))

  (decompile-ast (ana.jvm/analyze true))

  (decompile-ast (ana.jvm/analyze nil))

  (decompile-ast (ana.jvm/analyze [1 2]))

  (decompile-ast (ana.jvm/analyze '(my-add 22 33)))

  (decompile-ast (ana.jvm/analyze '(+ 22 33)))

  (decompile-ast (ana.jvm/analyze '(+ 22.3 33.2)))

  (decompile-ast (ana.jvm/analyze '(+ 22 (+ 33 44))))

  (decompile-ast (ana.jvm/analyze '(- 22 33)))


  (compile-debugging (decompile-ast (ana.jvm/analyze '(+ 22 33)))
                     {:type 'int?})

  ;; args in wrong order
  (compile-debugging (decompile-ast (ana.jvm/analyze '(+ 22 (+ 33 44))))
                     {:type 'int?})


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

  (compile-debugging (decompile-ast (ana.jvm/analyze '(< "hi" "hi")))
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

  (ana.jvm/analyze '(/ 80 7))


  (compile-debugging (decompile-ast (ana.jvm/analyze '(or true false)))
                     {:type 'boolean?})


  (compile-debugging (decompile-ast (ana.jvm/analyze '(+ 1 2 3 4)))
                     {:type 'int?})

  (compile-debugging (decompile-ast (ana.jvm/analyze '(* 1 2 3 4)))
                     {:type 'int?})

  (decompile-ast (ana.jvm/analyze '(+ 1 2 3 4)))

  (compile-debugging (decompile-ast (ana.jvm/analyze '(< 4 5 8)))
                     {:type 'boolean?})

  (ana.jvm/analyze '(+ 1 2 3 4))

  (ana.jvm/analyze
   '(< 4 5 8 17))
  
  (str "asdsa" "ASD")

  ;; vectors and sets etc

  )


