(ns erp12.cbgp-lite.benchmark.suite.big-ast
  (:require [clj-fuzzy.levenshtein :as lev]
            [clojure.string :as str]
            [erp12.cbgp-lite.benchmark.utils :as bu]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.search.individual :as i]
            [erp12.cbgp-lite.task :as task]))

(defn make-big-ast-loss
  [ind-or-ast-or-behaviors-i-dont-know]
  5)

(defn problems
  "Needs to take config map and return a map of problem names (strings)
   to maps (info about the problem). Probably fine to just return the one
   problem's info that is being used."
  [{:keys [penalty]}] 
  {"make-big-ast"
   {:description    "Doesn't run programs. Instead, only goal is to make an AST as large as possible."
    :n-train        0
    :n-test         0
    :input->type    {'input1 {:type :map-of, :key {:type 'string?}, :value {:type 'int?}}
                     'input2  {:type :vector :child {:type 'string?}}
                     'input3 {:type 'int?}}
    :ret-type       {:type 'int?}
    :other-type-ctors    #{'double? 'int? 'boolean? 'char?}
    :extra-genes    [{:gene :lit-generator, :fn (bu/int-generator 100), :type {:type 'int?}}
                     {:gene :lit, :val true, :type {:type 'boolean?}}
                     {:gene :lit, :val false, :type {:type 'boolean?}}
                     {:gene :lit, :val #{}, :type {:type :set :child {:type 'int?}}}]
    :loss-fns       [make-big-ast-loss]}})

(defn read-cases
  "Needs to take config map and return map of train and test cases."
  [_] 
  {:train '()
   :test  '()})



(comment
  
  (make-big-ast-loss 2)

  (/ 15 2)

  (clojure.pprint/pprint '(occurrences-of (safe-assoc-nth (replace' (safe-assoc-nth (vector #{} #{} #{}) (occurrences-of (vector #{} #{} #{}) #{}) (conj' #{} (first (replace' (replace-first' (map2v - (replace' (vector -90) input3 94) ((if false sortv-by sortv-by) safe-log2 (rest' (replace-first' (rangev input3) (square input3) 31)))) -62 84) 4 (first (assoc-right (assoc-left (vector (safe-mod input3 input3) false) input3) (safe-assoc-nth (replace' (safe-assoc-nth (butlast' (vector 93 input3)) -83 input3) 60 -52) (min' 78 input3) -62))))))) (conj' #{} 51) #{}) (dec -83) #{}) #{}))
  

  
  )

