(ns erp12.cbgp-lite.lang.schema-test
  (:require [clojure.test :refer [deftest is]]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.lang.schema :as sch]))

(deftest occurs?-test
  (is (sch/occurs? 'a {:op :local :name 'a}))
  (is (sch/occurs? {:type :s-var :sym 'A}
                   {:type   :=>
                    :input  {:type     :cat
                             :children [{:type :s-var :sym 'A}]}
                    :output {:type 'int?}}))
  (is (not (sch/occurs? 'x {:op :var :var '+})))
  (is (not (sch/occurs? {:type :s-var :sym 'A}
                        {:type   :=>
                         :input  {:type     :cat
                                  :children [{:type :s-var :sym 'B}]}
                         :output {:type 'int?}}))))

(deftest schema-terms-test
  (is (= "hi there"
         (sch/schema-terms {:type :overloaded ;;; where does indexable go?
                                                  ;:typeclasses #{:indexable}
                            :alternatives [(lib/scheme (lib/fn-of [(lib/vector-of (lib/s-var 'a))] (lib/s-var 'a)))
                                           (lib/fn-of [lib/STRING] lib/CHAR)]})))

  (is (= "hi there"
         (sch/schema-terms {:type :overloaded ;;; where does indexable go?
                                                    ;:typeclasses #{:indexable}
                            :alternatives [(lib/scheme (lib/fn-of [(lib/vector-of (lib/s-var 'a))] (lib/s-var 'a)))
                                           (lib/fn-of [lib/STRING] lib/CHAR)]}))) 
  (is (= '(* +
             -
             ->vector1
             ->vector2
             ->vector3
             abs
             comp
             count
             dec
             first
             fold
             inc
             last
             mapv
             mod
             nth-or-else
             partial
             quot
             range1
             range2
             range3
             reduce
             erp12.cbgp-lite.lang.lib/butlast'
             erp12.cbgp-lite.lang.lib/concat'
             erp12.cbgp-lite.lang.lib/conj'
             erp12.cbgp-lite.lang.lib/distinctv
             erp12.cbgp-lite.lang.lib/index-of
             erp12.cbgp-lite.lang.lib/map2v
             erp12.cbgp-lite.lang.lib/mapcat'
             erp12.cbgp-lite.lang.lib/mapv-indexed
             erp12.cbgp-lite.lang.lib/max'
             erp12.cbgp-lite.lang.lib/min'
             erp12.cbgp-lite.lang.lib/neg
             erp12.cbgp-lite.lang.lib/occurrences-of
             erp12.cbgp-lite.lang.lib/pow
             erp12.cbgp-lite.lang.lib/remove-element
             erp12.cbgp-lite.lang.lib/replace'
             erp12.cbgp-lite.lang.lib/replace-first'
             erp12.cbgp-lite.lang.lib/rest'
             erp12.cbgp-lite.lang.lib/reverse'
             erp12.cbgp-lite.lang.lib/safe-assoc-nth
             erp12.cbgp-lite.lang.lib/safe-nth
             erp12.cbgp-lite.lang.lib/safe-sub
             erp12.cbgp-lite.lang.lib/sort'
             erp12.cbgp-lite.lang.lib/sortv-by
             erp12.cbgp-lite.lang.lib/square
             erp12.cbgp-lite.lang.lib/take')
         (sort
          (keys
           (lib/lib-for-type-ctors #{'int? :vector :=>})))))

  (is (= '(->vector1 ->vector2
                     ->vector3
                     =
                     comp
                     empty?
                     first
                     fold
                     if
                     last
                     mapv
                     not
                     not=
                     partial
                     reduce
                     erp12.cbgp-lite.lang.lib/<'
                     erp12.cbgp-lite.lang.lib/<='
                     erp12.cbgp-lite.lang.lib/>'
                     erp12.cbgp-lite.lang.lib/>='
                     erp12.cbgp-lite.lang.lib/and
                     erp12.cbgp-lite.lang.lib/butlast'
                     erp12.cbgp-lite.lang.lib/concat'
                     erp12.cbgp-lite.lang.lib/conj'
                     erp12.cbgp-lite.lang.lib/distinctv
                     erp12.cbgp-lite.lang.lib/filter'
                     erp12.cbgp-lite.lang.lib/in?
                     erp12.cbgp-lite.lang.lib/map2v
                     erp12.cbgp-lite.lang.lib/mapcat'
                     erp12.cbgp-lite.lang.lib/max'
                     erp12.cbgp-lite.lang.lib/min'
                     erp12.cbgp-lite.lang.lib/or
                     erp12.cbgp-lite.lang.lib/remove'
                     erp12.cbgp-lite.lang.lib/remove-element
                     erp12.cbgp-lite.lang.lib/replace'
                     erp12.cbgp-lite.lang.lib/replace-first'
                     erp12.cbgp-lite.lang.lib/rest'
                     erp12.cbgp-lite.lang.lib/reverse'
                     erp12.cbgp-lite.lang.lib/sort'
                     erp12.cbgp-lite.lang.lib/sortv-by)
           (sort 
            (keys 
             (lib/lib-for-type-ctors #{:vector 'boolean? :=>})))))

  (is (= '(= comp
             if
             not
             not=
             partial
             erp12.cbgp-lite.lang.lib/<'
             erp12.cbgp-lite.lang.lib/<='
             erp12.cbgp-lite.lang.lib/>'
             erp12.cbgp-lite.lang.lib/>='
             erp12.cbgp-lite.lang.lib/and
             erp12.cbgp-lite.lang.lib/max'
             erp12.cbgp-lite.lang.lib/min'
             erp12.cbgp-lite.lang.lib/or)
         (sort
          (keys
           (lib/lib-for-type-ctors #{'boolean? :=>}))))))

(comment

  (use 'erp12.cbgp-lite.lang.lib)

  (decompose-typeclass #{#{:countable} :=> :cat :s-var :scheme 'int? {:sym 'c, :type :s-var}})

  ;; digit?
  (schema-terms (unary-pred CHAR))
  ;;=> #{:cat boolean? char? :=>}

  ;; get
  (schema-terms (scheme (fn-of [(map-of (s-var 'k) (s-var 'v)) (s-var 'k)]
                               (s-var 'v))))
  ;;=> #{:cat :s-var :=> :map-of :scheme}

  ;; +  
  (schema-terms (scheme (fn-of [(s-var 'a) (s-var 'a)] (s-var 'a)) {'a #{:number}}))
  ;;=> #{:cat :s-var :=> :scheme}

  ;; count
  (schema-terms (scheme (fn-of [{:type (s-var 'c)}] INT)
                        {'c #{:countable}}))
  ;;=> #{:cat :s-var int? :=> :scheme}
  )


(comment 
  
  (set/superset? #{'int? :vector :=>} ;; type-ctors
                 #{'int? :=>}) ;; types from an instruction
  )