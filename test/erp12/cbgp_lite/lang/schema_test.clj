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

  )

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
  
  ;; add lib-test tests

  (sort
   (keys
    (lib-for-type-ctors #{'int? :vector :=>})))

  (sort
   (keys
    (lib-for-type-ctors #{:vector 'boolean? :=>})))

  (sort
   (keys
    (lib-for-type-ctors #{'boolean? :=>})))

  (set/superset? #{'int? :vector :=>} ;; type-ctors
                 #{'int? :=>}) ;; types from an instruction
  )