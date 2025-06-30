(ns erp12.cbgp-lite.lang.ad-hoc-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [clojure.walk :as w]
            [erp12.cbgp-lite.lang.ast :as a]
            [erp12.cbgp-lite.lang.compile :as c]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.lang.schema :as schema]
            [mb.hawk.core]
            [meander.epsilon :as m]))

;; Indexing vectors and strings
;; First, second, last, rest, butlast, nth, 


;; Collection Conversion
;; Vec, Set, ->map


;; Combining collections
;; Conj, Concat, join


;; Higher Order functions
;; Reduce, Filter, Remove


;; Other 
;; remove-element, in?