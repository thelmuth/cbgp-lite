(ns erp12.cbgp-lite.lang.lib
  (:refer-clojure :exclude [and or vector-of])
  (:require [clojure.core :as core]
            [clojure.set :as set]
            [clojure.string :as str]
            [erp12.cbgp-lite.lang.schema :as schema]))

;; @todo What do do about nil?
;; first, last, etc. return nil on empty collections.
;; inc, +, etc. throw on nil.

(defn and
  [a b]
  ;; Wrap th macro
  (core/and a b))

(defn or
  [a b]
  ;; Wrap the macro
  (core/or a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic Comparisons

(defn <'
  [a b]
  (< (compare a b) 0))

(defn <='
  [a b]
  (core/or (= a b) (<' a b)))

(defn >'
  [a b]
  (not (<=' a b)))

(defn >='
  [a b]
  (not (<' a b)))

(defn min'
  [a b]
  (if (<' a b) a b))

(defn max'
  [a b]
  (if (<' a b) b a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Math

(defn neg
  [n]
  (- n))

(defn safe-div
  [n d]
  (if (zero? d) 0 (/ n d)))

(defn safe-mod
  [n d]
  (if (zero? d) 0 (mod n d)))

(defn safe-quot
  [n d]
  (if (zero? d) 0 (quot n d)))

;; @todo Switch to clojure.math (in v1.11 and above)

(defn sin
  [x]
  (Math/sin x))

(defn cos
  [x]
  (Math/cos x))

(defn tan
  [x]
  (Math/tan x))

(defn abs'
  "Returns absolute value, coercing to bigint if necessary."
  [x]
  (if (neg? x)
    (-' x)
    x))

;; We've decided to make safe-pow => pow, and have it test arguments
;; to see if they're integers (if so, cast to long) or not (return double)
;; We could instead just leave int-pow and double-pow as monomorphized and,
;; when decompiling, only use the double version
;; Same with square (and maybe others)
(defn pow
  [x y]
  (let [result (Math/pow x y)]
    (cond
      (or (NaN? result) (infinite? result))
      (cond
        (zero? x) 0
        (neg? x) (recur (abs' x) y)
        :else (throw (ex-info "Pow resulting in undefined value."
                              {:base x :exponent y})))

      (and (integer? x) (integer? y))
      (long result)

      :else
      result)))

(defn square
  [x]
  (pow x 2))

(defn safe-sqrt
  [x]
  (Math/sqrt (abs x)))

(defn safe-log2
  [x]
  (let [safe-x (if (<= x 0) Float/MIN_VALUE x)]
    (/ (Math/log safe-x)
       (Math/log 2))))

(defn safe-log10
  [x]
  (let [safe-x (if (<= x 0) Float/MIN_VALUE x)]
    (Math/log10 safe-x)))

(defn ceil
  [x]
  (Math/ceil x))

(defn int-ceil
  [x]
  (long (Math/ceil x)))

(defn floor
  [x]
  (Math/floor x))

(defn int-floor
  [x]
  (long (Math/floor x)))

(defn- safe-trig-x
  [x]
  (dec (mod (inc x) 2)))

(defn safe-acos
  [x]
  (if (= 1.0 (mod x 2))
    0.0
    (let [safe-x (safe-trig-x x)]
      (Math/acos safe-x))))

(defn safe-asin
  [x]
  (if (= 1.0 (mod x 2))
    (/ Math/PI 2)
    (let [safe-x (safe-trig-x x)]
      (Math/asin safe-x))))

(defn atan
  [x]
  (Math/atan x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text

(defn int->char
  [i]
  (char (mod i 128)))

(def concat-str (comp str/join concat))

(def ^:private regex-char-esc-smap
  (let [esc-chars "()*&^%$#!"]
    (zipmap esc-chars
            (map #(str "\\" %) esc-chars))))

(defn- str-to-pattern
  [string]
  (->> string
       str
       (replace regex-char-esc-smap)
       str/join
       re-pattern))

(defn split-str
  [s on]
  (str/split s (str-to-pattern on)))

(defn split-str-on-ws
  [s]
  (str/split (str/trim s) #"\s+"))

(defn set-char
  [s idx c]
  (if (empty? s)
    s
    (let [safe-idx (mod idx (count s))]
      (apply str (assoc (vec s) safe-idx c)))))

(defn whitespace?
  [^Character c]
  (Character/isWhitespace c))

(defn digit?
  [^Character c]
  (Character/isDigit c))

(defn letter?
  [^Character c]
  (Character/isLetter c))

(defn char-upper
  [^Character c]
  (Character/toUpperCase c))

(defn char-lower
  [^Character c]
  (Character/toLowerCase c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collections

(defn filter'
  [pred coll]
  (let [filtered (filter pred coll)]
    (if (string? coll)
      (apply str filtered)
      (into (empty coll) filtered))))

(defn remove'
  [pred coll]
  (let [removed (remove pred coll)]
    (if (string? coll)
      (apply str removed)
      (into (empty coll) removed))))

(defn mapcat'
  [pred coll]
  (vec (mapcat pred coll)))

(defn conj'
  [coll target]
  (cond 
    (set? coll)
    ((comp set conj) coll target)
    ;; (nil? coll)
    ;; (throw (Exception. "Conj' called on nil"))
    :else
    ((comp vec conj) coll target)))

(defn concat'
  [coll1 coll2]
  (if (string? coll1)
    (apply str (concat coll1 coll2))
    ((comp vec concat) coll1 coll2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vector

(def distinctv (comp vec distinct))
(def mapv-indexed (comp vec map-indexed))
(def sortv-by (comp vec sort-by))

(def rangev
  ;; Cap the range to avoid memory errors.
  (comp vec #(take 100 %) range))

(defn index-of
  [coll el]
  (if (string? coll)
    (str/index-of coll (str el))
    (.indexOf coll el)))

(defn occurrences-of
  [coll el]
  (count (filter #{el} coll)))

(defn in?
  [coll el]
  (if (string? coll)
    (str/includes? coll (str el))
    (<= 0 (.indexOf coll el))))

(defn remove-element
  [coll element]
  (let [removed (remove #{element} coll)]
    (if (string? coll)
      (apply str removed)
      (vec removed))))

(defn safe-assoc-nth
  [vtr idx el]
  (if (empty? vtr)
    vtr
    (let [idx (mod idx (count vtr))]
      (assoc vtr idx el))))

(defn safe-nth
  [coll idx]
  (if (empty? coll)
    (throw (ex-info "Cannot take safe-nth of empty vector." {:coll coll :idx idx}))
    (let [idx (mod idx (count coll))]
      (nth coll idx))))

(defn safe-sub-coll
  [coll start end]
  (let [start (min (count coll) (max 0 start))
        end (min (count coll) (max start end))]
    (if (string? coll)
      (subs coll start end)
      (subvec coll start end))))

(defn map2v
  [expr coll1 coll2]
  (mapv expr coll1 coll2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fixing LazySeqs

(defn rest'
  [coll]
  (if (string? coll)
    (apply str (rest coll))
    ((comp vec rest) coll)))

(defn butlast'
  [coll]
  (if (string? coll)
    (apply str (butlast coll))
    ((comp vec butlast) coll)))

(defn replace'
  [coll target replacement]
  (if (string? coll)
    (str/replace coll target replacement)
    (replace {target replacement} coll)))

(defn replace-first'
  [coll target replacement]
  (if (string? coll)
    (str/replace-first coll target replacement)
    (let [idx (.indexOf coll target)]
      (if (< idx 0)
        coll
        (assoc coll idx replacement)))))

(defn take'
  [num coll]
  (if (string? coll)
    (apply str (take num coll))
    (into (empty coll) (take num coll))))

(defn reverse'
  [coll]
  (if (string? coll)
    (apply str (reverse coll))
    (vec (reverse coll))))

(defn sort'
  [coll]
  (if (string? coll)
    (str/join (sort coll))
    ((comp vec sort) coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set

(defn map-set [f s] (into #{} (map f s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map

(defn ->map
  [coll]
  (into {} coll))

(def keys-vec (comp vec keys))
(def keys-set (comp set keys))
(def vals-vec (comp vec vals))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tuple

(defn assoc-tuple
  [tup i x]
  (assoc (or tup [nil nil]) i x))

(defn assoc-left [tuple val] (assoc-tuple tuple 0 val))
(defn assoc-right [tuple val] (assoc-tuple tuple 1 val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ground Schemas
;; nil? boolean? int? double? char? string? keyword?

(def NIL {:type 'nil?})
(def BOOLEAN {:type 'boolean?})
(def INT {:type 'int?})
(def DOUBLE {:type 'double?})
(def CHAR {:type 'char?})
(def STRING {:type 'string?})
(def KEYWORD {:type 'keyword?})

(def ground-schema-ctors
  (set (map :type [NIL BOOLEAN INT DOUBLE CHAR STRING KEYWORD])))

(defn unary-transform
  [type]
  {:type   :=>
   :input  {:type :cat :children [type]}
   :output type})

(defn binary-transform
  [type]
  {:type   :=>
   :input  {:type :cat :children [type type]}
   :output type})

(defn unary-pred
  [type]
  {:type   :=>
   :input  {:type :cat :children [type]}
   :output {:type 'boolean?}})

(defn binary-pred
  [type]
  {:type   :=>
   :input  {:type :cat :children [type type]}
   :output {:type 'boolean?}})

(defn fn-of
  [args ret]
  {:type   :=>
   :input  {:type :cat :children (vec args)}
   :output ret})

(defn s-var
  [sym]
  {:type :s-var :sym sym})

(defn vector-of
  [el]
  {:type :vector :child el})

(defn map-of
  [k v]
  {:type :map-of :key k :value v})

(defn set-of
  [el]
  {:type :set :child el})

(defn tuple-of
  [& els]
  {:type :tuple :children (vec els)})

(defn scheme
  "Optional second argument is a map from symbols to the typeclasses they should
   have in the resulting scheme"
  ([schema]
   (schema/generalize {} schema))
  ([schema typeclasses-of-s-vars]
   (let [result-scheme (schema/generalize {} schema)
         s-vars (:s-vars result-scheme)
         s-vars-with-tcs (mapv #(cond-> %
                                  (contains? typeclasses-of-s-vars (:sym %))
                                  (assoc :typeclasses (get typeclasses-of-s-vars (:sym %))))
                               s-vars)]
     (assoc result-scheme :s-vars s-vars-with-tcs))))

(def type-env
  {;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; FP
   'comp2-fn1          (scheme (fn-of [(fn-of [(s-var 'b)] (s-var 'c))
                                       (fn-of [(s-var 'a)] (s-var 'b))]
                                      (fn-of [(s-var 'a)] (s-var 'c))))
   'comp3-fn1          (scheme (fn-of [(fn-of [(s-var 'c)] (s-var 'd))
                                       (fn-of [(s-var 'b)] (s-var 'c))
                                       (fn-of [(s-var 'a)] (s-var 'b))]
                                      (fn-of [(s-var 'a)] (s-var 'd))))
   'comp2-fn2          (scheme (fn-of [(fn-of [(s-var 'c)] (s-var 'd))
                                       (fn-of [(s-var 'a) (s-var 'b)] (s-var 'c))]
                                      (fn-of [(s-var 'a) (s-var 'b)] (s-var 'd))))
   'comp3-fn2          (scheme (fn-of [(fn-of [(s-var 'd)] (s-var 'e))
                                       (fn-of [(s-var 'c)] (s-var 'd))
                                       (fn-of [(s-var 'a) (s-var 'b)] (s-var 'c))]
                                      (fn-of [(s-var 'a) (s-var 'b)] (s-var 'e))))
   'partial1-fn2       (scheme (fn-of [(fn-of [(s-var 'a) (s-var 'b)] (s-var 'c))
                                       (s-var 'a)]
                                      (fn-of [(s-var 'b)] (s-var 'c))))
   'partial1-fn3       (scheme (fn-of [(fn-of [(s-var 'a) (s-var 'b) (s-var 'c)] (s-var 'd))
                                       (s-var 'a)]
                                      (fn-of [(s-var 'b) (s-var 'c)] (s-var 'd))))
   'partial2-fn3       (scheme (fn-of [(fn-of [(s-var 'a) (s-var 'b) (s-var 'c)] (s-var 'd))
                                       (s-var 'a)
                                       (s-var 'b)]
                                      (fn-of [(s-var 'c)] (s-var 'd))))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Conditional Control Flow
   'if                 (scheme (fn-of [BOOLEAN (s-var 'a) (s-var 'a)]
                                      (s-var 'a)))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Common
   '=                  (scheme (fn-of [(s-var 'a) (s-var 'a)] BOOLEAN))
   'not=               (scheme (fn-of [(s-var 'a) (s-var 'a)] BOOLEAN))
   `<'                 (scheme (fn-of [(s-var 'a) (s-var 'a)] BOOLEAN) {'a #{:comparable}})
   `<='                (scheme (fn-of [(s-var 'a) (s-var 'a)] BOOLEAN) {'a #{:comparable}})
   `>'                 (scheme (fn-of [(s-var 'a) (s-var 'a)] BOOLEAN) {'a #{:comparable}})
   `>='                (scheme (fn-of [(s-var 'a) (s-var 'a)] BOOLEAN) {'a #{:comparable}})
   ;; @todo Multiple arity of min/max
   `min'               (scheme (fn-of [(s-var 'a) (s-var 'a)] (s-var 'a)) {'a #{:comparable}})
   `max'               (scheme (fn-of [(s-var 'a) (s-var 'a)] (s-var 'a)) {'a #{:comparable}})
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Numeric 
   '+                  (scheme (fn-of [(s-var 'a) (s-var 'a)] (s-var 'a)) {'a #{:number}})
   '-                  (scheme (fn-of [(s-var 'a) (s-var 'a)] (s-var 'a)) {'a #{:number}})
   '*                  (scheme (fn-of [(s-var 'a) (s-var 'a)] (s-var 'a)) {'a #{:number}})
   `safe-quot          (scheme (fn-of [(s-var 'a) (s-var 'a)] (s-var 'a)) {'a #{:number}})
   `safe-div           (scheme (fn-of [(s-var 'a) (s-var 'a)] DOUBLE) {'a #{:number}})
   `safe-mod           (scheme (fn-of [(s-var 'a) (s-var 'a)] (s-var 'a)) {'a #{:number}})
   'inc                (scheme (fn-of [(s-var 'a)] (s-var 'a)) {'a #{:number}})
   'dec                (scheme (fn-of [(s-var 'a)] (s-var 'a)) {'a #{:number}})
   `neg                (scheme (fn-of [(s-var 'a)] (s-var 'a)) {'a #{:number}})
   'abs                (scheme (fn-of [(s-var 'a)] (s-var 'a)) {'a #{:number}})
   `pow                (scheme (fn-of [(s-var 'a) (s-var 'a)] (s-var 'a)) {'a #{:number}})
   `square             (scheme (fn-of [(s-var 'a)] (s-var 'a)) {'a #{:number}})
   `int-ceil           (fn-of [DOUBLE] INT)
   `int-floor          (fn-of [DOUBLE] INT)
   'int                (scheme (fn-of [(s-var 'a)] INT) {'a #{:intable}})
   'double             (fn-of [INT] DOUBLE)
   `safe-sqrt          (scheme (fn-of [(s-var 'a)] DOUBLE) {'a #{:number}})
   `sin                (unary-transform DOUBLE)
   `cos                (unary-transform DOUBLE)
   `tan                (unary-transform DOUBLE)
   `safe-asin          (unary-transform DOUBLE)
   `safe-acos          (unary-transform DOUBLE)
   `atan               (unary-transform DOUBLE)
   `safe-log2          (scheme (fn-of [(s-var 'a)] DOUBLE) {'a #{:number}})
   `safe-log10         (scheme (fn-of [(s-var 'a)] DOUBLE) {'a #{:number}})
   `ceil               (unary-transform DOUBLE)
   `floor              (unary-transform DOUBLE)
   'zero?              (scheme (fn-of [(s-var 'a)] BOOLEAN) {'a #{:number}})
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Text
   `str/join           (scheme (fn-of [(vector-of (s-var 'c))] STRING) {'c #{:stringable}})
   'str                (scheme (fn-of [(s-var 't)] STRING))
   `int->char          (fn-of [INT] CHAR)
   `whitespace?        (unary-pred CHAR)
   `digit?             (unary-pred CHAR)
   `letter?            (unary-pred CHAR)
   'append-str         (fn-of [STRING CHAR] STRING)
   `split-str-on-ws    (fn-of [STRING] (vector-of STRING))
   `split-str          (fn-of [STRING STRING] (vector-of STRING))
   'split-str-on-char  (fn-of [STRING CHAR] (vector-of STRING))
   `set-char           (fn-of [STRING INT CHAR] STRING)
   'str-join-sep       (fn-of [STRING (vector-of STRING)] STRING)
   `str/capitalize     (unary-transform STRING)
   `str/upper-case     (unary-transform STRING)
   `str/lower-case     (unary-transform STRING)
   `char-upper         (unary-transform CHAR)
   `char-lower         (unary-transform CHAR)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Boolean
   `and                (binary-transform BOOLEAN)
   `or                 (binary-transform BOOLEAN)
   'not                (unary-transform BOOLEAN)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; (previously) Polymorphic collection functions  
   'count              (scheme (fn-of [{:type (s-var 'c)}] INT)
                               {'c #{:countable}})
   'mapv-vec            (scheme (fn-of [(fn-of [(s-var 'a)] (s-var 'b))
                                        (vector-of (s-var 'a))]
                                       (vector-of (s-var 'b))))
   'mapv-str            (scheme (fn-of [(fn-of [CHAR] (s-var 'a))
                                        STRING]
                                       (vector-of (s-var 'a))))
   'mapv-set            (scheme (fn-of [(fn-of [(s-var 'a)] (s-var 'b))
                                        (set-of (s-var 'a))]
                                       (vector-of (s-var 'b))))
   'mapv-map            (scheme (fn-of [(fn-of [(tuple-of (s-var 'k) (s-var 'v))] (s-var 'e))
                                        (map-of (s-var 'k) (s-var 'v))]
                                       (vector-of (s-var 'e))))
   'map2v-vec           (scheme (fn-of [(fn-of [(s-var 'a1) (s-var 'a2)] (s-var 'b))
                                        (vector-of (s-var 'a1))
                                        (vector-of (s-var 'a2))]
                                       (vector-of (s-var 'b))))
   'map2v-str           (scheme (fn-of [(fn-of [CHAR CHAR] (s-var 'a))
                                        STRING
                                        STRING]
                                       (vector-of (s-var 'a))))
   'map->vec           (scheme (fn-of [(map-of (s-var 'k) (s-var 'v))] (vector-of (tuple-of (s-var 'k) (s-var 'v)))))
   'set->vec           (scheme (fn-of [(set-of (s-var 'e))] (vector-of (s-var 'e))))
   'str->vec           (scheme (fn-of [STRING] (vector-of CHAR)))
   'map->set           (scheme (fn-of [(map-of (s-var 'k) (s-var 'v))] (set-of (tuple-of (s-var 'k) (s-var 'v)))))
   'vec->set           (scheme (fn-of [(vector-of (s-var 'e))] (set-of (s-var 'e))))
   'set->map           (scheme (fn-of [(set-of (tuple-of (s-var 'k) (s-var 'v)))] (map-of (s-var 'k) (s-var 'v))))
   'vec->map           (scheme (fn-of [(vector-of (tuple-of (s-var 'k) (s-var 'v)))] (map-of (s-var 'k) (s-var 'v)))) 
   'concat-str         (fn-of [STRING STRING] STRING)
   'concat-vec         (scheme (binary-transform (vector-of (s-var 'a))))
   'conj-set           (scheme (fn-of [(set-of (s-var 'e)) (s-var 'e)]
                                      (set-of (s-var 'e))))
   'conj-vec           (scheme (fn-of [(vector-of (s-var 'a)) (s-var 'a)] (vector-of (s-var 'a))))

   'first-str          (fn-of [STRING] CHAR)
   'first-vec          (scheme (fn-of [(vector-of (s-var 'a))] (s-var 'a))) 
   'last-str           (fn-of [STRING] CHAR)
   'last-vec           (scheme (fn-of [(vector-of (s-var 'a))] (s-var 'a))) 
   'rest-str           (unary-transform STRING)
   'rest-vec           (scheme (fn-of [(vector-of (s-var 'a))] (vector-of (s-var 'a))))
   'butlast-str        (unary-transform STRING) 
   'butlast-vec        (scheme (fn-of [(vector-of (s-var 'a))] (vector-of (s-var 'a)))) 
   'empty?             (scheme (fn-of [(s-var 'a)] BOOLEAN) {'a #{:countable}})
   'str/includes?      (binary-pred STRING)
   'char-in?           (fn-of [STRING CHAR] BOOLEAN)
   'in?                (scheme (fn-of [(vector-of (s-var 'a)) (s-var 'a)] BOOLEAN)) 
   `index-of           (scheme (fn-of [(s-var 'c) (s-var 'a)] INT) {'c #{:indexable}})
   'contains?          (scheme (fn-of [(s-var 'c) (s-var 'a)] BOOLEAN) {'c #{:keyable}})

   'filter-map         (scheme (fn-of [(fn-of [(tuple-of (s-var 'k) (s-var 'v))] BOOLEAN)
                                       (map-of (s-var 'k) (s-var 'v))]
                                      (map-of (s-var 'k) (s-var 'v))))
   'filter-set         (scheme (fn-of [(fn-of [(s-var 'a)] BOOLEAN)
                                       (set-of (s-var 'a))]
                                      (set-of (s-var 'a))))
   'filter-str         (fn-of [(fn-of [CHAR] BOOLEAN) STRING] STRING)
   'filter-vec         (scheme (fn-of [(fn-of [(s-var 'a)] BOOLEAN)
                                       (vector-of (s-var 'a))]
                                      (vector-of (s-var 'a))))
   'remove-map         (scheme (fn-of [(fn-of [(tuple-of (s-var 'k) (s-var 'v))] BOOLEAN) 
                                       (map-of (s-var 'k) (s-var 'v))]
                                       (map-of (s-var 'k) (s-var 'v))))
   'remove-set         (scheme (fn-of [(fn-of [(s-var 'a)] BOOLEAN)
                                       (set-of (s-var 'a))]
                                      (set-of (s-var 'a))))
   'remove-str         (fn-of [(fn-of [CHAR] BOOLEAN) STRING] STRING)
   'remove-vec         (scheme (fn-of [(fn-of [(s-var 'a)] BOOLEAN)
                                       (vector-of (s-var 'a))]
                                      (vector-of (s-var 'a))))
   'remove-char        (fn-of [STRING CHAR] STRING)
   `remove-element     (scheme (fn-of [(vector-of (s-var 'a)) (s-var 'a)]
                                      (vector-of (s-var 'a))))
   
   'reduce-map         (scheme (fn-of [(fn-of [(tuple-of (s-var 'k) (s-var 'v))
                                               (tuple-of (s-var 'k) (s-var 'v))]
                                              (tuple-of (s-var 'k) (s-var 'v)))
                                       (map-of (s-var 'k) (s-var 'v))]
                                      (tuple-of (s-var 'k) (s-var 'v))))
   'reduce-set         (scheme (fn-of [(fn-of [(s-var 'a) (s-var 'a)] (s-var 'a))
                                       (set-of (s-var 'a))]
                                      (s-var 'a)))
   'reduce-vec         (scheme (fn-of [(fn-of [(s-var 'a) (s-var 'a)] (s-var 'a))
                                       (vector-of (s-var 'a))]
                                      (s-var 'a)))
   'fold-map           (scheme (fn-of [(fn-of [(s-var 'r)
                                               (tuple-of (s-var 'k) (s-var 'v))]
                                              (s-var 'r))
                                       (s-var 'r)
                                       (map-of (s-var 'k) (s-var 'v))]
                                      (s-var 'r)))
   'fold-set           (scheme (fn-of [(fn-of [(s-var 'b) (s-var 'a)] (s-var 'b))
                                       (s-var 'b)
                                       (set-of (s-var 'a))]
                                      (s-var 'b)))
   'fold-str           (scheme (fn-of [(fn-of [(s-var 'a) CHAR] (s-var 'a))
                                       (s-var 'a)
                                       STRING]
                                      (s-var 'a)))
   'fold-vec           (scheme (fn-of [(fn-of [(s-var 'b) (s-var 'a)] (s-var 'b))
                                       (s-var 'b)
                                       (vector-of (s-var 'a))]
                                      (s-var 'b)))
   'mapcat-str         (scheme (fn-of [(fn-of [CHAR] (vector-of (s-var 'a)))
                                       STRING]
                                      (vector-of (s-var 'a))))
   'mapcat-vec         (scheme (fn-of [(fn-of [(s-var 'a)] (vector-of (s-var 'b)))
                                       (vector-of (s-var 'a))]
                                      (vector-of (s-var 'b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Text/Vec
   'nth-str           (fn-of [STRING INT] CHAR)
   `safe-nth          (scheme (fn-of [(vector-of (s-var 'a)) INT] (s-var 'a)))

   'replace-str       (fn-of [STRING STRING STRING] STRING)
   'replace-char      (fn-of [STRING CHAR CHAR] STRING)
   'replace-vec       (scheme (fn-of [(vector-of (s-var 'a))
                                      (s-var 'a)
                                      (s-var 'a)]
                                     (vector-of (s-var 'a)))) 
   'replace-first-str       (fn-of [STRING STRING STRING] STRING)
   'replace-first-char      (fn-of [STRING CHAR CHAR] STRING)
   'replace-first-vec       (scheme (fn-of [(vector-of (s-var 'a))
                                            (s-var 'a)
                                            (s-var 'a)]
                                           (vector-of (s-var 'a))))
   'take-str                (fn-of [INT STRING] STRING)
   'take-vec                (scheme (fn-of [INT (vector-of (s-var 'a))]
                                           (vector-of (s-var 'a))))
   'reverse-str             (unary-transform STRING)
   'reverse-vec             (scheme (fn-of [(vector-of (s-var 'a))] (vector-of (s-var 'a))))
   'sort-str                (unary-transform STRING)
   'sort-vec                (scheme (fn-of [(vector-of (s-var 'e))]
                                           (vector-of (s-var 'e))))
   'safe-sub-str            (fn-of [STRING INT INT] STRING)
   'safe-sub-vec            (scheme (fn-of [(vector-of (s-var 'a)) INT INT]
                                           (vector-of (s-var 'a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Vector
   '->vector1          (scheme (fn-of [(s-var 'a)]
                                      (vector-of (s-var 'a))))
   '->vector2          (scheme (fn-of [(s-var 'a) (s-var 'a)]
                                      (vector-of (s-var 'a))))
   '->vector3          (scheme (fn-of [(s-var 'a) (s-var 'a) (s-var 'a)]
                                      (vector-of (s-var 'a))))
   'nth-or-else        (scheme (fn-of [(vector-of (s-var 'a)) INT (s-var 'a)] (s-var 'a)))
   'char-occurrences-of (fn-of [STRING CHAR] INT)
   `occurrences-of      (scheme (fn-of [(vector-of (s-var 'a)) (s-var 'a)] INT)) 
   `safe-assoc-nth     (scheme (fn-of [(vector-of (s-var 'a)) INT (s-var 'a)]
                                      (vector-of (s-var 'a))))
   'range1             (scheme (fn-of [INT] (vector-of INT)))
   'range2             (scheme (fn-of [INT INT] (vector-of INT)))
   'range3             (scheme (fn-of [INT INT INT] (vector-of INT)))
   `mapv-indexed       (scheme (fn-of [(fn-of [INT {:type :s-var :sym 'a :typeclasses #{:indexable}}] (s-var 'b))
                                       (vector-of {:type :s-var :sym 'a :typeclasses #{:indexable}})]
                                      (vector-of (s-var 'b))))
   `distinctv          (scheme (fn-of [(vector-of (s-var 'e))]
                                      (vector-of (s-var 'e))))
   `sortv-by           (scheme (fn-of [(fn-of [{:type :s-var :sym 'e :typeclasses #{:comparable}}] (s-var 'k))
                                       (vector-of {:type :s-var :sym 'e :typeclasses #{:comparable}})]
                                      (vector-of (s-var 'e))))
   'group-by           (scheme (fn-of [(fn-of [(s-var 'e)] (s-var 'k))
                                       (vector-of (s-var 'e))]
                                      (map-of (s-var 'k) (vector-of (s-var 'e)))))
   'zipmap             (scheme (fn-of [(vector-of (s-var 'k))
                                       (vector-of (s-var 'v))]
                                      (map-of (s-var 'k) (s-var 'v))))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;; Tuple
   '->tuple2           (scheme (fn-of [(s-var 'a) (s-var 'b)]
                                      (tuple-of (s-var 'a) (s-var 'b))))
   'left               (scheme (fn-of [(tuple-of (s-var 'a) (s-var 'b))]
                                      (s-var 'a)))
   'right              (scheme (fn-of [(tuple-of (s-var 'a) (s-var 'b))]
                                      (s-var 'b)))
   `assoc-left         (scheme (fn-of [(tuple-of (s-var 'a) (s-var 'b)) (s-var 'c)]
                                      (tuple-of (s-var 'c) (s-var 'b))))
   `assoc-right        (scheme (fn-of [(tuple-of (s-var 'a) (s-var 'b)) (s-var 'c)]
                                      (tuple-of (s-var 'a) (s-var 'c))))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Set
   '->set1             (scheme (fn-of [(s-var 'e)]
                                      (set-of (s-var 'e))))
   '->set2             (scheme (fn-of [(s-var 'e) (s-var 'e)]
                                      (set-of (s-var 'e))))
   '->set3             (scheme (fn-of [(s-var 'e) (s-var 'e) (s-var 'e)]
                                      (set-of (s-var 'e))))
   `set/union          (scheme (fn-of [(set-of (s-var 'e))
                                       (set-of (s-var 'e))]
                                      (set-of (s-var 'e))))
   `set/difference     (scheme (fn-of [(set-of (s-var 'e))
                                       (set-of (s-var 'e))]
                                      (set-of (s-var 'e))))
   `set/intersection   (scheme (fn-of [(set-of (s-var 'e))
                                       (set-of (s-var 'e))]
                                      (set-of (s-var 'e))))
   `set/subset?        (scheme (fn-of [(set-of (s-var 'e))
                                       (set-of (s-var 'e))]
                                      BOOLEAN))
   `set/superset?      (scheme (fn-of [(set-of (s-var 'e))
                                       (set-of (s-var 'e))]
                                      BOOLEAN))
   'disj               (scheme (fn-of [(set-of (s-var 'e)) (s-var 'e)]
                                      (set-of (s-var 'e))))
   `map-set            (scheme (fn-of [(fn-of [(s-var 'a)] (s-var 'b))
                                       (set-of (s-var 'a))]
                                      (set-of (s-var 'b))))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;; Map
   '->map1             (scheme (fn-of [(s-var 'k) (s-var 'v)]
                                      (map-of (s-var 'k) (s-var 'v))))
   '->map2             (scheme (fn-of [(s-var 'k) (s-var 'v)
                                       (s-var 'k) (s-var 'v)]
                                      (map-of (s-var 'k) (s-var 'v))))
   '->map3             (scheme (fn-of [(s-var 'k) (s-var 'v)
                                       (s-var 'k) (s-var 'v)
                                       (s-var 'k) (s-var 'v)]
                                      (map-of (s-var 'k) (s-var 'v))))
   'get                (scheme (fn-of [(map-of (s-var 'k) (s-var 'v)) (s-var 'k)]
                                      (s-var 'v)))
   'get-or-else        (scheme (fn-of [(map-of (s-var 'k) (s-var 'v)) (s-var 'k) (s-var 'v)]
                                      (s-var 'v)))
   'assoc              (scheme (fn-of [(map-of (s-var 'k) (s-var 'v))
                                       (s-var 'k)
                                       (s-var 'v)]
                                      (map-of (s-var 'k) (s-var 'v))))
   'update             (scheme (fn-of [(map-of (s-var 'k) (s-var 'v))
                                       (s-var 'k)
                                       (fn-of [(s-var 'v)] (s-var 'v))]
                                      (map-of (s-var 'k) (s-var 'v))))
   `keys-vec           (scheme (fn-of [(map-of (s-var 'k) (s-var 'v))]
                                      (vector-of (s-var 'k))))
   `keys-set           (scheme (fn-of [(map-of (s-var 'k) (s-var 'v))]
                                      (set-of (s-var 'k))))
   `vals-vec           (scheme (fn-of [(map-of (s-var 'k) (s-var 'v))]
                                      (vector-of (s-var 'v))))
   'merge              (scheme (fn-of [(map-of (s-var 'k) (s-var 'v))
                                       (map-of (s-var 'k) (s-var 'v))]
                                      (map-of (s-var 'k) (s-var 'v))))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Printing & Side Effects
   'do2                (scheme (fn-of [NIL (s-var 'a)] (s-var 'a)))
   'do3                (scheme (fn-of [NIL NIL (s-var 'a)] (s-var 'a)))
   'print              (scheme (fn-of [(s-var 'a)] NIL))
   'println            (scheme (fn-of [(s-var 'a)] NIL))})

(def dealiases
  '{comp2-fn1         comp
    comp2-fn2         comp
    comp3-fn1         comp
    comp3-fn2         comp 
    partial1-fn2      partial
    partial1-fn3      partial
    partial2-fn3      partial
    
    ->map1            hash-map
    ->map2            hash-map
    ->map3            hash-map
    ->set1            hash-set
    ->set2            hash-set
    ->set3            hash-set
    ->tuple2          vector
    ->vector1         vector
    ->vector2         vector
    ->vector3         vector
    append-str        str
    do2               do
    do3               do
    get-or-else       get
    left              first

    mapv-map          mapv
    mapv-str          mapv
    mapv-vec          mapv
    mapv-set          mapv
    map2v-str         mapv
    map2v-vec         mapv 
    map->set          set
    vec->set          set
    map->vec          vec 
    set->vec          vec
    str->vec          vec
    set->map          erp12.cbgp-lite.lang.lib/->map
    vec->map          erp12.cbgp-lite.lang.lib/->map
    
    concat-str        erp12.cbgp-lite.lang.lib/concat'
    concat-vec           erp12.cbgp-lite.lang.lib/concat'
    conj-set          erp12.cbgp-lite.lang.lib/conj'
    conj-vec          erp12.cbgp-lite.lang.lib/conj'
    
    first-str         first
    first-vec         first
    last-str          last
    last-vec          last
    rest-str          erp12.cbgp-lite.lang.lib/rest'
    rest-vec          erp12.cbgp-lite.lang.lib/rest'
    butlast-str       erp12.cbgp-lite.lang.lib/butlast'
    butlast-vec       erp12.cbgp-lite.lang.lib/butlast'
    
    str/includes?     erp12.cbgp-lite.lang.lib/in?
    char-in?          erp12.cbgp-lite.lang.lib/in?
    in?               erp12.cbgp-lite.lang.lib/in?

    filter-map        erp12.cbgp-lite.lang.lib/filter'
    filter-set        erp12.cbgp-lite.lang.lib/filter'
    filter-str        erp12.cbgp-lite.lang.lib/filter'
    filter-vec        erp12.cbgp-lite.lang.lib/filter'
    remove-map        erp12.cbgp-lite.lang.lib/remove'
    remove-set        erp12.cbgp-lite.lang.lib/remove'
    remove-str        erp12.cbgp-lite.lang.lib/remove'
    remove-vec        erp12.cbgp-lite.lang.lib/remove' 
    remove-char       erp12.cbgp-lite.lang.lib/remove-element
    
    reduce-map        reduce
    reduce-set        reduce
    reduce-vec        reduce
    fold-map          reduce
    fold-set          reduce
    fold-str          reduce
    fold-vec          reduce
    mapcat-str        erp12.cbgp-lite.lang.lib/mapcat'
    mapcat-vec        erp12.cbgp-lite.lang.lib/mapcat'
    
    nth-str           erp12.cbgp-lite.lang.lib/safe-nth
    replace-str        erp12.cbgp-lite.lang.lib/replace'
    replace-char       erp12.cbgp-lite.lang.lib/replace'
    replace-vec        erp12.cbgp-lite.lang.lib/replace'
    replace-first-str        erp12.cbgp-lite.lang.lib/replace-first'
    replace-first-char       erp12.cbgp-lite.lang.lib/replace-first'
    replace-first-vec        erp12.cbgp-lite.lang.lib/replace-first'
    take-str              erp12.cbgp-lite.lang.lib/take'
    take-vec              erp12.cbgp-lite.lang.lib/take'
    reverse-str              erp12.cbgp-lite.lang.lib/reverse'
    reverse-vec              erp12.cbgp-lite.lang.lib/reverse'
    sort-str              erp12.cbgp-lite.lang.lib/sort'
    sort-vec              erp12.cbgp-lite.lang.lib/sort'
    safe-sub-str          erp12.cbgp-lite.lang.lib/safe-sub-coll
    safe-sub-vec          erp12.cbgp-lite.lang.lib/safe-sub-coll

    nth-or-else       nth
    range1            erp12.cbgp-lite.lang.lib/rangev
    range2            erp12.cbgp-lite.lang.lib/rangev
    range3            erp12.cbgp-lite.lang.lib/rangev
    right             second
    split-str-on-char erp12.cbgp-lite.lang.lib/split-str
    str-join-sep      clojure.string/join 
    char-occurrences-of  erp12.cbgp-lite.lang.lib/occurrences-of})

(def macros
  #{'if 'do2 'do3})

(defn check-type-for-type-ctors
  "Checks if all of typ's :types are in type-ctors"
  [typ type-ctors]
  (let [schema-types (->> (schema/schema-terms typ)
                          (remove #{:cat :s-var :scheme}))
        schema-typeclass-types (filter set? schema-types)
        all-typeclass-types-valid (empty?
                                   (remove #(not (empty? (set/intersection type-ctors %)))
                                           schema-typeclass-types))
        schema-static-types (set (remove set? schema-types))
        schema-static-types-valid (set/superset? type-ctors schema-static-types)]
    (and schema-static-types-valid all-typeclass-types-valid)))

(defn lib-for-type-ctors
  "Filters type-env to include all functions that have all of their types in type-ctors.
   Run on each alternative if overloaded; if any return true, include this instruction"
  [type-ctors]
  (->> type-env
       (filter (fn [[_ typ]]
                 (if (not= :overloaded (:type typ))
                   (check-type-for-type-ctors typ type-ctors)
                   (not (empty? (filter #(check-type-for-type-ctors % type-ctors)
                                        (:alternatives typ)))))))
       (into {})))

(comment
  (count (keys (lib-for-type-ctors #{boolean? :tuple int? :vector :=> :map-of :t-var :set})))
  (count (keys (lib-for-type-ctors #{boolean? :tuple int? :vector :=> :map-of :set})))

  (let [input3 "t"]
    (erp12.cbgp-lite.lang.lib/conj'
     (last (erp12.cbgp-lite.lang.lib/rest' (mapv hash-set input3)))
     input3))
  (let [input3 "t"]
    (erp12.cbgp-lite.lang.lib/conj'
     (last (erp12.cbgp-lite.lang.lib/rest' (mapv hash-set '("t" "p" "l"))))
     input3))
  (let [input3 "t"]
    (last (erp12.cbgp-lite.lang.lib/rest' (mapv hash-set input3))))

  (rest' (mapv hash-set "t"))

  (let [input1 #{"T:^,\\3_M]"
                 "1`"
                 "T"
                 "u~'"
                 "UY$DO&i@"
                 ";_&"
                 "X6"
                 "D\nM"
                 "\\i?$?"
                 "UTn"
                 "Mz"
                 "af"
                 "3YCcy"
                 "<9t5"
                 "y`wtdvW"
                 ":fXC1D2["
                 "~^"
                 "-mV,"
                 "?,JBCw"
                 ":("
                 "w6s?wbI}\t"}
        input2 "`1,f"
        input3 "h@b~\""]
    (erp12.cbgp-lite.lang.lib/conj'
     (first (erp12.cbgp-lite.lang.lib/rest'
             (let [v-5521793 (erp12.cbgp-lite.lang.lib/butlast' (erp12.cbgp-lite.lang.lib/keys-vec
                                                                 (hash-map input1 input2 input1
                                                                           (erp12.cbgp-lite.lang.lib/in? input3 (erp12.cbgp-lite.lang.lib/max' input2 input3)))))]
               v-5521793)))
     input3))

  (let [input1 #{"T:^,\\3_M]"
                 "1`"
                 "T"
                 "u~'"
                 "UY$DO&i@"
                 ";_&"
                 "X6"
                 "D\nM"
                 "\\i?$?"
                 "UTn"
                 "Mz"
                 "af"
                 "3YCcy"
                 "<9t5"
                 "y`wtdvW"
                 ":fXC1D2["
                 "~^"
                 "-mV,"
                 "?,JBCw"
                 ":("
                 "w6s?wbI}\t"}
        input2 "`1,f"
        input3 "h@b~\""]
    (first (rest' (butlast' (keys-vec (hash-map input1 input2 input1
                                                (erp12.cbgp-lite.lang.lib/in? input3 (erp12.cbgp-lite.lang.lib/max' input2 input3))))))))
  )
               
  