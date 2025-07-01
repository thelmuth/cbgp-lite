(ns erp12.cbgp-lite.lang.lib
  (:refer-clojure :exclude [and or vector-of])
  (:require [clojure.core :as core]
            [clojure.set :as set]
            [clojure.string :as str]
            [erp12.cbgp-lite.lang.schema :as schema]
            [clojure.string :as string]))

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
      (throw (ex-info "Pow resulting in undefined value." {:base x :exponent y}))
      
      (and (integer? x) (integer? y))
      (long result)
      
      :else
      result)))

;; (defn int-pow
;;   [x y]
;;   (long (safe-pow x y)))

;; (defn double-pow
;;   [x y]
;;   (safe-pow x y))

(defn square
  [x]
  (pow x 2))

;; (defn int-square
;;   [x]
;;   (long (safe-pow x 2)))

;; (defn double-square
;;   [x]
;;   (safe-pow x 2))

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
;; (def take-str (comp str/join take))
;; (def rest-str (comp str/join rest))
(def butlast-str (comp str/join butlast))
(def filter-str (comp str/join filter))

;; (defn char-in? [s c] (str/includes? s (str c)))

(defn char-occurrences
  [s c]
  (count (filter #{c} s)))

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

(defn str-sort
  [s]
  (str/join (sort s)))

(defn split-str
  [s on]
  (str/split s (str-to-pattern on)))

(defn split-str-on-ws
  [s]
  (str/split (str/trim s) #"\s+"))

(defn replace-char
  [s c1 c2]
  (str/replace s (str c1) (str c2)))

(defn replace-first-char
  [s c1 c2]
  (str/replace-first s (str c1) (str c2)))

;; (defn remove-char
;;   [s c]
;;   (apply str (remove #{c} s)))

(defn set-char
  [s idx c]
  (if (empty? s)
    s
    (let [safe-idx (mod idx (count s))]
      (apply str (assoc (vec s) safe-idx c)))))

(defn safe-subs
  [s start end]
  (let [start (min (count s) (max 0 start))
        end (min (count s) (max start end))]
    (subs s start end)))

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


;; (defn mapcat'
;;   [pred coll]
;;   (let [mapcated (mapcat pred coll)]
;;     (if (string? coll)
;;       (apply str mapcated)
;;       (into (empty coll) mapcated))))

; [!] may not work
(defn conj'
  [coll target]
  (if (set? coll)
    ((comp set conj) coll target)
    ((comp vec conj) coll target)))

(defn concat'
  [coll1 coll2]
  (if (string? coll1)
    (reduce str (concat coll1 coll2))
    ((comp vec concat) coll1 coll2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vector

;; (def conj-vec (comp vec conj))
(def distinctv (comp vec distinct))
(def mapcatv (comp vec mapcat))
(def mapv-indexed (comp vec map-indexed))
(def removev (comp vec remove))
(def concatv (comp vec concat))
;; (def takev (comp vec take))
;; (def restv (comp vec rest))
(def butlastv (comp vec butlast))
(def reversev (comp vec reverse))
(def sortv (comp vec sort))
(def sortv-by (comp vec sort-by))

(def rangev
  ;; Cap the range to avoid memory errors.
  (comp vec #(take 100 %) range))

;; (defn index-of
;;   [coll el]
;;   (.indexOf coll el))

(defn index-of
  [coll el]
  (if (string? coll)
    (str/index-of coll (str el))
    (.indexOf coll el)))

(defn occurrences-of
  [coll el]
  (count (filter #{el} coll)))

;; (defn in?
;;   [coll el]
;;   (<= 0 (.indexOf coll el)))

; merged in? and char-in?
(defn in?
  [coll el]
  (if (string? coll)
    (str/includes? coll (str el))
    (<= 0 (.indexOf coll el))))

;; (defn replacev
;;   [vtr to-replace replace-with]
;;   (replace {to-replace replace-with} vtr))

;; (defn replacev-first
;;   [vtr to-replace replace-with]
;;   (let [idx (.indexOf vtr to-replace)]
;;     (if (< idx 0)
;;       vtr
;;       (assoc vtr idx replace-with))))

;; (defn remove-element
;;   [vtr el]
;;   (vec (remove #{el} vtr)))

; new remove-element, works w/ chars too
(defn remove-element
  [coll element]
  (let [removed (remove #{element} coll)]
    (if (string? coll)
      (apply str removed)
      (vec removed))))

(defn safe-subvec
  [vtr start end]
  (let [start (min (count vtr) (max 0 start))
        end (min (count vtr) (max start end))]
    (subvec vtr start end)))

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

;;New safe-sub
(defn safe-sub
  [coll start end]
  (if (string? coll)
    (safe-subs coll start end)
    (safe-subvec coll start end)))

(safe-sub "Hamilton" 0 3)
(safe-sub [1 2 3 4] 0 3)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fixing LazySeqs

(defn rest'
  [coll]
  (if (string? coll)
    (reduce str (rest coll))
    ((comp vec rest) coll)))

(defn butlast'
  [coll]
  (if (string? coll)
    (reduce str (butlast coll))
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
    (reduce str (take num coll))
    (take num coll)))

(defn reverse'
  [coll]
  (if (string? coll)
    (reduce str (reverse coll))
    (reverse coll)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set

;; (def conj-set (comp set conj))
(defn map-set [f s] (into #{} (map f s)))
(defn filter-set [pred s] (into #{} (filter pred s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map

(defn ->map
  [coll]
  (into {} coll))

(defn filter-map [pred m] (into {} (filter pred m)))
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
   'comp                  #_{:type :overloaded
                           :alternatives [(scheme (fn-of [(fn-of [(s-var 'b)] (s-var 'c))
                                                          (fn-of [(s-var 'a)] (s-var 'b))]
                                                         (fn-of [(s-var 'a)] (s-var 'c))))
                                          (scheme (fn-of [(fn-of [(s-var 'c)] (s-var 'd))
                                                          (fn-of [(s-var 'b)] (s-var 'c))
                                                          (fn-of [(s-var 'a)] (s-var 'b))]
                                                         (fn-of [(s-var 'a)] (s-var 'd))))
                                          (scheme (fn-of [(fn-of [(s-var 'c)] (s-var 'd))
                                                          (fn-of [(s-var 'a) (s-var 'b)] (s-var 'c))]
                                                         (fn-of [(s-var 'a) (s-var 'b)] (s-var 'd))))
                                          (scheme (fn-of [(fn-of [(s-var 'd)] (s-var 'e))
                                                          (fn-of [(s-var 'c)] (s-var 'd))
                                                          (fn-of [(s-var 'a) (s-var 'b)] (s-var 'c))]
                                                         (fn-of [(s-var 'a) (s-var 'b)] (s-var 'e))))
                                          ]} ; common -> rare order
                          {:type :overloaded
                           :alternatives [(scheme (fn-of [(fn-of [(s-var 'd)] (s-var 'e))
                                                          (fn-of [(s-var 'c)] (s-var 'd))
                                                          (fn-of [(s-var 'a) (s-var 'b)] (s-var 'c))]
                                                         (fn-of [(s-var 'a) (s-var 'b)] (s-var 'e))))
                                          (scheme (fn-of [(fn-of [(s-var 'c)] (s-var 'd))
                                                          (fn-of [(s-var 'a) (s-var 'b)] (s-var 'c))]
                                                         (fn-of [(s-var 'a) (s-var 'b)] (s-var 'd))))
                                          (scheme (fn-of [(fn-of [(s-var 'c)] (s-var 'd))
                                                          (fn-of [(s-var 'b)] (s-var 'c))
                                                          (fn-of [(s-var 'a)] (s-var 'b))]
                                                         (fn-of [(s-var 'a)] (s-var 'd))))
                                          #_(scheme (fn-of [(fn-of [(s-var 'b)] (s-var 'c))
                                                          (fn-of [(s-var 'a)] (s-var 'b))]
                                                         (fn-of [(s-var 'a)] (s-var 'c))))
                                          ]}
  ;;  'comp2-fn1          (scheme (fn-of [(fn-of [(s-var 'b)] (s-var 'c))
  ;;                                      (fn-of [(s-var 'a)] (s-var 'b))]
  ;;                                     (fn-of [(s-var 'a)] (s-var 'c))))
  ;;  'comp3-fn1          (scheme (fn-of [(fn-of [(s-var 'c)] (s-var 'd))
  ;;                                      (fn-of [(s-var 'b)] (s-var 'c))
  ;;                                      (fn-of [(s-var 'a)] (s-var 'b))]
  ;;                                     (fn-of [(s-var 'a)] (s-var 'd))))
  ;;  'comp2-fn2          (scheme (fn-of [(fn-of [(s-var 'c)] (s-var 'd))
  ;;                                      (fn-of [(s-var 'a) (s-var 'b)] (s-var 'c))]
  ;;                                     (fn-of [(s-var 'a) (s-var 'b)] (s-var 'd))))
  ;;  'comp3-fn2          (scheme (fn-of [(fn-of [(s-var 'd)] (s-var 'e))
  ;;                                      (fn-of [(s-var 'c)] (s-var 'd))
  ;;                                      (fn-of [(s-var 'a) (s-var 'b)] (s-var 'c))]
  ;;                                     (fn-of [(s-var 'a) (s-var 'b)] (s-var 'e))))
   'partial                  {:type :overloaded
                              :alternatives [(scheme (fn-of [(fn-of [(s-var 'a) (s-var 'b) (s-var 'c)] (s-var 'd))
                                                             (s-var 'a)
                                                             (s-var 'b)]
                                                            (fn-of [(s-var 'c)] (s-var 'd))))
                                             (scheme (fn-of [(fn-of [(s-var 'a) (s-var 'b) (s-var 'c)] (s-var 'd))
                                                             (s-var 'a)]
                                                            (fn-of [(s-var 'b) (s-var 'c)] (s-var 'd))))
                                             (scheme (fn-of [(fn-of [(s-var 'a) (s-var 'b)] (s-var 'c))
                                                             (s-var 'a)]
                                                            (fn-of [(s-var 'b)] (s-var 'c))))
                                             ]}
  ;;  'partial1-fn2       (scheme (fn-of [(fn-of [(s-var 'a) (s-var 'b)] (s-var 'c))
  ;;                                      (s-var 'a)]
  ;;                                     (fn-of [(s-var 'b)] (s-var 'c))))
  ;;  'partial1-fn3       (scheme (fn-of [(fn-of [(s-var 'a) (s-var 'b) (s-var 'c)] (s-var 'd))
  ;;                                      (s-var 'a)]
  ;;                                     (fn-of [(s-var 'b) (s-var 'c)] (s-var 'd))))
  ;;  'partial2-fn3       (scheme (fn-of [(fn-of [(s-var 'a) (s-var 'b) (s-var 'c)] (s-var 'd))
  ;;                                      (s-var 'a)
  ;;                                      (s-var 'b)]
  ;;                                     (fn-of [(s-var 'c)] (s-var 'd))))
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
   'quot               (scheme (fn-of [(s-var 'a) (s-var 'a)] (s-var 'a)) {'a #{:number}})
   '/                  (scheme (fn-of [(s-var 'a) (s-var 'a)] DOUBLE) {'a #{:number}})
   'mod                (scheme (fn-of [(s-var 'a) (s-var 'a)] (s-var 'a)) {'a #{:number}})
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
   'str                (scheme (fn-of [(s-var 't)] STRING))
   `int->char          (fn-of [INT] CHAR)
   `whitespace?        (unary-pred CHAR)
   `digit?             (unary-pred CHAR)
   `letter?            (unary-pred CHAR)
  ;;  `concat-str         (fn-of [STRING STRING] STRING)
   'append-str         (fn-of [STRING CHAR] STRING)
  ;;  `take-str           (fn-of [INT STRING] STRING)
  ;;  `safe-subs          (fn-of [STRING INT INT] STRING)
  ;;  `filter-str         (fn-of [(fn-of [CHAR] BOOLEAN) STRING] STRING)
  ;;  'first-str          (fn-of [STRING] CHAR)
  ;;  'last-str           (fn-of [STRING] CHAR)
  ;;  `rest-str           (unary-transform STRING)
  ;;  `butlast-str        (unary-transform STRING)
  ;;  'nth-str            (fn-of [STRING INT] CHAR)
  ;;  'length             (fn-of [STRING] INT)
  ;;  'map-str            {:type   :scheme
  ;;                       :s-vars ['a]
  ;;                       :body   (fn-of [(fn-of [CHAR] (s-var 'a))
  ;;                                       STRING]
  ;;                                      (vector-of (s-var 'a)))}
  ;;  'mapcat-str         {:type   :scheme
  ;;                       :s-vars ['a]
  ;;                       :body   (fn-of [(fn-of [CHAR] (vector-of (s-var 'a)))
  ;;                                       STRING]
  ;;                                      (vector-of (s-var 'a)))}
  ;;  `str/reverse        (unary-transform STRING)
  ;;  'string->chars      (fn-of [STRING] (vector-of CHAR)) ; see 'vec
   `split-str          (fn-of [STRING STRING] (vector-of STRING))
   'split-str-on-char  (fn-of [STRING CHAR] (vector-of STRING))
   `split-str-on-ws    (fn-of [STRING] (vector-of STRING))
  ;;  'empty-str?         (unary-pred STRING)
  ;;  `str/includes?      (binary-pred STRING)
  ;;  `char-in?           (fn-of [STRING CHAR] BOOLEAN)
  ;;  'index-of-char      (fn-of [STRING CHAR] INT)
  ;;  'index-of-str       (fn-of [STRING STRING] INT)
   'char-occurrences   (fn-of [STRING CHAR] INT)
  ;;  `str/replace        (fn-of [STRING STRING STRING] STRING)
  ;;  `str/replace-first  (fn-of [STRING STRING STRING] STRING)
  ;;  `replace-char       (fn-of [STRING CHAR CHAR] STRING)
  ;;  `replace-first-char (fn-of [STRING CHAR CHAR] STRING)
  ;;  `remove-char        (fn-of [STRING CHAR] STRING)
   `set-char           (fn-of [STRING INT CHAR] STRING)
   `str/join           (fn-of [(vector-of STRING)] STRING)
   'str-join-sep       (fn-of [STRING (vector-of STRING)] STRING)
   'join-chars         (fn-of [(vector-of CHAR)] STRING)
   `str/capitalize     (unary-transform STRING)
   `str/upper-case     (unary-transform STRING)
   `str/lower-case     (unary-transform STRING)
  ;;  `str-sort           (unary-transform STRING)
   `char-upper         (unary-transform CHAR)
   `char-lower         (unary-transform CHAR)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Boolean
   `and                (binary-transform BOOLEAN)
   `or                 (binary-transform BOOLEAN)
   'not                (unary-transform BOOLEAN)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Polymorphic collection functions 
   ; [!] to do: check alternative scheme priority
   'count              (scheme (fn-of [{:type (s-var 'c)}] INT)
                               {'c #{:countable}})
   'mapv               {:type :overloaded
                        :alternatives [(scheme (fn-of [(fn-of [(s-var 'a)] (s-var 'b))
                                                       (vector-of (s-var 'a))]
                                                      (vector-of (s-var 'b)))) ; map-vec 
                                       (scheme (fn-of [(fn-of [CHAR] (s-var 'a))
                                                       STRING]
                                                      (vector-of (s-var 'a)))) ; map-str      
                                       (scheme (fn-of [(fn-of [(s-var 'a)] (s-var 'b))
                                                       (set-of (s-var 'a))]
                                                      (vector-of (s-var 'b)))) ; map-set? [!] to-do: make map-set return a vec
                                       (scheme (fn-of [(fn-of [(tuple-of (s-var 'k) (s-var 'v))] (s-var 'e))
                                                       (map-of (s-var 'k) (s-var 'v))]
                                                      (vector-of (s-var 'e)))) ; map-map
                                       ]}
   ; [!] above, possibly make typeclassed?   
   'map2v              {:type :overloaded
                        :alternatives [(scheme (fn-of [(fn-of [CHAR CHAR] (s-var 'a))
                                                       STRING
                                                       STRING]
                                                      (vector-of (s-var 'a)))) ; str 
                                       (scheme (fn-of [(fn-of [(s-var 'a1) (s-var 'a2)] (s-var 'b))
                                                       (vector-of (s-var 'a1))
                                                       (vector-of (s-var 'a2))]
                                                      (vector-of (s-var 'b)))) ; vec
                                       ]}
   'vec                {:type :overloaded
                        :alternatives [(scheme (fn-of [(map-of (s-var 'k) (s-var 'v))] (vector-of (tuple-of (s-var 'k) (s-var 'v)))))
                                       (scheme (fn-of [(set-of (s-var 'e))] (vector-of (s-var 'e))))
                                       (scheme (fn-of [STRING] (vector-of CHAR)))]}
   'set                {:type :overloaded
                        :alternatives [(scheme (fn-of [(vector-of (s-var 'e))] (set-of (s-var 'e))))
                                       (scheme (fn-of [(map-of (s-var 'k) (s-var 'v))] (set-of (tuple-of (s-var 'k) (s-var 'v)))))]}
   `->map              {:type :overloaded
                        :alternatives [(scheme (fn-of [(vector-of (tuple-of (s-var 'k) (s-var 'v)))] (map-of (s-var 'k) (s-var 'v))))
                                       (scheme (fn-of [(set-of (tuple-of (s-var 'k) (s-var 'v)))] (map-of (s-var 'k) (s-var 'v))))]}
   `concat'             {:type :overloaded
                         :alternatives [(scheme (binary-transform (vector-of (s-var 'a))))
                                        (fn-of [STRING STRING] STRING)]} ; double-check
   `conj'              {:type :overloaded
                        :alternatives [(scheme (fn-of [(vector-of (s-var 'a)) (s-var 'a)] (vector-of (s-var 'a)))) ; conj-vec
                                       (scheme (fn-of [(set-of (s-var 'e)) (s-var 'e)]
                                                      (set-of (s-var 'e)))) ; conj-set
                                       ]}
   'first              {:type :overloaded ;;; where does indexable go?
                           ;:typeclasses #{:indexable}
                        :alternatives [(scheme (fn-of [(vector-of (s-var 'a))] (s-var 'a)))
                                       (fn-of [STRING] CHAR)]}
   'last               {:type :overloaded
                        :alternatives [(scheme (fn-of [(vector-of (s-var 'a))] (s-var 'a)))
                                       (fn-of [STRING] CHAR)]}
   `rest'               {:type :overloaded
                        :alternatives [(scheme (fn-of [(vector-of (s-var 'a))] (vector-of (s-var 'a))))
                                       (unary-transform STRING)]}
   `butlast'            {:type :overloaded
                        :alternatives [(scheme (fn-of [(vector-of (s-var 'a))] (vector-of (s-var 'a))))
                                       (unary-transform STRING)]}
   'empty?             (scheme (fn-of [(s-var 'a)] BOOLEAN) {'a #{:countable}})
   `in?                {:type :overloaded
                        :alternatives [(scheme (fn-of [(vector-of (s-var 'a)) (s-var 'a)] BOOLEAN))
                                       (fn-of [STRING CHAR] BOOLEAN)
                                       (binary-pred STRING)]}
   `index-of           (scheme (fn-of [(s-var 'c) (s-var 'a)] INT) {'c #{:indexable}}) 
   'contains?          (scheme (fn-of [(s-var 'c) (s-var 'a)] BOOLEAN) {'c #{:keyable}})
   `filter'             {:type :overloaded
                         :alternatives [(scheme (fn-of [(fn-of [(s-var 'a)] BOOLEAN)
                                                        (vector-of (s-var 'a))]
                                                       (vector-of (s-var 'a)))) ; filter-vec
                                        (scheme (fn-of [(fn-of [(s-var 'a)] BOOLEAN)
                                                        (set-of (s-var 'a))]
                                                       (set-of (s-var 'a)))) ; filter-set
                                        (scheme (fn-of [(fn-of [(tuple-of (s-var 'k) (s-var 'v))] BOOLEAN)
                                                        (map-of (s-var 'k) (s-var 'v))]
                                                       (map-of (s-var 'k) (s-var 'v)))) ; filter-map
                                        (fn-of [(fn-of [CHAR] BOOLEAN) STRING] STRING) ; filter-str
                                        ]}
   `remove'           {:type :overloaded
                       :alternatives [(scheme (fn-of [(fn-of [(s-var 'a)] BOOLEAN)
                                                      (vector-of (s-var 'a))]
                                                     (vector-of (s-var 'a)))) ; remove-vec
                                      (scheme (fn-of [(fn-of [(s-var 'a)] BOOLEAN)
                                                      (set-of (s-var 'a))]
                                                     (set-of (s-var 'a)))) ; remove-set (new)
                                      (scheme (fn-of [(fn-of [(tuple-of (s-var 'k) (s-var 'v))] BOOLEAN)
                                                      (map-of (s-var 'k) (s-var 'v))]
                                                     (map-of (s-var 'k) (s-var 'v)))) ; remove-map (new)
                                      (fn-of [(fn-of [CHAR] BOOLEAN) STRING] STRING) ; remove-str (new)
                                      ]}
   `remove-element    {:type :overloaded
                       :alternatives [(scheme (fn-of [(vector-of (s-var 'a)) (s-var 'a)]
                                                     (vector-of (s-var 'a))))
                                      (fn-of [STRING CHAR] STRING) ; remove-char 
                                      ]}
   'reduce             {:type :overloaded
                        :alternatives [(scheme (fn-of [(fn-of [(s-var 'a) (s-var 'a)] (s-var 'a))
                                                       (vector-of (s-var 'a))]
                                                      (s-var 'a))) ; reduce-vec
                                       (scheme (fn-of [(fn-of [(s-var 'a) (s-var 'a)] (s-var 'a))
                                                       (set-of (s-var 'a))]
                                                      (s-var 'a)))
                                       (scheme (fn-of [(fn-of [(tuple-of (s-var 'k) (s-var 'v))
                                                               (tuple-of (s-var 'k) (s-var 'v))]
                                                              (tuple-of (s-var 'k) (s-var 'v)))
                                                       (map-of (s-var 'k) (s-var 'v))]
                                                      (tuple-of (s-var 'k) (s-var 'v)))) ; reduce-map
                                       (scheme (fn-of [(fn-of [CHAR CHAR] CHAR)
                                                       STRING]
                                                      CHAR)) ; reduce-str
                                       ]}
   'fold               {:type :overloaded
                        :alternatives [(scheme (fn-of [(fn-of [(s-var 'b) (s-var 'a)] (s-var 'b))
                                                       (s-var 'b)
                                                       (vector-of (s-var 'a))]
                                                      (s-var 'b))) ; fold-vec
                                       (scheme (fn-of [(fn-of [(s-var 'b) (s-var 'a)] (s-var 'b))
                                                       (s-var 'b)
                                                       (set-of (s-var 'a))]
                                                      (s-var 'b))) ; fold-set
                                       (scheme (fn-of [(fn-of [(s-var 'r)
                                                               (tuple-of (s-var 'k) (s-var 'v))]
                                                              (s-var 'r))
                                                       (s-var 'r)
                                                       (map-of (s-var 'k) (s-var 'v))]
                                                      (s-var 'r))) ; fold-map
                                       (scheme (fn-of [(fn-of [(s-var 'a) CHAR] (s-var 'a))
                                                       (s-var 'a)
                                                       STRING]
                                                      (s-var 'a))) ; fold-str
                                       ]}
   `mapcat'            {:type :overloaded
                       :alternatives [(scheme (fn-of [(fn-of [(s-var 'a)] (vector-of (s-var 'b)))
                                                      (vector-of (s-var 'a))]
                                                     (vector-of (s-var 'b))))
                                      (scheme (fn-of [(fn-of [CHAR] (vector-of (s-var 'a)))
                                                      STRING]
                                                     (vector-of (s-var 'a))))]}

;;;;;;; string-vec functions; organize later
   `safe-nth          {:type :overloaded
                       :alternatives [(scheme (fn-of [(vector-of (s-var 'a)) INT] (s-var 'a))) ; safe-nth
                                      (fn-of [STRING INT] CHAR) ; nth-str 
                                      ]}
   `replace'          {:type :overloaded
                       :alternatives [(scheme (fn-of [(vector-of (s-var 'a))
                                                      (s-var 'a)
                                                      (s-var 'a)]
                                                     (vector-of (s-var 'a)))) ; vec
                                      (fn-of [STRING CHAR CHAR] STRING) ; char
                                      (fn-of [STRING STRING STRING] STRING) ; str
                                      ]} ; [!] to-do - test
   `replace-first'    {:type :overloaded
                       :alternatives [(scheme (fn-of [(vector-of (s-var 'a))
                                                      (s-var 'a)
                                                      (s-var 'a)]
                                                     (vector-of (s-var 'a))))
                                      (fn-of [STRING CHAR CHAR] STRING) ; char
                                      (fn-of [STRING STRING STRING] STRING) ; str
                                      ]} ; [!] to-do - test
   `take'             {:type :overloaded
                       :alternatives [
                                      (fn-of [INT STRING] STRING) ; take-str
                                      (scheme (fn-of [INT (vector-of (s-var 'a))]
                                                     (vector-of (s-var 'a)))) ; take-vec
                                      ]} ; [!] to-do - make func
   `reverse'          {:type :overloaded
                       :alternatives [
                                      (scheme (fn-of [(vector-of (s-var 'a))] (vector-of (s-var 'a)))) ; reversev
                                      (unary-transform STRING) ; str/reverse
                       ]} ; [!] to-do - make func
   `sort'             {:type :overloaded
                       :alternatives [
                                      (scheme (fn-of [(vector-of (s-var 'e))]
                                                     (vector-of (s-var 'e)))) ; sortv
                                      (unary-transform STRING)
                       ]}  
   `safe-sub          {:type :overloaded
                       :alternatives [(fn-of [STRING INT INT] STRING) ; safe-subs
                                      (scheme (fn-of [(vector-of (s-var 'a)) INT INT]
                                                     (vector-of (s-var 'a)))) ; safe-sub-vec
                        
                       ]} ; [!] to-do - make func                           
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Vector
   '->vector1          (scheme (fn-of [(s-var 'a)]
                                      (vector-of (s-var 'a))))
   '->vector2          (scheme (fn-of [(s-var 'a) (s-var 'a)]
                                      (vector-of (s-var 'a))))
   '->vector3          (scheme (fn-of [(s-var 'a) (s-var 'a) (s-var 'a)]
                                      (vector-of (s-var 'a))))
  ;;  'map->vec           (scheme (fn-of [(map-of (s-var 'k) (s-var 'v))]
  ;;                                     (vector-of (tuple-of (s-var 'k) (s-var 'v)))))
  ;;  'set->vec           (scheme (fn-of [(set-of (s-var 'e))] (vector-of (s-var 'e)))) 
  ;;  `concatv            {:type   :scheme
  ;;                       :s-vars ['a]
  ;;                       :body   (binary-transform (vector-of (s-var 'a)))}
  ;;  `conj-vec           {:type   :scheme
  ;;                       :s-vars ['a]
  ;;                       :body   (fn-of [(vector-of (s-var 'a)) (s-var 'a)] (vector-of (s-var 'a)))}
  ;;  `takev              {:type   :scheme
  ;;                       :s-vars ['a]
  ;;                       :body   (fn-of [INT (vector-of (s-var 'a))]
  ;;                                      (vector-of (s-var 'a)))}
  ;;  `safe-subvec        {:type   :scheme
  ;;                       :s-vars ['a]
  ;;                       :body   (fn-of [(vector-of (s-var 'a)) INT INT]
  ;;                                      (vector-of (s-var 'a)))}
  ;;  'last               {:type   :scheme
  ;;                       :s-vars ['a]
  ;;                       :body   (fn-of [(vector-of (s-var 'a))] (s-var 'a))}                                    
  ;;  `restv              {:type   :scheme
  ;;                       :s-vars ['a]
  ;;                       :body   (fn-of [(vector-of (s-var 'a))] (vector-of (s-var 'a)))}
  ;;  `butlastv           {:type   :scheme
  ;;                       :s-vars ['a]
  ;;                       :body   (fn-of [(vector-of (s-var 'a))] (vector-of (s-var 'a)))}
  ;;  `safe-nth           {:type   :scheme
  ;;                       :s-vars ['a]
  ;;                       :body   (fn-of [(vector-of (s-var 'a)) INT] (s-var 'a))}
   'nth-or-else        (scheme (fn-of [(vector-of (s-var 'a)) INT (s-var 'a)] (s-var 'a)))
  ;;  `reversev           {:type   :scheme
  ;;                       :s-vars ['a]
  ;;                       :body   (fn-of [(vector-of (s-var 'a))] (vector-of (s-var 'a)))}
  ;;  'empty?             {:type   :scheme
  ;;                       :s-vars ['a]
  ;;                       :body   (fn-of [(vector-of (s-var 'a))] BOOLEAN)}
  ;;  `in?                {:type   :scheme
  ;;                       :s-vars ['a]
  ;;                       :body   (fn-of [(vector-of (s-var 'a)) (s-var 'a)] BOOLEAN)}
  ;;  `index-of           {:type   :scheme
  ;;                       :s-vars ['a]
  ;;                       :body   (fn-of [(vector-of (s-var 'a)) (s-var 'a)] INT)}
   `occurrences-of     (scheme (fn-of [(vector-of (s-var 'a)) (s-var 'a)] INT))
   `safe-assoc-nth     (scheme (fn-of [(vector-of (s-var 'a)) INT (s-var 'a)]
                                      (vector-of (s-var 'a))))
  ;;  `replacev           {:type   :scheme
  ;;                       :s-vars ['a]
  ;;                       :body   (fn-of [(vector-of (s-var 'a))
  ;;                                       (s-var 'a)
  ;;                                       (s-var 'a)]
  ;;                                      (vector-of (s-var 'a)))}
  ;;  `replacev-first     {:type   :scheme
  ;;                       :s-vars ['a]
  ;;                       :body   (fn-of [(vector-of (s-var 'a))
  ;;                                       (s-var 'a)
  ;;                                       (s-var 'a)]
  ;;                                      (vector-of (s-var 'a)))}
  ;;  `remove-element     {:type   :scheme
  ;;                       :s-vars ['a]
  ;;                       :body   (fn-of [(vector-of (s-var 'a)) (s-var 'a)]
  ;;                                      (vector-of (s-var 'a)))}
   'range1             (scheme (fn-of [INT] (vector-of INT)))
   'range2             (scheme (fn-of [INT INT] (vector-of INT)))
   'range3             (scheme (fn-of [INT INT INT] (vector-of INT)))
  ;;  'count-vec          (scheme (fn-of [(vector-of (s-var 'a))] INT))
  ;;  'map-vec            {:type   :scheme
  ;;                       :s-vars ['a 'b]
  ;;                       :body   (fn-of [(fn-of [(s-var 'a)] (s-var 'b))
  ;;                                       (vector-of (s-var 'a))]
  ;;                                      (vector-of (s-var 'b)))}
  ;;  'map2-vec           {:type   :scheme
  ;;                       :s-vars ['a1 'a2 'b]
  ;;                       :body   (fn-of [(fn-of [(s-var 'a1) (s-var 'a2)] (s-var 'b))
  ;;                                       (vector-of (s-var 'a1))
  ;;                                       (vector-of (s-var 'a2))]
  ;;                                      (vector-of (s-var 'b)))}
   `mapv-indexed       (scheme (fn-of [(fn-of [INT (s-var 'a)] (s-var 'b))
                                       (vector-of (s-var 'a))]
                                      (vector-of (s-var 'b))))
  ;;  'filterv            {:type   :scheme
  ;;                       :s-vars ['a]
  ;;                       :body   (fn-of [(fn-of [(s-var 'a)] BOOLEAN)
  ;;                                       (vector-of (s-var 'a))]
  ;;                                      (vector-of (s-var 'a)))}
  ;;  'reduce-vec         {:type   :scheme
  ;;                       :s-vars ['a]
  ;;                       :body   (fn-of [(fn-of [(s-var 'a) (s-var 'a)] (s-var 'a))
  ;;                                       (vector-of (s-var 'a))]
  ;;                                      (s-var 'a))}
  ;;  'fold-vec           {:type   :scheme
  ;;                       :s-vars ['a 'b]
  ;;                       :body   (fn-of [(fn-of [(s-var 'b) (s-var 'a)] (s-var 'b))
  ;;                                       (s-var 'b)
  ;;                                       (vector-of (s-var 'a))]
  ;;                                      (s-var 'b))}
   
  ;;  `removev            {:type   :scheme
  ;;                       :s-vars ['a]
  ;;                       :body   (fn-of [(fn-of [(s-var 'a)] BOOLEAN)
  ;;                                       (vector-of (s-var 'a))]
  ;;                                      (vector-of (s-var 'a)))}
  ;;  `mapcatv            {:type   :scheme
  ;;                       :s-vars ['a 'b]
  ;;                       :body   (fn-of [(fn-of [(s-var 'a)] (vector-of (s-var 'b)))
  ;;                                       (vector-of (s-var 'a))]
  ;;                                      (vector-of (s-var 'b)))}
   
   `distinctv          (scheme (fn-of [(vector-of (s-var 'e))]
                                      (vector-of (s-var 'e))))
  ;;  `sortv              (scheme (fn-of [(vector-of (s-var 'e))]
  ;;                                     (vector-of (s-var 'e))))
   `sortv-by           (scheme (fn-of [(fn-of [(s-var 'e)] (s-var 'k))
                                       (vector-of (s-var 'e))]
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
  ;;  'vec->set           (scheme (fn-of [(vector-of (s-var 'e))]
  ;;                                     (set-of (s-var 'e))))
  ;;  'map->set           (scheme (fn-of [(map-of (s-var 'k) (s-var 'v))]
  ;;                                     (set-of (tuple-of (s-var 'k) (s-var 'v)))))
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
  ;;  `conj-set           (scheme (fn-of [(set-of (s-var 'e)) (s-var 'e)]
  ;;                                     (set-of (s-var 'e))))
   'disj               (scheme (fn-of [(set-of (s-var 'e)) (s-var 'e)]
                                      (set-of (s-var 'e))))
  ;;  'set-contains?      (scheme (fn-of [(set-of (s-var 'e)) (s-var 'e)]
  ;;                                     BOOLEAN))
  ;;  'count-set          (scheme (fn-of [(set-of (s-var 'e))] INT))
   `map-set            (scheme (fn-of [(fn-of [(s-var 'a)] (s-var 'b))
                                       (set-of (s-var 'a))]
                                      (set-of (s-var 'b))))
  ;;  `filter-set         (scheme (fn-of [(fn-of [(s-var 'a)] BOOLEAN)
  ;;                                      (set-of (s-var 'a))]
  ;;                                     (set-of (s-var 'a))))
  ;;  'reduce-set         (scheme (fn-of [(fn-of [(s-var 'a) (s-var 'a)] (s-var 'a))
  ;;                                      (set-of (s-var 'a))]
  ;;                                     (s-var 'a)))
  ;;  'fold-set           (scheme (fn-of [(fn-of [(s-var 'b) (s-var 'a)] (s-var 'b))
  ;;                                      (s-var 'b)
  ;;                                      (set-of (s-var 'a))]
  ;;                                     (s-var 'b)))
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
  ;;  'vec->map           (scheme (fn-of [(vector-of (tuple-of (s-var 'k) (s-var 'v)))]
  ;;                                     (map-of (s-var 'k) (s-var 'v))))
  ;;  'set->map           (scheme (fn-of [(set-of (tuple-of (s-var 'k) (s-var 'v)))]
  ;;                                     (map-of (s-var 'k) (s-var 'v))))
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
  ;;  'map-contains?      (scheme (fn-of [(map-of (s-var 'k) (s-var 'v)) (s-var 'k)]
  ;;                                     BOOLEAN))
   `keys-vec           (scheme (fn-of [(map-of (s-var 'k) (s-var 'v))]
                                      (vector-of (s-var 'k))))
   `keys-set           (scheme (fn-of [(map-of (s-var 'k) (s-var 'v))]
                                      (set-of (s-var 'k))))
   `vals-vec           (scheme (fn-of [(map-of (s-var 'k) (s-var 'v))]
                                      (vector-of (s-var 'v))))
   'merge              (scheme (fn-of [(map-of (s-var 'k) (s-var 'v))
                                       (map-of (s-var 'k) (s-var 'v))]
                                      (map-of (s-var 'k) (s-var 'v))))
  ;;  'count-map          (scheme (fn-of [(map-of (s-var 'k) (s-var 'v))]
  ;;                                     INT))
  ;;  'map-map            (scheme (fn-of [(fn-of [(tuple-of (s-var 'k) (s-var 'v))] (s-var 'e))
  ;;                                      (map-of (s-var 'k) (s-var 'v))]
  ;;                                     (vector-of (s-var 'e))))
  ;;  `filter-map         (scheme (fn-of [(fn-of [(tuple-of (s-var 'k) (s-var 'v))] BOOLEAN)
  ;;                                      (map-of (s-var 'k) (s-var 'v))]
  ;;                                     (map-of (s-var 'k) (s-var 'v))))
  ;;  'reduce-map         (let [entry (tuple-of (s-var 'k) (s-var 'v))]
  ;;                        (scheme (fn-of [(fn-of [entry entry] entry)
  ;;                                        (map-of (s-var 'k) (s-var 'v))]
  ;;                                       entry)))
  ;;  'fold-map           (scheme (fn-of [(fn-of [(s-var 'r)
  ;;                                              (tuple-of (s-var 'k) (s-var 'v))]
  ;;                                             (s-var 'r))
  ;;                                      (s-var 'r)
  ;;                                      (map-of (s-var 'k) (s-var 'v))]
  ;;                                     (s-var 'r)))
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Printing & Side Effects
   'do2                (scheme (fn-of [NIL (s-var 'a)] (s-var 'a)))
   'do3                (scheme (fn-of [NIL NIL (s-var 'a)] (s-var 'a)))
   'print              (scheme (fn-of [(s-var 'a)] NIL))
   'println            (scheme (fn-of [(s-var 'a)] NIL))})

(def dealiases
  '{->map1            hash-map
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
    ;; char->int         int
    char-occurrences  erp12.cbgp-lite.lang.lib/occurrences-of
    ;; comp2-fn1         comp
    ;; comp2-fn2         comp
    ;; comp3-fn1         comp
    ;; comp3-fn2         comp
    ;; count-map         count
    ;; count-set         count
    ;; count-vec         count
    do2               do
    do3               do
    ;; double-abs        abs
    ;; double-add        +
    ;; double-dec        dec
    ;; double-div        erp12.cbgp-lite.lang.lib/safe-div
    ;; double-inc        inc
    ;; double-mod        erp12.cbgp-lite.lang.lib/safe-mod
    ;; double-mult       *
    ;; double-quot       erp12.cbgp-lite.lang.lib/safe-quot
    ;; double-sub        -
    ;; double-neg        -
    ;; empty-str?        empty?
    ;; first-str         first
    fold                 reduce
    ;; fold-vec          reduce
    ;; fold-map          reduce
    ;; fold-set          reduce
    get-or-else       get
    ;; index-of-char     clojure.string/index-of
    ;; index-of-str      clojure.string/index-of
    ;; int-abs           abs
    ;; int-add           +
    ;; int-dec           dec
    ;; int-div           erp12.cbgp-lite.lang.lib/safe-div
    ;; int-inc           inc
    ;; int-mod           erp12.cbgp-lite.lang.lib/safe-mod
    ;; int-mult          *
    ;; int-quot          erp12.cbgp-lite.lang.lib/safe-quot
    ;; int-sub           -
    ;; int-neg           -
    join-chars        clojure.string/join
    ;; last-str          last
    left              first
    ;; length            count
    ;; map->set          set
    ;; map->vec          vec
    ;; map-contains?     contains?
    ;; map-map           mapv
    ;; map-str           mapv
    ;; map-vec           mapv
    ;; map2-vec          mapv
    ;; mapcat-str        erp12.cbgp-lite.lang.lib/mapcatv
    nth-or-else       nth
    ;; nth-str           erp12.cbgp-lite.lang.lib/safe-nth
    ;; partial1-fn2      partial
    ;; partial1-fn3      partial
    ;; partial2-fn3      partial
    range1            erp12.cbgp-lite.lang.lib/rangev
    range2            erp12.cbgp-lite.lang.lib/rangev
    range3            erp12.cbgp-lite.lang.lib/rangev
    ;; reduce-vec        reduce
    ;; reduce-map        reduce
    ;; reduce-set        reduce
    right             second
    ;; set->map          erp12.cbgp-lite.lang.lib/->map
    ;; set->vec          vec
    ;; set-contains?     contains?
    split-str-on-char erp12.cbgp-lite.lang.lib/split-str
    str-join-sep      clojure.string/join})
    ;; string->chars     vec
    ;; vec->map          erp12.cbgp-lite.lang.lib/->map
    ;; vec->set          set
    
    ;; zero-double?      zero?
    ;; zero-int?         zero?
    

(def macros
  #{'if 'do2 'do3})

(defn lib-for-type-ctors
  [type-ctors] 
  (->> type-env
       (filter (fn [[_ typ]]
                 (->> (schema/schema-terms typ) 
                      (remove #{:cat :s-var :scheme})
                      (set)
                      (set/superset? type-ctors)))) 
       (into {})))
