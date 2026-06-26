(ns erp12.cbgp-lite.benchmark.problems.composite
  "Composite benchmark problems with procedurally generated training cases spanning diverse output types."
  (:require [clj-fuzzy.levenshtein :as lev]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [erp12.cbgp-lite.benchmark.problems.psb :refer [input-symbols]]
            [erp12.cbgp-lite.benchmark.utils :as u]
            [erp12.cbgp-lite.genome :as g]
            [erp12.cbgp-lite.individual :as i]
            [erp12.cbgp-lite.program.lib :as lib]
            [erp12.cbgp-lite.program.types :as t]))

(defn rand-int-range
  "Returns random int between low and high, both inclusive."
  [low high]
  (+ low (rand-int (inc (- high low)))))

(defn rand-float-range
  "Returns random float between low and high, both inclusive."
  [low high]
  (+ low (rand (- high low))))

(defn rand-vector
  [min-size max-size element-gen]
  (vec (repeatedly (rand-int-range min-size max-size)
                   element-gen)))

(def names-100
  ["Abel" "Margaret" "Kimber" "Kase" "Cecelia" "Katalina" "Alianna" "Bode" "Cody" "Charles" "Kinsley" "Kaliyah" "Jon" "Salem" "Nora" "Brodie" "Davis" "Ares" "Andres" "Adrian" "Michael" "Mara" "Azariah" "Eileen" "Russell" "Royal" "Ricardo" "Andi" "Hank" "Annika" "Oaklyn" "Shepherd" "Killian" "Oakleigh" "Garrett" "Forest" "Daleyza" "Deacon" "Eden" "Oscar" "Lillie" "Cole" "Emberly" "Nathan" "Indie" "Elise" "Andy" "Brayan" "Brylee" "Princess" "Julie" "Raelyn" "Clay" "Georgia" "Manuel" "Cataleya" "Lian" "Krew" "Marceline" "Ryder" "Asa" "Beckham" "Emmy" "Piper" "Cal" "Isabella" "Blaine" "Peyton" "Jasiah" "Elon" "Kai" "Mariam" "Ryan" "Jamie" "Zavier" "Lee" "Declan" "Adalynn" "Griffin" "Bristol" "Colt" "Eva" "Erin" "Landry" "Maeve" "Finley" "Spencer" "Luciano" "Trevor" "Adelynn" "Everlee" "Damon" "Alexis" "Renata" "Layne" "Emerson" "Khari" "Gracelynn" "Ozzy" "Eve"])

(def int-predicates
  [zero?
   pos?
   neg?
   even?
   odd?])

(def double-predicates
  [zero?
   pos?
   neg?
   (fn mag-10? [x] (and (< -10 x) (< x 10)))])

(def bool-predicates
  [true? false?])

(def string-predicates
  [empty?
   distinct?
   (fn has-space? [s] (str/includes? s " "))
   (fn >5-chars? [s] (> (count s) 5))
   (fn even-chars? [s] (zero? (mod (count s) 2)))])

(def char-predicates
  [lib/whitespace?
   lib/digit?
   lib/letter?
   (fn upper-case? [c] (= (str c) (str/upper-case c)))
   (fn lower-case? [c] (= (str c) (str/lower-case c)))])

(def set-of-ints-predicates
  [empty?
   (fn has-zero? [s] (contains? s 0))
   (fn has-even? [s] (some even? s))
   (fn has-neg? [s] (some neg? s))
   (fn >10-size? [s] (> (count s) 10))
   (fn has-012? [s] (set/subset? #{0 1 2} s))])

;;;;;;;;;;;;;;;
;; Case generator functions

(defn sum-2-vals-case-generator
  "Produce a map of inputs and outputs.
   Works with any key generator function key-gen"
  [key-gen]
  (let [key1      (key-gen)
        key2      (key-gen)
        ; along with two guaranteed keys, this makes at most 50 kv pairs
        num-kvs   (rand-int 49)
        the-keys  (repeatedly num-kvs key-gen)
        the-vals  (repeatedly num-kvs #(rand-int-range -1000 1000))
        input-map (assoc (zipmap the-keys the-vals)
                         key1 (rand-int-range -1000 1000)
                         key2 (rand-int-range -1000 1000))]
    {:inputs [input-map key1 key2]
     :output (+ (get input-map key1) (get input-map key2))}))

(def value-generators-and-predicates
  [{:val-gen (u/int-generator 100)
    :preds   int-predicates}
   {:val-gen #(- (rand 200.0) 100.0)
    :preds   double-predicates}
   {:val-gen u/rand-bool
    :preds   bool-predicates}
   {:val-gen u/rand-char
    :preds   char-predicates}
   {:val-gen (u/string-generator 12)
    :preds   string-predicates}
   {:val-gen #(set (repeatedly (rand-int 12) (u/int-generator 100)))
    :preds   set-of-ints-predicates}])

(defn make-int-to-int-fn
  [bound]
  (let [rand-map             (zipmap (range bound) (shuffle (range bound)))
        rand-int-bound       (rand-int bound)
        rand-int-range-50-50 (rand-int-range -50 50)
        options              [(fn [x] (+ (- (abs (- rand-int-bound x)))
                                         rand-int-range-50-50))
                              (fn [x] (let [a rand-int-bound]
                                        (+ (* -1 (- a x) (- a x))
                                           rand-int-range-50-50)))
                              (fn [x] (get rand-map x))]]
    (rand-nth options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Problems ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def DEFAULT-PENALTY 1e12)

(defn case-gen->dataset-reader
  [case-gen]
  (fn dataset-reader [{:keys [n-train n-test]}]
    {:train (repeatedly n-train case-gen)
     :test  (repeatedly n-test case-gen)}))

;; @TODO add :solution genomes to ensure all problems are solvable.

(def problems
  {"area-of-rectangle"
   {:description    (str "Given two tuples of floats representing the upper-right and "
                         "lower-left coordinates of a rectangle in the cartesian plane, "
                         "find the area of the rectangle.")
    :input-symbols  (input-symbols 2)
    :input-types    [(t/tuple-type [t/FLOAT t/FLOAT])
                     (t/tuple-type [t/FLOAT t/FLOAT])]
    :output-type    t/FLOAT
    :type-ctors     #{t/FLOAT (t/tuple-ctor 2)}
    :extra-genes    []
    :dataset-reader (case-gen->dataset-reader
                     (fn area-of-rectangle-gen
                       []
                       (let [xs [(rand-float-range -100 100) (rand-float-range -100 100)]
                             ys [(rand-float-range -100 100) (rand-float-range -100 100)]
                             x1 (reduce max xs)
                             x2 (reduce min xs)
                             y1 (reduce max ys)
                             y2 (reduce min ys)
                             output (* (- x1 x2)
                                       (- y1 y2))]
                         {:inputs [[x1 y1] [x2 y2]]
                          :output output})))
    :penalty        DEFAULT-PENALTY
    :loss-fns       [#(u/round 4 (u/absolute-distance %1 %2))]
    :solution       (list (g/->Var 'input2)
                          (g/->Var `lib/left)
                          (g/->App)
                          (g/->Var 'input1)
                          (g/->Var `lib/left)
                          (g/->App)
                          (g/->Var `lib/float-sub)
                          (g/->App)

                          (g/->Var 'input2)
                          (g/->Var `lib/right)
                          (g/->App)
                          (g/->Var 'input1)
                          (g/->Var `lib/right)
                          (g/->App)
                          (g/->Var `lib/float-sub)
                          (g/->App)

                          (g/->Var `lib/float-mult)
                          (g/->App)

                          ;
                          )}

   "centimeters-to-meters"
   {:description    (str "Given a length in centimeters, return a tuple of (meters, centimeters) "
                         "that corresponds to the same length.")
    :input-symbols  (input-symbols 1)
    :input-types    [t/INT]
    :output-type    (t/tuple-type [t/INT t/INT])
    :type-ctors     #{t/INT t/BOOL (t/tuple-ctor 2)}
    :extra-genes    [(g/->Lit 0 t/INT)
                     (g/->Lit 100 t/INT)]
    :dataset-reader (case-gen->dataset-reader
                     (fn centimeters-to-meters-gen []
                       (let [in-cm (rand-int 10000)
                             out-m (quot in-cm 100)
                             out-cm (mod in-cm 100)]
                         {:inputs [in-cm]
                          :output (vector out-m out-cm)})))
    :penalty        DEFAULT-PENALTY
    :loss-fns       [#(u/absolute-distance (first %1) (first %2))
                     #(u/absolute-distance (second %1) (second %2))]
    :solution       (list (g/->Lit 100 t/INT)
                          (g/->Var 'input1)
                          (g/->Var `lib/int-mod)
                          (g/->App)

                          (g/->Lit 100 t/INT)
                          (g/->Var 'input1)
                          (g/->Var `lib/int-quot)
                          (g/->App)

                          (g/->Var `lib/->tuple2)
                          (g/->App))}

   "count-true"
   {:description    (str "Given a vector of T and a predicate T => bool, return the "
                         "count of the number of elements in T that make the predicate true.")
    :input-symbols  (input-symbols 2)
    :input-types    [(t/vec-type (t/rigid 'T))
                     (t/fn-type [(t/rigid 'T)] t/BOOL)]
    :output-type    t/INT
    :type-ctors     #{t/INT t/BOOL t/VECTOR}
    :extra-genes    [(g/->Lit 0 t/INT)
                     (g/->Lit true t/BOOL)
                     (g/->Lit false t/BOOL)]
    :dataset-reader (case-gen->dataset-reader
                     (fn count-true-gen []
                       (let [{:keys [val-gen preds]} (rand-nth value-generators-and-predicates)
                             vector (rand-vector 0 50 val-gen)
                             pred (rand-nth preds)]
                         {:inputs [vector pred]
                          :output (count (filter pred vector))})))
    :penalty        DEFAULT-PENALTY
    :loss-fns       [u/absolute-distance]
    ;; :solution-clojure '(count (filter input2 input1))
    :solution       (list (g/->Var 'input1)
                          (g/->Var 'input2)
                          (g/->Var `filterv)
                          (g/->App)

                          (g/->Var `lib/count-vec)
                          (g/->App))}

   "filter-bounds"
   {:description    (str "Given a set of elements that are all of the same comparable "
                         "type, T , and two instance of type T representing a lower and "
                         "upper bound, filter the set to the elements that fall "
                         "between two bounds (inclusively)."
                         "Note: This version of CBGP does not yet enforce that T is comparable.")
    :input-symbols  (input-symbols 3)
    :input-types    [(t/set-type (t/rigid 'T))
                     (t/rigid 'T)
                     (t/rigid 'T)]
    :output-type    (t/set-type (t/rigid 'T))
    :type-ctors     #{t/BOOL t/SET}
    :extra-genes    [(g/->Abs [(t/rigid 'T)] t/BOOL)
                     (g/->Abs [(t/rigid 'T) (t/rigid 'T)] t/BOOL)
                     (g/->Abs [(t/set-type (t/rigid 'T))] (t/set-type (t/rigid 'T)))]
    :dataset-reader (let [generators [(u/string-generator 10)
                                      u/rand-char
                                      (u/int-generator 1000)
                                      (u/int-generator 20)
                                      rand]]
                      (case-gen->dataset-reader
                       (fn filter-bounds-gen []
                         (let [val-gen (rand-nth generators)
                               the-set (set (rand-vector 20 50 val-gen))
                               x (val-gen)
                               y (val-gen)
                               lower (lib/min' x y)
                               upper (lib/max' x y)]
                           {:inputs [the-set lower upper]
                            :output (set (filter #(and (lib/<' lower %) (lib/<' % upper))
                                                 the-set))}))))
    :penalty        DEFAULT-PENALTY
    :loss-fns       [u/jaccard-similarity-loss]
    ;; :solution-clojure '(lib/filter-set (fn [local0] 
    ;;                                      (and (lib/<' input2 local0)
    ;;                                           (lib/<' local0 inpu3)))
    ;;                                    input1)
    :broken-solution       (list (g/->Var 'input1)
                                 ;; TMH: I can't get this working. Am I doing anon fn right?

                          ;; anon-fn
                                 (g/->Abs [(t/rigid 'T)] t/BOOL)
                          ;; TMH: Are arguments taken as top first or lower first? Push does lower first,
                          ;; but if I'm reading things correctly, it's top first here
                                 (g/->Var 'input2)
                                 (g/->Local 0) ;; local0
                                 (g/->Var `lib/<')
                                 (g/->App)

                          ;;;; TMH: commenting out to just see if it produces better code
                          ;; (g/local-gene {:idx 0}) ;; local0
                          ;; (g/->Var 'input3)
                          ;; (g/->Var `lib/<')
                          ;; (g/->App)

                          ;; (g/->Var `lib/and')
                          ;; (g/->App)
                                 :close ;; end anon-fn

                                 (g/->Var `lib/filter-set)
                                 (g/->App)

                          ;; todo: if I had the order wrong for set-cartesian-product, maybe I could fix it

                          ;
                                 )}

   "filter-bounds-int"
   {:description    (str "Given a set of elements that are all of the same comparable "
                         "type, T , and two instance of type T representing a lower and "
                         "upper bound, filter the set to the elements that fall "
                         "between two bounds (inclusively)."
                         "Note: This version of CBGP does not yet enforce that T is comparable.")
    :input-symbols  (input-symbols 3)
    :input-types    [(t/set-type t/INT)
                     t/INT
                     t/INT]
    :output-type    (t/set-type t/INT)
    :type-ctors     #{t/BOOL t/SET}
    :extra-genes    []
    :dataset-reader (let [generators [(u/int-generator 1000)
                                      (u/int-generator 20)]]
                      (case-gen->dataset-reader
                       (fn filter-bounds-gen []
                         (let [val-gen (rand-nth generators)
                               the-set (set (rand-vector 20 50 val-gen))
                               x (val-gen)
                               y (val-gen)
                               lower (lib/min' x y)
                               upper (lib/max' x y)]
                           {:inputs [the-set lower upper]
                            :output (set (filter #(and (lib/<' lower %) (lib/<' % upper))
                                                 the-set))}))))
    :penalty        DEFAULT-PENALTY
    :loss-fns       [u/jaccard-similarity-loss]
       ;; :solution-clojure '(lib/filter-set (fn [local0] 
       ;;                                      (and (lib/<' input2 local0)
       ;;                                           (lib/<' local0 inpu3)))
       ;;                                    input1)
    :broken-solution       (list (g/->Var 'input1)

                             ;; anon-fn
                                 (g/->Abs [t/INT] t/BOOL)
                             ;; TMH: Are arguments taken as top first or lower first? Push does lower first,
                             ;; but if I'm reading things correctly, it's top first here
                                 (g/->Local 0) ;; local0
                                 (g/->Var 'input2)
                                 (g/->Var `lib/<')
                                 (g/->App)

                                 (g/->Var 'input3)
                                 (g/->Local 0) ;; local0
                                 (g/->Var `lib/<')
                                 (g/->App)

                                 (g/->Var `lib/and')
                                 (g/->App)
                                 :close ;; end anon-fn

                                 (g/->Var `lib/filter-set)
                                 (g/->App)

                             ;
                                 )}

   "first-index-of-true"
   {:description    (str "Given a vector of T and a predicate T => bool, return the "
                         "first index in the vector where the predicate is true.")
    :input-symbols  (input-symbols 2)
    :input-types    [(t/vec-type (t/rigid 'T))
                     (t/fn-type [(t/rigid 'T)] t/BOOL)]
    :output-type    t/INT
    :type-ctors     #{t/INT t/BOOL t/VECTOR}
    :extra-genes    [(g/->Lit -1 t/INT)
                     (g/->Lit 0 t/INT)
                     (g/->Lit true t/BOOL)
                     (g/->Lit false t/BOOL)]
    :dataset-reader (case-gen->dataset-reader
                     (fn first-index-of-true-gen []
                       (loop [attempt 0]
                         (let [{:keys [val-gen preds]} (rand-nth value-generators-and-predicates)
                               the-vector (rand-vector 0 50 val-gen)
                               pred (rand-nth preds)
                               output (->> the-vector
                                           (map-indexed vector)
                                           (filter #(pred (second %)))
                                           ffirst)]
                           (if (or (nil? output)
                                   (< output (- 10 attempt)))
                             (recur (inc attempt))
                             {:inputs [the-vector pred]
                              :output output})))))
    :penalty        DEFAULT-PENALTY
    :loss-fns       [u/absolute-distance]
    ;; :solution-clojure '(index-of 'input1 (first (filter input2 input1)))
    :solution       (list (g/->Var 'input1)
                          (g/->Var 'input2)
                          (g/->Var `filterv)
                          (g/->App)

                          (g/->Var `first)
                          (g/->App)

                          (g/->Var 'input1)
                          (g/->Var `lib/index-of)
                          (g/->App))}

   "get-vals-of-key"
   {:description    (str "Given a vector of maps [{string => int} ...] and a key, "
                         "make a list of the values of that key in all the maps.")
    :input-symbols  (input-symbols 2)
    :input-types    [(t/vec-type (t/map-type t/STRING t/INT))
                     t/STRING]
    :output-type    (t/vec-type t/INT)
    :type-ctors     #{t/INT t/STRING t/MAP t/VECTOR t/BOOL}
    :extra-genes    [(g/->Lit [] (t/vec-type t/INT))
                     (g/->Abs [(t/map-type t/STRING t/INT)] t/INT)]
    :dataset-reader (case-gen->dataset-reader
                     (fn get-vals-of-key-gen
                       []
                       (let [num-keys-per-map (rand-int-range 1 8)
                             keys (repeatedly num-keys-per-map (u/string-generator 10))
                             map-gen #(zipmap keys (repeatedly (u/int-generator 1000)))
                             the-maps (rand-vector 0 25 map-gen)
                             the-key (rand-nth keys)
                             output (mapv #(get % the-key) the-maps)]
                         {:inputs [the-maps the-key]
                          :output output})))
    :penalty        DEFAULT-PENALTY
    :loss-fns       [lev/distance]
    :solution-clojure '(mapv #(get % input2)
                             input1)
    ;; TMH: Much easier with fn abstraction, not attempted yet
    :broken-solution       (list (g/->Var 'input1))}
   
   "max-applied-fn"
   {:description   (str "Given an integer X < 50 and a (int => int) function, return "
                        "the integer in [0, X) that results in the maximum value for "
                        "the function.")
    :input-symbols  (input-symbols 2)
    :input-types    [t/INT
                     (t/fn-type [t/INT] t/INT)]
    :output-type    t/INT
    :type-ctors     #{t/INT t/BOOL t/VECTOR}
    :extra-genes    [(g/->Lit 0 t/INT)
                     (g/->Lit true t/BOOL)
                     (g/->Lit false t/BOOL)]
    :dataset-reader (case-gen->dataset-reader
                     (fn max-applied-fn-case-gen
                       []
                       (let [bound (rand-int-range 1 49)
                             the-fn (make-int-to-int-fn bound)
                             output (apply max-key the-fn (range bound))]
                         {:inputs [bound the-fn]
                          :output output})))
    :penalty        DEFAULT-PENALTY
    :loss-fns       [u/absolute-distance]
    ;; Solution program: (last (sortv-by input2 (range input1)))
    :solution       (list (g/->Var 'input1)
                          (g/->Var `lib/range1)
                          (g/->App)

                          (g/->Var 'input2)
                          (g/->Var `lib/sort-by-vec)
                          (g/->App)

                          (g/->Var `last)
                          (g/->App))}

   "min-key"
   {:description    "Given map of {key => int}, return the key with the min value."
    :input-symbols  (input-symbols 1)
    :input-types    [(t/map-type (t/rigid 'T) t/INT)]
    :output-type    (t/rigid 'T)
    :type-ctors     #{t/INT t/BOOL t/MAP}
    :extra-genes    []
    :dataset-reader (let [generators [(u/string-generator 10)
                                      u/rand-char
                                      (u/int-generator 1000)
                                      rand
                                      u/rand-bool
                                      ;; vector of booleans
                                      #(vec (repeatedly (inc (rand-int 16)) u/rand-bool))
                                      ;; tuple containing a char and an integer
                                      #(vector (u/rand-char) (rand-int-range -10 10))]]
                      (case-gen->dataset-reader
                       (fn min-key-gen []
                         (let [val-gen (rand-nth generators)
                               the-map (zipmap (rand-vector 1 50 val-gen)
                                               (repeatedly (u/int-generator 1000)))
                               output (first (apply min-key second the-map))]
                               ;; Ensure the min is unique, i.e. there aren't two keys with same min
                               ;; Just recur to try again if not.
                           (if (< 1 (count (filter #(= (get the-map output) %)
                                                   (vals the-map))))
                             (recur)
                             {:inputs [the-map]
                              :output output})))))
    :penalty        DEFAULT-PENALTY
    :loss-fns       [#(if (= %1 %2) 0 1)]
    :solution-clojure '(first (lib/sort-by-vec (lib/partial1-fn2 get input1)
                                               (lib/keys-vec input1)))
    :solution       (list (g/->Var 'input1)
                          (g/->Var `lib/keys-vec)
                          (g/->App) ;; get keys of input1

                          (g/->Var 'input1)
                          (g/->Var `get)
                          (g/->Var `lib/partial1-fn2)
                          (g/->App) ;; partial get over input1

                          (g/->Var `lib/sort-by-vec)
                          (g/->App) ;; sort keys by partialed fn

                          (g/->Var `first)
                          (g/->App) ;; get first key
                          )}

   "set-cartesian-product"
   {:description    "Given two sets of ints, find their cartesian product, which will be a set of tuples of ints."
    :input-symbols  (input-symbols 2)
    :input-types    [(t/set-type t/INT)
                     (t/set-type t/INT)]
    :output-type    (t/set-type (t/tuple-type [t/INT t/INT]))
    :type-ctors     #{t/SET t/INT t/BOOL (t/tuple-ctor 2)}
    :extra-genes    [(g/->Lit 0 t/INT)
                     (g/->Abs [t/INT] (t/tuple-type [t/INT t/INT]))
                     (g/->Abs [t/INT] (t/set-type (t/tuple-type [t/INT t/INT])))]
    :dataset-reader (case-gen->dataset-reader
                     (let [set-generator (fn [] (set (rand-vector 0 21 #(rand-int 100))))]
                       (fn cartesian-product-gen
                         []
                         (let [set1 (set-generator)
                               set2 (set-generator)
                               output (set (for [x set1
                                                 y set2]
                                             (vector x y)))]
                           {:inputs [set1 set2]
                            :output output}))))
    :penalty        DEFAULT-PENALTY
    :loss-fns       [u/jaccard-similarity-loss]
    ;; :solution-clojure '(lib/reduce-vec lib/set-union
    ;;                                    (mapv (fn [i1]
    ;;                                            (lib/map-set (fn [i2] ;; fn-inner
    ;;                                                           (lib/->tuple2 i1 i2))
    ;;                                                         input2))
    ;;                                          input1))

    ;; TMH this isn't compiling correctly after first draft, I don't know why
    :broken-solution       (list (g/->Var 'input1)

                          ;; fn-outer
                                 (g/->Abs [t/INT] (t/set-type (t/tuple-type [t/INT t/INT])))
                                 (g/->Var 'input2)

                          ;; fn-inner
                                 (g/->Abs [t/INT] (t/tuple-type [t/INT t/INT]))
                                 (g/local-gene {:idx 0}) ;; i1
                                 (g/local-gene {:idx 1}) ;; i2
                                 (g/->Var `lib/->tuple2)
                                 (g/->App)
                                 :close ;; fn-inner close

                                 (g/->Var `lib/map-set)
                                 (g/->App)
                                 :close ;; fn-outer close

                                 (g/->Var `mapv)
                                 (g/->App) ;; mapv fn-outer over input1

                                 (g/->Var `lib/set-union)
                                 (g/->Var `lib/reduce-vec)
                                 (g/->App) ;; reduce set-union over result of above
                                 )
    :broken-push '[{:sym input1}
                   {:param-types [{:kind :*, :sym INT}],
                    :push [{:sym input2}
                           {:param-types [{:kind :*, :sym INT}],
                            :push [{:idx 1}
                                   {:idx 0}
                                   {:sym erp12.cbgp-lite.program.lib/->tuple2}
                                   {}],
                            :ret-type {:args [{:kind :*, :sym INT}
                                              {:kind :*, :sym INT}],
                                       :con {:kind {:k-args [:* :*],
                                                    :k-ret :*},
                                             :sym TUPLE2}}}
                           {:sym erp12.cbgp-lite.program.lib/map-set}
                           {}],
                    :ret-type {:args [{:args [{:kind :*, :sym INT}
                                              {:kind :*, :sym INT}],
                                       :con {:kind {:k-args [:* :*],
                                                    :k-ret :*},
                                             :sym TUPLE2}}],
                               :con {:kind {:k-args [:*], :k-ret :*}, :sym SET}}}
                   {:sym clojure.core/mapv}
                   {}
                   {:sym erp12.cbgp-lite.program.lib/set-union}
                   {:sym erp12.cbgp-lite.program.lib/reduce-vec}
                   {}]}

   "set-symmetric-difference"
   {:description    "Given two sets, find the symmetric difference."
    :input-symbols  (input-symbols 2)
    :input-types    [(t/set-type t/INT)
                     (t/set-type t/INT)]
    :output-type    (t/set-type t/INT)
    :type-ctors     #{t/SET t/INT t/BOOL}
    :extra-genes    [(g/->Lit #{} (t/set-type t/INT))]
    :dataset-reader (let [set-gen (fn [] (set (repeatedly (rand-int 50) #(rand-int 50))))]
                      (case-gen->dataset-reader
                       (fn set-symmetric-difference-gen
                         []
                         (let [set1   (set-gen)
                               set2   (set-gen)
                               output (set/union (set/difference set1 set2)
                                                 (set/difference set2 set1))]
                           {:inputs [set1 set2]
                            :output output}))))
    :penalty        DEFAULT-PENALTY
    :loss-fns       [u/jaccard-similarity-loss]
    :solution       (list (g/->Var 'input1)
                          (g/->Var 'input2)
                          (g/->Var `set/difference)
                          (g/->App)
                          (g/->Var 'input2)
                          (g/->Var 'input1)
                          (g/->Var `set/difference)
                          (g/->App)
                          (g/->Var `lib/set-union)
                          (g/->App))}

   "sets-with-element"
   {:description    (str "Given a set of sets of integers, filter to only contain sets that "
                         "contain a certain element that is an integer.")
    :input-symbols  (input-symbols 2)
    :input-types    [(t/set-type (t/set-type t/INT))
                     t/INT]
    :output-type    (t/set-type (t/set-type t/INT))
    :type-ctors     #{t/SET t/INT t/BOOL}
    ;; TMH: should this have an anon fn extra gene, once they're working?
    :extra-genes    [(g/->LitGenerator (u/int-generator 100) t/INT)
                     (g/->Lit true t/BOOL)
                     (g/->Lit false t/BOOL)
                     (g/->Lit #{} (t/set-type t/INT))]
    :dataset-reader (case-gen->dataset-reader
                     (fn sets-with-element-gen []
                       (let [max-int 100
                             num-sets (rand-int 25)
                             int-gen #(rand-int max-int)
                             the-int (int-gen)
                             prob (rand) ; prob of including the-int
                             set-gen #(let [s (set (repeatedly (rand-int 25) int-gen))]
                                        (if (< (rand) prob)
                                          (conj s the-int)
                                          (disj s the-int)))
                             the-sets (set (repeatedly num-sets set-gen))
                             output (set (filter #(contains? % the-int)
                                                 the-sets))]
                         {:inputs [the-sets the-int]
                          :output output})))
    :penalty        DEFAULT-PENALTY
    :loss-fns       [u/jaccard-similarity-loss]

    :solution-clojure '(lib/filter-set (fn [local0] (lib/set-contains? local0 input2))
                                       input1)

    ;; TMH: Not easy to do this without anon fn, so I haven't really tried. You can do
    ;; it in a weird way with partial and subset, but not worth doing it that way
    :broken-solution       (list (g/->Var 'input2)
                                 (g/->Var 'input1)
                                 (g/->App))}

   "simple-encryption"
   {:description    (str "Given a string and a function (char => char), use the "
                         "function to encrypt the string.")
    :input-symbols  (input-symbols 2)
    :input-types    [t/STRING
                     (g/->Abs [t/CHAR] t/CHAR)]
    :output-type    t/STRING
    :type-ctors     #{t/STRING t/CHAR}
    :extra-genes    [(g/->Lit "" t/STRING)
                     (g/->Abs [t/CHAR] t/CHAR)
                     (g/->Abs [t/STRING] t/STRING)]
    :dataset-reader (case-gen->dataset-reader
                     (fn simple-encryption-gen []
                       (let [available-chars (vec (concat [\newline \tab] (map char (range 32 127))))
                                                         ;; These three need to be let here, so that they can be used inside
                                                         ;; of functions without those functions being random when run
                             char-map (zipmap available-chars (shuffle available-chars))
                             offset (rand-int-range -20 20)
                             char-map-with-limited-values (zipmap available-chars
                                                                  (let [opts (take (rand-int-range 2 6)
                                                                                   (shuffle available-chars))]
                                                                    (repeatedly #(rand-nth opts))))

                             the-string ((u/string-generator 20))
                             the-fn (rand-nth [(fn encrypt-random-map [ch]
                                                 (get char-map ch))
                                               (fn encrypt-random-limited-map [ch]
                                                 (get char-map-with-limited-values ch))
                                               (fn encrypt-caesar [ch]
                                                 (nth available-chars
                                                      (mod (+ offset (.indexOf available-chars ch))
                                                           (count available-chars))))])
                             output (apply str (map the-fn the-string))]
                         {:inputs [the-string the-fn]
                          :output output})))
    :penalty        DEFAULT-PENALTY
    :loss-fns       [lev/distance]
    :solution-clojure '(lib/map-str input2 input1)
    ;; TMH broken because Abs is broken
    :broken-solution       (list (g/->Var 'input2)
                                 (g/->Var 'input1)
                                 (g/->Var `lib/map-str)
                                 (g/->App))}

   "sum-2-vals"
   {:description    (str "Given a map from strings to ints and two strings that are "
                         "keys of the map, look up the values associated with those keys "
                         "in the map and return their sum.")
    :input-symbols  (input-symbols 3)
    :input-types    [(t/map-type t/STRING t/INT)
                     t/STRING
                     t/STRING]
    :output-type    t/INT
    :type-ctors     #{t/MAP t/STRING t/INT}
    :extra-genes    [(g/->Lit 0 t/INT)
                     (g/->LitGenerator (u/string-generator 21)
                                       t/STRING)]
    :dataset-reader (case-gen->dataset-reader
                     (fn sum-2-vals-gen []
                       (sum-2-vals-case-generator (u/string-generator 10))))
    :penalty        DEFAULT-PENALTY
    :loss-fns       [u/absolute-distance]
    :solution       (list (g/->Var 'input2) ;; get val of first string
                          (g/->Var 'input1)
                          (g/->Var `get)
                          (g/->App)

                          (g/->Var 'input3) ;; get val of second string
                          (g/->Var 'input1)
                          (g/->Var `get)
                          (g/->App)

                          (g/->Var `lib/int-add) ;; add those vals
                          (g/->App))}

   "sum-2-vals-polymorphic"
   {:description    (str "Given a map from polymorphic keys to ints and two"
                         "polymorphic keys, look up the values associated with"
                         "those keys in the map and return their sum.")
    :input-symbols  (input-symbols 3)
    :input-types    [(t/map-type (t/rigid 'T) t/INT) ;; TMH: How do I change the keys to be a polymorphic type that's same for all 3 inputs? Is this right?
                     (t/rigid 'T)
                     (t/rigid 'T)]
    :output-type    t/INT
    :type-ctors     #{t/MAP t/INT}
    :extra-genes    [(g/->Lit 0 t/INT)]
    :dataset-reader (let [key-generators [(u/string-generator 10)
                                          (u/int-generator 1000)
                                          u/rand-char
                                          rand
                                          ;; vector of booleans
                                          #(vec (repeatedly (inc (rand-int 16)) u/rand-bool))
                                          ;; tuple containing a char and an integer
                                          #(vector (u/rand-char) (rand-int-range -10 10))]]
                      (case-gen->dataset-reader
                       (fn sum-2-vals-polymorphic-gen []
                         (sum-2-vals-case-generator (rand-nth key-generators)))))
    :penalty        DEFAULT-PENALTY
    :loss-fns       [u/absolute-distance]
    :solution       (list (g/->Var 'input2) ;; get val of first string
                          (g/->Var 'input1)
                          (g/->Var `get)
                          (g/->App)

                          (g/->Var 'input3) ;; get val of second string
                          (g/->Var 'input1)
                          (g/->Var `get)
                          (g/->App)

                          (g/->Var `lib/int-add) ;; add those vals
                          (g/->App))}

   "sum-2D"
   {:description    "Given 2D vector of ints (i.e. vector of vector of ints), return sum of all ints."
    :input-symbols  (input-symbols 1)
    :input-types    [(t/vec-type (t/vec-type t/INT))]
    :output-type    t/INT
    :type-ctors     #{t/VECTOR t/INT t/BOOL}
    :extra-genes    [(g/->Lit 0 t/INT)]
    :dataset-reader (case-gen->dataset-reader
                     (fn sum-2D-gen []
                       (let [rows (inc (rand-int 10))
                             cols (inc (rand-int 10))
                             input-matrix (vec (for [_ (range rows)]
                                                 (vec (repeatedly cols (u/int-generator 1000)))))
                             output (reduce + (map #(reduce + %) input-matrix))]
                         {:inputs [input-matrix]
                          :output output})))
    :penalty        DEFAULT-PENALTY
    :loss-fns       [u/absolute-distance]
    :solution       (list (g/->Var 'input1)
                          (g/->Var `lib/concat-v)
                          (g/->Var `lib/reduce-vec)
                          (g/->App)

                          (g/->Var `lib/int-add)
                          (g/->Var `lib/reduce-vec)
                          (g/->App))}

   "sum-vector-vals"
   {:description    (str "Given a map {string => int} and vector of strings that are "
                         "keys of the map, look up the values associated with those "
                         "keys in the map and return their sum.")
    :input-symbols  (input-symbols 2)
    :input-types    [(t/map-type t/STRING t/INT)
                     (t/vec-type t/STRING)]
    :output-type    t/INT
    :type-ctors     #{t/MAP t/VECTOR t/INT t/STRING}
    :extra-genes    [(g/->Lit 0 t/INT)
                     (g/->Abs [t/STRING] t/INT)]
    :dataset-reader (case-gen->dataset-reader
                     (fn sum-vector-vals-gen []
                       (let [the-map (first (:inputs (sum-2-vals-case-generator (u/string-generator 10))))
                             prob (+ 0.1 (rand 0.8))
                             the-vector (vec (random-sample prob (keys the-map)))]
                         {:inputs [the-map the-vector]
                          :output (apply + (map the-map the-vector))})))
    :penalty        DEFAULT-PENALTY
    :loss-fns       [u/absolute-distance]

    :solution-clojure '(lib/reduce-vec lib/int-add
                                       (mapv (lib/partial1-fn2 get input1)
                                             input2))

    :solution-clojure2 '(lib/reduce-vec lib/int-add
                                        (mapv (fn [local0] (get input1 local0))
                                              input2))

    :solution       (list (g/->Var 'input2)

                          (g/->Var 'input1)
                          (g/->Var `get)
                          (g/->Var `lib/partial1-fn2)
                          (g/->App) ;; partial get over input1

                          (g/->Var `mapv)
                          (g/->App) ;; mapv partialled fn over input2

                          (g/->Var `lib/int-add)
                          (g/->Var `lib/reduce-vec)
                          (g/->App) ;; sum results
                          )

    ;; TMH this one with anon fn also doesn't work. I think anon fns are just broken?
    :broken-solution       (list (g/->Var 'input2)

                                 (g/->Abs [t/STRING] t/INT)
                                 (g/->Local 0)
                                 (g/->Var 'input1)
                                 (g/->Var `get)
                                 (g/->App)
                                 :close

                                 (g/->Var `mapv)
                                 (g/->App) ;; mapv anon fn over input2

                                 (g/->Var `lib/int-add)
                                 (g/->Var `lib/reduce-vec)
                                 (g/->App) ;; sum results
                                 )}
   "time-sheet"
   {:description    (str "Given a list of tuples of the form: [(name, hours), ...], "
                         "and a specific name, sum the hours associated with that name.")
    :input-symbols  (input-symbols 2)
    :input-types    [(t/vec-type (t/tuple-type [t/STRING t/INT]))
                     t/STRING]
    :output-type    t/INT
    :type-ctors     #{t/VECTOR t/INT t/STRING t/BOOL (t/tuple-ctor 2)}
    ;; TMH: Do I need a fn abs extra gene or two?
    :extra-genes    [(g/->Lit true t/BOOL)
                     (g/->Lit false t/BOOL)]
    :dataset-reader (case-gen->dataset-reader
                     (fn time-sheet-gen []
                       (let [num-records (inc (rand-int 50))
                             num-names (inc (rand-int 10))
                             names (vec (take num-names (shuffle names-100)))
                             records (vec (repeatedly num-records #(vector (rand-nth names)
                                                                           (rand-int 50))))
                             the-name (rand-nth names)
                             output (apply + (map second (filter #(= the-name (first %))
                                                                 records)))]
                         {:inputs [records the-name]
                          :output output})))
    :penalty        DEFAULT-PENALTY
    :loss-fns       [u/absolute-distance]
    ;; TMH: This would be easier with anon fns. When working, update it and genome solution?
    :solution-clojure '(lib/reduce-vec lib/int-add
                                       (mapv lib/right
                                             (filterv (lib/comp2-fn1 (lib/partial1-fn2 = input2)
                                                                     lib/left)
                                                      input1)))
    :solution       (list (g/->Var `lib/right)

                          (g/->Var 'input1)

                          (g/->Var `lib/left)

                          (g/->Var 'input2)
                          (g/->Var `=)
                          (g/->Var `lib/partial1-fn2)
                          (g/->App) ;; partial = with input2

                          (g/->Var `lib/comp2-fn1)
                          (g/->App) ;; comp the partialled function with left

                          (g/->Var `filterv)
                          (g/->App) ;; filter input1 using comp'ed function

                          (g/->Var `mapv)
                          (g/->App) ;; map right over filtered vector

                          (g/->Var `lib/int-add)
                          (g/->Var `lib/reduce-vec)
                          (g/->App) ;; sum results
                          )}
;
   })


(defn validate-solutions
  [{:keys [num-cases penalty hooks]
    :or   {penalty 1000
           hooks   {}}}]
  (let [problems-with-solutions (filter (fn [[_ md]] (contains? md :solution)) problems)]
    (doseq [[problem-name problem-metadata] problems-with-solutions]
      (log/info "Starting" problem-name)
      (let [evaluator  (i/make-genome-evaluator (assoc problem-metadata
                                                       :cases (:test ((:dataset-reader problem-metadata) {:n-train 0 :n-test num-cases}))
                                                       :penalty penalty
                                                       :hooks hooks))
            start-time (System/currentTimeMillis)
            evaluation (evaluator (:solution problem-metadata) nil)
            duration   (/ (- (System/currentTimeMillis) start-time) 1000.0)]
        (cond
          (> (:total-error evaluation) 0)
          (throw (ex-info (str problem-name " solution has non-zero error.") {:eval evaluation}))

          (some? (:exception evaluation))
          (throw (ex-info (str problem-name " solution threw an error.") {:eval evaluation} (:exception evaluation)))

          :else
          (log/info problem-name "passed in" duration "seconds."))))
    (log/info "Finished testing" (count problems-with-solutions) "solutions")))


(comment

  (try
    (validate-solutions {:num-cases 200})
    (catch Exception e (select-keys (:eval (ex-data e))
                                    [:push :func :code])))

    (try
    (validate-solutions {:num-cases 200})
    (catch Exception e e))

  (t/fn-type [t/INT] t/INT)
  ;;=> {:con {:sym FUNCTION1, :kind {:k-args [:* :*], :k-ret :*}}, :args [{:sym INT, :kind :*} {:sym INT, :kind :*}]}

  (g/abs-gene {:param-types [t/INT]
               :ret-type    t/BOOL})
  ;;=> {:param-types [{:sym INT, :kind :*}], :ret-type {:sym BOOL, :kind :*}}

  (g/->Abs [t/INT] t/BOOL)
  ;;=> {:param-types [{:sym INT, :kind :*}], :ret-type {:sym BOOL, :kind :*}}

  (prn
   (list (g/->Var 'input2)

         (g/->Local 0)
         (g/->Var 'input1)
         (g/->Var `get)
         (g/->App)
         :close

         (g/->Var `mapv)
         (g/->App) ;; mapv anon fn over input2

         (g/->Var `lib/int-add)
         (g/->Var `lib/reduce-vec)
         (g/->App) ;; sum results
         ))


  (comment)
  )