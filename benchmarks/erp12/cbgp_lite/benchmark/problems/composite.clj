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
  {"centimeters-to-meters"
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
                          (g/->App)
                          )}

   "set-symmetric-difference"
   {:description    "Given two sets, find the symmetric difference."
    :input-symbols  (input-symbols 2)
    :input-types    [(t/set-type t/INT) (t/set-type t/INT)]
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

   "sum-2-vals"
   {:description    (str "Given a map from strings to ints and two strings that are "
                         "keys of the map, look up the values associated with those keys "
                         "in the map and return their sum.")
    :input-symbols  (input-symbols 3)
    :input-types    [(t/map-type t/STRING t/INT)
                     t/STRING
                     t/STRING]
    :output-type    t/INT
    :type-ctors     #{t/MAP t/STRING t/INT t/BOOL}
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
    :type-ctors     #{t/MAP t/INT t/BOOL t/STRING t/CHAR t/FLOAT}
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

  ;
   })


(defn validate-solutions
  [{:keys [num-cases penalty hooks]
    :or   {penalty 1000
           hooks   {}}}]
  (doseq [[problem-name problem-metadata] (filter (fn [[_ md]] (contains? md :solution)) problems)]
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
        (log/info problem-name "passed in" duration "seconds.")))))


(comment

  (try
    (validate-solutions {:num-cases 200})
    (catch Exception e (:eval (ex-data e))))



  (t/fn-type [t/INT] t/INT)
  ;;=> {:con {:sym FUNCTION1, :kind {:k-args [:* :*], :k-ret :*}}, :args [{:sym INT, :kind :*} {:sym INT, :kind :*}]}
  
  

  ;; Does Eddie's do this?
  ;; 
  ;; Same error in other runs? Always let?
  ;; 
  ;; Do any of my problems need anon fns
  ;; 
  ;; Do we have the type sig for partial (or let?) wrong?
  

  (comment)
  )