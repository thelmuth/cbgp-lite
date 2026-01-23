(ns erp12.cbgp-lite.benchmark.suite.sobania
  (:require [clj-fuzzy.levenshtein :as lev]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [erp12.cbgp-lite.benchmark.utils :as bu]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.search.individual :as i]
            [erp12.cbgp-lite.task :as task]))

(defn read-csv-from-filename
  [filename]
  (with-open [reader (io/reader filename)]
    (doall (csv/read-csv reader))))

(defn string-to-cbgp-type
  [raw-type]
  (case raw-type
    "int" lib/INT
    "float" lib/DOUBLE
    "boolean" lib/BOOLEAN
    "string" lib/STRING))

(defn get-input->type
  [inputs]
  (into {}
        (map-indexed (fn [index input-string]
                       (let [raw-type (second (str/split input-string #":"))]
                         [(symbol (str "input" (inc index))) (string-to-cbgp-type raw-type)]))
                     inputs)))

(defn get-inputs-and-outputs
  "Given problem name (i.e. folder name in sobania_data), get the input->type
   map and return type required in the problem map"
  [problem]
  (let [csv-vec (read-csv-from-filename (str (io/file "sobania_data" problem "train.csv")))
        header (first csv-vec)
        input->type (get-input->type (butlast header))
        output-type (string-to-cbgp-type (second (str/split (last header) #":")))]
    {:input->type input->type
     :output-type output-type}))

(defn get-parse-fn
  [type]
  (case type
    "int" parse-long
    "float" parse-double
    "boolean" parse-boolean
    "string" identity))

(defn process-literal-pair
  "Given pair such as [\"9\" \"int\"], return literal as required by CBGP"
  [[value-str type]]
  (let [parse-fn (get-parse-fn type)
        value (parse-fn value-str)]
    {:gene :lit, :val value, :type (string-to-cbgp-type type)}))

(defn get-vector-of-literals
  "Given problem file, return vector of literals as required by problem map"
  [problem]
  (let [csv-vec (read-csv-from-filename (str (io/file "sobania_data" problem "literals.csv")))
        literals (rest csv-vec)]
    (mapv process-literal-pair literals)))

(defn get-solution
  "Stores CBGP genome solutions to problems for easier access."
  [problem]
  (case problem
    "instance1_loc5_cc1_any" (list {:gene :local :idx 0}
                                   {:gene :lit :type {:type 'int?} :val -5}
                                   {:gene :var :name '*}
                                   {:gene :apply} ;; input0 * -5 ;; line 3 besides subtraction

                                   {:gene :local :idx 0}
                                   {:gene :local :idx 2}
                                   {:gene :var :name '+}
                                   {:gene :apply} ;; input0 + input2 ;; line 2

                                   {:gene :var :name '-}
                                   {:gene :apply} ;; (input0 + input2) - (input0 * -5) ;; line 3

                                   {:gene :local :idx 1}
                                   {:gene :var :name 'abs}
                                   {:gene :apply} ;; line 4 besides addition

                                   {:gene :var :name '+}
                                   {:gene :apply} ;; line 4, which is returned
                                   )

    "instance6_loc5_cc2_if_only" (list {:gene :local :idx 1}
                                       {:gene :var :name 'abs}
                                       {:gene :apply} ;; line 1 to make var0. This is for else clause

                                       {:gene :lit :type {:type 'int?} :val 5}
                                       {:gene :local :idx 0}
                                       {:gene :local :idx 0}
                                       {:gene :var :name '*}
                                       {:gene :apply}
                                       {:gene :var :name `lib/safe-mod}
                                       {:gene :apply}
                                       {:gene :lit :type {:type 'int?} :val 3}
                                       {:gene :var :name '+}
                                       {:gene :apply} ;; all of line 3 except for var0 *=

                                       {:gene :local :idx 1}
                                       {:gene :var :name 'abs}
                                       {:gene :apply} ;; line 1 to make var0. This is for *= in then clause

                                       {:gene :var :name '*}
                                       {:gene :apply} ;; full line 3

                                       {:gene :lit :type {:type 'int?} :val 10}
                                       {:gene :local :idx 2}
                                       {:gene :var :name `lib/<='}
                                       {:gene :apply} ;; condition to if
                                       {:gene :var :name 'if}
                                       {:gene :apply} ;; if statement line 14

                                       {:gene :local :idx 0}
                                       {:gene :var :name '*}
                                       {:gene :apply} ;; line 4

                                       {:gene :lit :type {:type 'int?} :val 5}
                                       {:gene :var :name '*}
                                       {:gene :apply} ;; line 5, which is returned
                                       )

    #_(reduce (fn [var0 i0]
                (+ var0
                   (* i0 2)))
              input0
              (range (+ 1 (abs input2))))

    "solution" '(min' (* input1 input2)
                      (safe-quot (reduce (fn [a-18590 a-18591] 
                                           (+ a-18590 (* 2 a-18591)))
                                         input1
                                         (rangev (+ 1 (abs input3))))
                                 (+ 2 (safe-mod (abs input3) 2))))

    "instance9_loc7_cc2_for_only" (list {:gene :lit :type {:type 'int?} :val 2} ;; 2 for the mod
                                        {:gene :local :idx 2}
                                        {:gene :var :name 'abs}
                                        {:gene :apply} ;; abs(var2), which is the same as abs(arg2)

                                        {:gene :var :name `lib/safe-mod}
                                        {:gene :apply} ;; abs(var2) % 2

                                        {:gene :lit :type {:type 'int?} :val 2}
                                        {:gene :var :name '+}
                                        {:gene :apply} ;; abs(var2) % 2 + 2

                                        ;; the above calculates abs(var2) % 2 + 2 for line 6 first, since it's the denom for division, it must be on the AST stack first

                                        {:gene :local :idx 2}
                                        {:gene :var :name 'abs}
                                        {:gene :apply} ;; (abs input2)

                                        {:gene :lit :type {:type 'int?} :val 1}
                                        {:gene :var :name '+}
                                        {:gene :apply} ;; (+ 1 (abs input2))

                                        {:gene :var :name 'range1}
                                        {:gene :apply} ;; (range (+ 1 (abs input2)))

                                        {:gene :local :idx 0} ;; input0, as initial value for reduce/fold

                                        {:gene :fn :arg-types [lib/INT lib/INT] :ret-type lib/INT} ;; start fn
                                        {:gene :local :idx 4} ;; i0
                                        {:gene :lit :type {:type 'int?} :val 2}
                                        {:gene :var :name '*}
                                        {:gene :apply} ;; (* i0 2)
                                        {:gene :local :idx 3} ;; var0
                                        {:gene :var :name '+}
                                        {:gene :apply} ;; (+ var0 (* i0 2)) 
                                        {:gene :close} ;; (fn [var0 i0] (+ var0 (* i0 2)))

                                        {:gene :var :name 'fold}
                                        {:gene :apply}

                                        ;; so far, this compiles to the following, which looks correct
                                        #_(reduce (fn [a-18554 a-18555] (+ a-18554 (* 2 a-18555)))
                                                  input0
                                                  (erp12.cbgp-lite.lang.lib/rangev (+ 1 (abs input2))))

                                        ;; this is var0 from line 18, so we're now ready for the //=
                                        {:gene :var :name `lib/safe-python-quot}
                                        {:gene :apply}

                                        ;; now calculate var1
                                        {:gene :local :idx 1}
                                        {:gene :local :idx 0}
                                        {:gene :var :name '*}
                                        {:gene :apply}

                                        ;; finally return min of top two ASTs
                                        {:gene :var :name `lib/min'}
                                        {:gene :apply} ;; returned
                                        )))

(defn problems
  "Needs to take config map and return a map of problem names (strings)
   to maps (info about the problem). Probably fine to just return the one
   problem's info that is being used.
   
   Assumes problem is name of the folder in sobania_data"
  [{:keys [penalty problem]}] 
  (let [problem (name problem)
        {:keys [input->type output-type]} (get-inputs-and-outputs problem)]
    {problem
     {:description    "Generated problem"
      :n-train        200
      :n-test         1000
      :input->type    input->type
      :ret-type       output-type
      :other-type-ctors    #{'double? 'int? 'boolean?}
      :extra-genes    (get-vector-of-literals problem)
      :loss-fns       (map (partial bu/penalize-nil-and-exception penalty) ;; This adds nil penalties to all loss functions
                           (case output-type
                             {:type int?} [bu/absolute-distance]
                             {:type double?} [#(bu/round 4 (bu/absolute-distance %1 %2))]
                             {:type string?} [lev/distance]))
      :solution (get-solution problem)}}))

(defn read-cases
  "Needs to take config map and return map of train and test cases."
  [{:keys [problem]}]
  (let [problem (name problem)
        [train-header & train-data] (read-csv-from-filename
                                     (str (io/file "sobania_data" problem "train.csv")))
        [_ & test-data]             (read-csv-from-filename
                                     (str (io/file "sobania_data" problem "test.csv")))
        parse-fns (map #(get-parse-fn
                         (last (str/split % #":")))
                       train-header)
        parse-set (fn [data] ;; function to parse and format train/test set using parse-fns
                    (map (fn [row]
                           (let [parsed-row (map #(%2 %1)
                                                 row
                                                 parse-fns)]
                             {:inputs (vec (butlast parsed-row))
                              :output (last parsed-row)}))
                         data))
        train (parse-set train-data)
        test  (parse-set test-data)]
    {:train train
     :test  test}))

(defn validate-solutions
  [{:keys [num-cases problem]}]
  (let [suite (problems {:problem problem
                         :penalty 1000})]
    (doseq [[problem-name task] (filter (fn [[_ task]] (contains? task :solution)) suite)]
      (println "\nStarting" problem-name)
      (let [factory    (i/make-evaluator (-> task
                                             task/enhance-task
                                             (assoc :evaluate-fn i/evaluate-full-behavior
                                                    :cases (:test (read-cases {:problem  problem-name
                                                                               :n-test   num-cases
                                                                               :n-train  0})))))
            start-time (System/currentTimeMillis)
            evaluation (factory (:solution task) nil)
            duration   (/ (- (System/currentTimeMillis) start-time) 1000.0)]
        (cond
          (> (:total-error evaluation) 0)
          (throw (ex-info (str problem-name " solution has non-zero error.") {:eval evaluation}))

          (some? (:exception evaluation))
          (throw (ex-info (str problem-name " solution threw an error.") {:eval evaluation} (:exception evaluation)))

          :else
          (println problem-name "passed in" duration "seconds."))))))

(comment

  (validate-solutions {:num-cases 100
                       :problem "instance1_loc5_cc1_any"})

  (validate-solutions {:num-cases 100
                       :problem "instance6_loc5_cc2_if_only"})

  (count (get-solution "instance6_loc5_cc2_if_only"))

  (validate-solutions {:num-cases 100
                       :problem "instance9_loc7_cc2_for_only"})

  '(min' (* input1 input2)
         (safe-quot (reduce (fn [a-18590 a-18591]
                              (+ a-18590 (* 2 a-18591)))
                            input1
                            (rangev (+ 1 (abs input3))))
                    (+ 2 (safe-mod (abs input3) 2))))

  (reduce (fn [a-18590 a-18591]
            (+ a-18590 (* 2 a-18591)))
          -100
          (range (+ 1 (abs 5))))

  (lib/safe-quot -100 3)
  ;;=> -33 ;;; lol, python's -100 // 3 gives -34, so they round opposite directions

  (int -33.8)

  (count
   (remove zero? [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 100 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))

  
  (lib/safe-python-quot 100 3)
  ;;=> 33
  (lib/safe-python-quot -100 3)
  ;;=> -34

  )


(comment

  (get-inputs-and-outputs "instance2_loc7_cc1_any")

  (get-vector-of-literals "instance2_loc7_cc1_any")

  lib/STRING

  (problems {:problem "instance2_loc7_cc1_any"
             :penalty 500})

  (read-cases {:problem "instance2_loc7_cc1_any"})

  "instance1_loc5_cc1_any"

  ;; old instruction set
  (def old '#{erp12.cbgp-lite.lang.lib/keys-set reduce contains? set clojure.set/intersection erp12.cbgp-lite.lang.lib/reverse' erp12.cbgp-lite.lang.lib/mapcat' first erp12.cbgp-lite.lang.lib/rest' erp12.cbgp-lite.lang.lib/->map last = erp12.cbgp-lite.lang.lib/keys-vec dec erp12.cbgp-lite.lang.lib/concat' erp12.cbgp-lite.lang.lib/sortv-by ->vector2 erp12.cbgp-lite.lang.lib/and erp12.cbgp-lite.lang.lib/assoc-right erp12.cbgp-lite.lang.lib/butlast' nth-or-else ->map1 comp disj erp12.cbgp-lite.lang.lib/max' erp12.cbgp-lite.lang.lib/square erp12.cbgp-lite.lang.lib/remove' ->set3 left input3 erp12.cbgp-lite.lang.lib/atan ->map2 erp12.cbgp-lite.lang.lib/safe-nth zipmap erp12.cbgp-lite.lang.lib/neg * erp12.cbgp-lite.lang.lib/floor erp12.cbgp-lite.lang.lib/vals-vec get erp12.cbgp-lite.lang.lib/remove-element double erp12.cbgp-lite.lang.lib/sin empty? erp12.cbgp-lite.lang.lib/safe-acos erp12.cbgp-lite.lang.lib/safe-asin not= erp12.cbgp-lite.lang.lib/conj' erp12.cbgp-lite.lang.lib/occurrences-of vec int erp12.cbgp-lite.lang.lib/ceil erp12.cbgp-lite.lang.lib/sort' group-by erp12.cbgp-lite.lang.lib/>' erp12.cbgp-lite.lang.lib/or if ->set1 erp12.cbgp-lite.lang.lib/>=' input1 erp12.cbgp-lite.lang.lib/int-ceil erp12.cbgp-lite.lang.lib/safe-div - range2 erp12.cbgp-lite.lang.lib/<=' zero? erp12.cbgp-lite.lang.lib/assoc-left get-or-else fold update erp12.cbgp-lite.lang.lib/safe-sub-coll clojure.set/difference not ->map3 ->set2 clojure.set/superset? erp12.cbgp-lite.lang.lib/safe-quot ->vector1 erp12.cbgp-lite.lang.lib/safe-mod erp12.cbgp-lite.lang.lib/tan erp12.cbgp-lite.lang.lib/replace-first' merge mapv clojure.set/subset? erp12.cbgp-lite.lang.lib/take' erp12.cbgp-lite.lang.lib/index-of partial erp12.cbgp-lite.lang.lib/<' erp12.cbgp-lite.lang.lib/safe-log10 inc erp12.cbgp-lite.lang.lib/safe-log2 clojure.set/union + abs erp12.cbgp-lite.lang.lib/distinctv erp12.cbgp-lite.lang.lib/pow erp12.cbgp-lite.lang.lib/safe-assoc-nth input2 erp12.cbgp-lite.lang.lib/safe-sqrt range1 erp12.cbgp-lite.lang.lib/in? erp12.cbgp-lite.lang.lib/min' count erp12.cbgp-lite.lang.lib/replace' assoc ->vector3 range3 erp12.cbgp-lite.lang.lib/cos erp12.cbgp-lite.lang.lib/mapv-indexed erp12.cbgp-lite.lang.lib/map2v right erp12.cbgp-lite.lang.lib/filter' erp12.cbgp-lite.lang.lib/map-set ->tuple2 erp12.cbgp-lite.lang.lib/int-floor})

  (def new '#{reduce = comp erp12.cbgp-lite.lang.lib/max' input3 erp12.cbgp-lite.lang.lib/neg * double not= int erp12.cbgp-lite.lang.lib/>' if erp12.cbgp-lite.lang.lib/>=' input1 erp12.cbgp-lite.lang.lib/safe-div - range2 erp12.cbgp-lite.lang.lib/<=' fold erp12.cbgp-lite.lang.lib/safe-quot erp12.cbgp-lite.lang.lib/safe-mod mapv partial erp12.cbgp-lite.lang.lib/<' + abs input2 range1 erp12.cbgp-lite.lang.lib/min' range3})

  (sort new)

  (count new)
  ;;=> 30
  
  )

