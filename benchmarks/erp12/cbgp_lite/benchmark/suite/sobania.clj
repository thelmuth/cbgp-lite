(ns erp12.cbgp-lite.benchmark.suite.sobania
  (:require [clj-fuzzy.levenshtein :as lev]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [erp12.cbgp-lite.benchmark.utils :as bu]
            [erp12.cbgp-lite.lang.lib :as lib]))

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
      :input->type    input->type
      :ret-type       output-type
      :other-type-ctors    #{'double? 'int? 'boolean?}
      :extra-genes    (get-vector-of-literals problem)
      :loss-fns       (map (partial bu/penalize-nil-and-exception penalty) ;; This adds nil penalties to all loss functions
                           (case output-type
                             {:type int?} [bu/absolute-distance]
                             {:type double?} [#(bu/round 4 (bu/absolute-distance %1 %2))]
                             {:type string?} [lev/distance]))}}))

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

  (def new '#{reduce = comp erp12.cbgp-lite.lang.lib/max' input3 erp12.cbgp-lite.lang.lib/neg * double not= int erp12.cbgp-lite.lang.lib/>' if erp12.cbgp-lite.lang.lib/>=' input1 erp12.cbgp-lite.lang.lib/safe-div - range2 erp12.cbgp-lite.lang.lib/<=' fold erp12.cbgp-lite.lang.lib/safe-quot erp12.cbgp-lite.lang.lib/safe-mod mapv partial erp12.cbgp-lite.lang.lib/<' inc + abs input2 range1 erp12.cbgp-lite.lang.lib/min' range3})

  (sort new)

  (count new)
  ;;=> 31
  
  )




;; TODO:
;; x - get read-cases working
;; x - see TMH PROBLEM
;; - trim instruction set to have approx parity with Martin's grammar
