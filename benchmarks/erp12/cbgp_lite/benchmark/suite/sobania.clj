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
        (map (fn [input-string]
               (let [[input-str raw-type] (str/split input-string #":")]
                 [(symbol input-str) (string-to-cbgp-type raw-type)]))
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
  (let [{:keys [input->type output-type]} (get-inputs-and-outputs problem)]
    {problem
     {:description    "Generated problem"
      :input->type    input->type ;; TMH PROBLEM: inputs start with 0, where CBGP expects them to start with 1
      :ret-type       output-type
      :other-type-ctors    #{'double? 'int?}
      :extra-genes    (get-vector-of-literals problem)
      :loss-fns       (map (partial bu/penalize-nil-and-exception penalty) ;; This adds nil penalties to all loss functions
                           (case output-type
                             {:type int?} [bu/absolute-distance]
                             {:type double?} [#(bu/round 4 (bu/absolute-distance %1 %2))]
                             {:type string?} [lev/distance]))}}))

(defn read-cases
  "Needs to take config map and return map of train and test cases."
  [{:keys [problem]}]
  (let [[train-header & train-data] (read-csv-from-filename
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
  
  
  )




;; TODO:
;; x - get read-cases working
;; - see TMH PROBLEM
