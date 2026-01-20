(ns erp12.cbgp-lite.benchmark.suite.sobania
  (:require [clj-fuzzy.levenshtein :as lev]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [erp12.cbgp-lite.benchmark.utils :as bu]
            [erp12.cbgp-lite.lang.lib :as lib]
            [psb2.core :as psb2]))

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

(defn process-literal-pair
  "Given pair such as [\"9\" \"int\"], return literal as required by CBGP"
  [[value-str type]]
  (let [parse-fn (case type
                   "int" parse-long
                   "float" parse-double
                   "boolean" parse-boolean
                   "string" identity)
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
      :input->type    input->type
      :ret-type       output-type
      :other-type-ctors    #{'double? 'int?}
      :extra-genes    (get-vector-of-literals problem)
      :loss-fns       (map (partial bu/penalize-nil-and-exception penalty) ;; This adds nil penalties to all loss functions
                           (case output-type
                             {:type int?} [bu/absolute-distance]
                             {:type double?} [#(bu/round 4 (bu/absolute-distance %1 %2))]
                             {:type string?} [lev/distance]))}}))

(comment

  (get-inputs-and-outputs "instance2_loc7_cc1_any")

  (get-vector-of-literals "instance2_loc7_cc1_any")

  lib/STRING
  
  (problems {:problem "instance2_loc7_cc1_any"
             :penalty 500})
  
  
  )




(defn reshape-case
  [case {:keys [out-key stdout-key]
         :or   {out-key :output1}}]
  (merge
   {:inputs (->> case
                 (filter (fn [[k _]] (str/starts-with? (name k) "input")))
                 (sort-by first)
                 (mapv second))
    :output (if (sequential? out-key)
              (vec (map #(get case %) out-key))
              (out-key case))}
   (when stdout-key
     {:std-out (stdout-key case)})))

(defn read-cases
  "Needs to take config map and return map of train and test cases."
  [{:keys [data-dir problem n-train n-test]}]
  (let [problem-info         (get (problems {}) (name problem))
        reshape              #(reshape-case % problem-info)
        {:keys [train test]} (psb2/fetch-examples (str data-dir) (str problem) n-train n-test)]
    {:train (map reshape train)
     :test  (map reshape test)}))