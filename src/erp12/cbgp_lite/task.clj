(ns erp12.cbgp-lite.task
  (:require [clojure.set :as set]
            [erp12.cbgp-lite.lang.compile :as compile]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.lang.schema :as schema]
            [erp12.cbgp-lite.search.plushy :as pl]
            [erp12.cbgp-lite.utils :as u]))

;; @TODO Move to `benchmarks/`

(defn arg-symbols
  [{:keys [input->type]}]
  (vec (sort (keys input->type))))

(arg-symbols {:input->type {'input1 {:type :vector
                                     :child {:type 'int?}}
                            'input2 {:type 'int?}}
              })

(defn task-type-ctors
  [{:keys [input->type ret-type other-type-ctors] :or {other-type-ctors #{}}}]
  (->> (schema/schema-terms {:type :=>
                             :input {:type :cat
                                     :children (vec (vals input->type))}
                             :output ret-type})
       (set/union (set other-type-ctors))
       (remove #{:cat :s-var :scheme})
       (set)))

(defn type-environment
  [{:keys [input->type type-ctors]}]
  (merge (lib/lib-for-type-ctors type-ctors)
         input->type))

(defn default-gene-distribution
  []
  ;; @todo Calibrate by analyzing real code.
  {:var           0.2
   :local         0.2
   :lit           0.2
   :lit-generator 0.1
   :apply         (case @compile/app-type
                    :original 0.2
                    :dna 0.025
                    :all 0
                    :baked-in 0.025
                    (throw (Exception. (str "Unrecognized Application Type of " @compile/app-type))))
   :fn            0.025
   :let           0.025
   :close         0.05
   :dna           (case @compile/app-type
                    :original 0
                    :dna 0.1
                    :all 0
                    :baked-in 0)})

(defn default-genetic-source
  [{:keys [types vars extra-genes]}]
  (pl/make-genetic-source
    (pl/prob-by-gene-kind (concat (map (fn [v] {:gene :var :name v}) vars)
                                  ;; Task-specific genes
                                  extra-genes
                                  ;; 1-arg functions
                                  (for [arg types ret types]
                                    {:gene :fn :arg-types [arg] :ret-type ret})
                                  ;; 2-arg functions
                                  (for [arg1 types
                                        arg2 types
                                        ret types]
                                    {:gene :fn :arg-types [arg1 arg2] :ret-type ret})
                                  ;; Always used genes
                                  [{:gene :local}
                                   {:gene :apply}
                                   {:gene :dna}
                                   {:gene :let}
                                   {:gene :close}])
                          (default-gene-distribution))))

(defn enhance-task
  [opts]
  (-> opts
      (assoc :dealiases lib/dealiases)
      (u/enhance
        ;; The size of an individual's error vector
       :num-errors (fn [{:keys [train loss-fns stdout-key]}]
                     (+ (* (count train) (count loss-fns))
                        (if (nil? stdout-key) 0 (count train))))
        ;; Create a sequence of program argument symbols
       :arg-symbols arg-symbols
        ;; Find all types related to the task
       :type-ctors task-type-ctors
       ;; Derive the full type-environment used to compile programs from genomes.
       :type-env type-environment
       ;; Find the set of all variables that leverage to task's types.
       ;; Includes generic functions.
       :vars (fn [{:keys [type-env]}] (set (keys type-env)))
        ;; Derive the genetic source.
       :genetic-source default-genetic-source
        ;; Derive a function for generating genomes.
       :genome-factory (fn [opts] #(pl/random-plushy-genome opts)))))