(ns erp12.cbgp-lite.lang.schema
  (:require [clojure.walk :as w]
            [erp12.schema-inference.impl.typeclasses :as tcs]
            [erp12.schema-inference.impl.util :as su]))

(defn decompose-typeclass
  "Given a set from schema-terms, locate typeclasses and decompose it in to possible types"
  [tctor-list]
  (mapcat (fn [x]
            (if (set? x)
              (map #(tcs/typeclasses %) x)
              (list x)))
          tctor-list))

(defn schema-terms
  "Gives a set of the possible types for a given form"
  [form]
  (->> form
       (tree-seq coll? identity)
       (map #(or (:type %)
                 (:typeclasses %)))
       (decompose-typeclass) 
       (filter some?)
       (remove map?)
       (into #{})))

(defn occurs?
  [term form]
  (let [t (transient #{})]
    (w/postwalk #(do (conj! t (= % term)) %) form)
    (contains? t true)))

(defn fn-arg-schemas
  [{:keys [type] :as schema}]
  (if (= type :scheme)
    (fn-arg-schemas (:body schema))
    (get-in schema [:input :children])))

(defn fn-ret-schema
  [{:keys [type] :as schema}]
  (if (= type :scheme)
    (fn-ret-schema (:body schema))
    (:output schema)))

(defn mgu
  [schema1 schema2]
  (su/mgu schema1 schema2))

(defn mgu-failure?
  [m]
  (su/mgu-failure? m))

(defn generalize
  [env schema]
  (su/generalize env schema))

(defn instantiate
  [schema]
  (su/instantiate schema))

(defn substitute
  [subs schema] 
  (su/substitute subs schema))

(defn compose-substitutions
  [subs1 subs2]
  (su/compose-substitutions subs1 subs2))
