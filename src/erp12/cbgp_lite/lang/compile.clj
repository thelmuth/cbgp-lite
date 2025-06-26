(ns erp12.cbgp-lite.lang.compile
  (:require [clojure.string :as str]
            [clojure.walk :as w]
            [erp12.cbgp-lite.lang.ast :as a]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.lang.schema :as schema]
            [taoensso.timbre :as log]
            [taoensso.timbre.appenders.core :as log-app]
            [clojure.pprint]))

(def collect-types? (atom false))
(def types-seen (atom {}))


;; Here is where the default function application type is defined. By default, it is :original. To change it, add the command-line argument :app-type along with the type
;; that you want to use. The options are :original, :all, ;dna, and :baked-in. For :baked-in, the baked-in apply probability also has to be defined. By default, it is set to 0.5.
;; To change it, add the command-line argument :baked-in-apply-probability as well as the value you want.
;; If you want to use the original apply strategy, neither of these two command line arguments have to be provided.
;; Backtracking is the same as :app-type and :baked-in-apply-probability. In the command line, type :backtracking followed by true or false to enable or disable it.
(def app-type (atom :original))
(def baked-in-apply-probability (atom 0.5))
(def backtracking (atom false))


;; @todo Move to schema-inference
(defn tap-nodes
  [f tree]
  (w/walk (partial tap-nodes f) identity (f tree)))


;; @todo Move to schema-inference
(defn s-vars
  [schema]
  (let [x (transient #{})]
    (tap-nodes
     (fn [node]
       (when (= (:type node) :s-var)
         (conj! x (:sym node)))
       node)
     schema)
    (persistent! x)))



(defn canonical-type
  [type]
  (let [subs (into {} (map-indexed (fn [i s] [s (symbol (str "S" i))])
                                   (sort (s-vars type))))]
    (w/postwalk-replace subs type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stack AST sizes

(def sketches (atom {}))



(defn record-asts!
  [state]
  (let [sketch (->> state
                    :asts
                    (map (fn [{::keys [ast type]}]
                           {:root (:op ast)
                            :size (a/ast-size ast)
                            :type type})))]
    (swap! sketches
           #(assoc % (->> state
                          :asts
                          (map (fn [{::keys [ast type]}]
                                 {:root (:op ast)
                                  :size (a/ast-size ast)
                                  :type type})))
                   (inc (or (get % sketch) 0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State Manipulation

;; This is an empty state. It serves to be a starting point.
(def empty-state
  {:asts    (list)
   :push    []
   :locals  []
   ;; @todo Experimental
   :biggest :none
   :newest  :none
   :dna 0
   :fn-applied 0
   :fn-not-applied 0
   :total-apply-attempts 0
   :fn-not-applied-because-no-functions 0})



(defn macro?
  [{:keys [op] :as ast}]
  (let [sym (case op
              :var (:var ast)
              :local (:name ast)
              :VAR (:sym ast)
              nil)]
    (if sym
      (contains? lib/macros sym)
      false)))



(defn unifiable?
  [unify-with typ]
  (not (schema/mgu-failure? (schema/mgu unify-with typ))))



(declare compile-step)



;; Push to the stack. The state, everything involved in compilation.
(defn push-ast
  "Push the `ast` to the AST stack in the `state`."
  [ast {:keys [biggest newest ret-type dna] :as state}]

  
  
  ;; collect-types is an atom that is false by default.
  ;; If set to true. start recording into atom "types-seen".
  ;; Keeps track of how much every single data type was used.
  (when @collect-types?
    (swap! types-seen
           (fn [m t] (assoc m t (inc (get m t 0))))
           (canonical-type (::type ast))))

  
  
  ;; output-able? checks if the output type of the tree youre checking has the same output type as the problem.
  (let [output-able? (and (unifiable? ret-type (::type ast))
                          (not (macro? (::ast ast))))
        newest-out-ast (if output-able? ast newest)
        biggest-out-ast (if (and output-able?
                                 (or (= biggest :none)
                                     (> (a/ast-size (::ast ast))
                                        (a/ast-size (::ast biggest)))))
                          ast
                          biggest)]

    
    ;; Below is where app-type is checked and used. Depending on the app-type chosen, will apply in a different way.
    (cond
      (= @app-type :all)
      (compile-step {:push-unit {:gene :apply}
                     :state (assoc state
                                   :asts (conj (:asts state) ast)
                                   :biggest biggest-out-ast
                                   :newest newest-out-ast)})

      
      ;; The original apply strategy used in cbgp-lite. Will apply when an apply gene is found.
      (= @app-type :original)
      (assoc state
             :asts (conj (:asts state) ast)
             :biggest biggest-out-ast
             :newest newest-out-ast)

      
      ;; The do-not-apply strategy. By default, functions are applied. When a :dna gene is found, the :dna counter increases by 1. If a function is found
      ;; and the :dna counter is 0, an apply attempt is made. Otherwise, it remains on the stack.
      (= @app-type :dna)
      (if (= (:op (::ast ast)) :var)
        (if (= (:dna state) 0)
          (compile-step {:push-unit {:gene :apply}
                         :state (assoc state
                                       :asts (conj (:asts state) ast)
                                       :biggest biggest-out-ast
                                       :newest newest-out-ast
                                       :dna 0)})
          (assoc state
                 :asts (conj (:asts state) ast)
                 :biggest biggest-out-ast
                 :newest newest-out-ast
                 :dna (dec dna)))
        (assoc state
               :asts (conj (:asts state) ast)
               :biggest biggest-out-ast
               :newest newest-out-ast
               :dna dna))

      
      ;; The baked-in apply strategy. Each gene has an attribute :apply-it, which is randomly assigned to either true or false depending on the :baked-in-apply-probability value set by
      ;; the command line argument. If true, an apply attempt is made. Otherwise, it will remain on the stack as is. There are still some :apply genes, so these functions still have a 
      ;; chance of being applied.
      (= @app-type :baked-in)
      (if (:apply-it state)
        (compile-step {:push-unit {:gene :apply}
                       :state (assoc (dissoc state :apply-it)
                                     :asts (conj (:asts state) ast)
                                     :biggest biggest-out-ast
                                     :newest newest-out-ast)})
        (assoc (dissoc state :apply-it)
               :asts (conj (:asts state) ast)
               :biggest biggest-out-ast
               :newest newest-out-ast))

      
      ;; If the application type set by the user isn't one of the valid types, an exception is thrown.
      :else
      (throw (Exception. (str "Unrecognized Application Type of " app-type))))))



(defn nth-local
  "Get the nth variable from the state using modulo to ensure `n` always selects a
  variable unless no variables are bound in the state. If there are no variables,
  returns nil."
  [n state]
  (let [locals (get state :locals)]
    (if (empty? locals)
      nil
      (nth locals (mod n (count locals))))))



(defn pop-ast
  "Get the top AST from the ast stack of `state`.

  Returns a map of 2 elements:
    :ast - The popped AST or `:none` if no AST is found.
    :state - The state without the popped AST. If no AST found, the state is unchanged."
  ([state]
   (pop-ast state {}))
  ([state {:keys [allow-macros] :or {allow-macros false}}]
   (loop [remaining (:asts state)
          acc []]
     (let [ast (first remaining)]
       (cond
         (empty? remaining)
         {:ast   :none
          :state state}

         (or (not (macro? (::ast ast))) allow-macros)
         {:ast   ast
          :state (assoc state :asts (concat acc (rest remaining)))}

         :else
         (recur (rest remaining)
                (conj acc ast)))))))



(defn pop-unifiable-ast
  "Get the first AST (from the top) that is unifiable with the given schema.

  Returns a map of 3 elements:
    :ast - The popped AST or `:none` if no AST is found.
    :state - The state without the popped AST. If no AST found, the state is unchanged.
    :bindings - A map of type substitutions used to unify the types."
  ([unify-with state]
   (pop-unifiable-ast unify-with state {}))
  ([unify-with state {:keys [allow-macros] :or {allow-macros false}}]
   (loop [remaining (:asts state)
          acc []]
     (if (empty? remaining)
       {:ast      :none
        :state    state
        :bindings {}}
       (let [ast (first remaining)
             subs (schema/mgu unify-with (::type ast))]
         (if (and (not (schema/mgu-failure? subs))
                  (or allow-macros
                      (not (macro? (::ast ast)))))
           {:ast      ast
            :state    (assoc state :asts (concat acc (rest remaining)))
            :bindings subs}
           (recur (rest remaining)
                  (conj acc ast))))))))



(defn pop-function-ast
  "Pops the top function AST regardless of argument/return types.
  See `pop-ast` for return structure."
  [state]
  (loop [remaining (:asts state)
         acc []]
    (if (empty? remaining)
      {:ast   :none
       :state state}
      (let [ast (first remaining)
            schema-type (get-in ast [::type :type])
            schema-type (if (= schema-type :scheme)
                          (get-in ast [::type :body :type])
                          schema-type)]
        (if (= schema-type :=>)
          {:ast   ast
           :state (assoc state :asts (concat acc (rest remaining)))}
          (recur (rest remaining)
                 (conj acc ast)))))))



(defn all-pop-function-ast
  "Pops all of the function ASTs regardless of argument/return types. Starts with an empty list. If a function is found,
   a map containing the function ast as well as the state with that function popped is added to a list.
   At the end, a list containing the maps of the functions and states with the function popped is returned."
  [state]

  (loop [remaining (:asts state)
         acc []
         funclist (list)]
    (if (empty? remaining)
      (reverse funclist)
      (let [ast (first remaining)
            schema-type (get-in ast [::type :type])
            schema-type (if (= schema-type :scheme)
                          (get-in ast [::type :body :type])
                          schema-type)]
        (recur (rest remaining)
               (conj acc ast)
               (if (= schema-type :=>)
                 (conj funclist {:ast   ast
                                 :state (assoc state :asts (concat acc (rest remaining)))})
                 funclist))))))



(defn pop-all-unifiable-asts
  "Pops every ast that is unifiable with the given schema. Does the same thing as pop-unifiable-ast, but instead of
   returning a map of the ast, the state, and bindings, it adds this map to a list. When every ast is checked and none remain, the list of maps is reversed and returned."
  ([unify-with state bindings]
   (pop-all-unifiable-asts unify-with state bindings {}))
  ([unify-with state bindings {:keys [allow-macros] :or {allow-macros false}}]
   (loop [remaining (:asts state)
          acc []
          unifiable-list '()]
     (if (empty? remaining)
       (reverse unifiable-list)

       (let [ast (first remaining)
             subs (schema/mgu unify-with (::type ast))]
         ;; If the bindings doesn't change, we don't need to try backtracking, and we only need to return the first unified AST.
         (if (and (= subs bindings) (empty? unifiable-list))
           (list {:ast      ast
                  :state    (assoc state :asts (concat acc (rest remaining)))
                  :bindings subs})

           (recur (rest remaining)
                  (conj acc ast)
                  (if (and (not (schema/mgu-failure? subs))
                           (or allow-macros
                               (not (macro? (::ast ast)))))
                    (conj unifiable-list {:ast      ast
                                          :state    (assoc state :asts (concat acc (rest remaining)))
                                          :bindings subs})
                    unifiable-list))))))))



(defn pop-push-unit
  [state]
  {:push-unit (first (:push state))
   :state     (update state :push rest)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Push to AST Compilation

(declare push->ast)



(defmulti compile-step (fn [{:keys [push-unit]}] (:gene push-unit)))



(defmethod compile-step :lit
  [{:keys [push-unit state]}]
  ;; Literals are pushed directly to the AST stack.
  ;; Type annotation is taken from push-unit.
  (let [{:keys [val type]} push-unit]
    (push-ast {::ast  {:op :const :val val}
               ::type type}
              state)))



(defmethod compile-step :var
  [{:keys [push-unit state type-env]}]
  ;; Vars are pushed directly to the AST stack.
  ;; Type is taken from the type environment.
  (push-ast {::ast  {:op :var :var (symbol (:name push-unit))}
             ::type (schema/instantiate (get type-env (:name push-unit)))}
            state))



(defmethod compile-step :local
  [{:keys [push-unit state type-env]}]
  ;; Local variable numbers are mapped to a symbol using modulo logic and
  ;; then pushed to the AST stack.
  (let [local-symbol (nth-local (:idx push-unit) state)]
    (if (nil? local-symbol)
      state
      (push-ast {::ast  {:op :local :name local-symbol}
                 ::type (schema/instantiate (get type-env local-symbol))}
                state))))







(defn try-arguments
  "Attempts to apply "
  [remaining-arg-types bindings args new-state fn-ast fn-type]

  (if (empty? remaining-arg-types)
            ;; Push an AST which calls the function to the arguments and
            ;; box the AST with the return type of the function.
    (let [ret-s-var {:type :s-var :sym (gensym "s-")}
          subs (schema/mgu (schema/substitute bindings fn-type)
                           {:type   :=>
                            :input  {:type     :cat
                                     :children (mapv ::type args)}
                            :output ret-s-var})]
      (if (schema/mgu-failure? subs)
        nil

        (push-ast {::ast  {:op   :invoke
                           :fn   fn-ast
                           :args (mapv ::ast args)}
                   ::type (schema/substitute subs ret-s-var)}
                  new-state)))

    ;; Grab next arg we need to find. If we know what the bindings are, we need to substitute those in.
    (let [arg-type (first remaining-arg-types)
          _ (log/trace "Searching for arg of type:" arg-type)
                  ;; If arg-type is a t-var that we have seen before,
                  ;; bind it to the actual same type as before.
          arg-type (schema/substitute bindings arg-type)
          _ (log/trace "In-context arg type:" arg-type)
                  ;; is-s-var (= (:type arg-type) :s-var)
                  ;; If arg-type is still a t-var, pop an ast of any type.
                  ;; Otherwise, pop the AST of the expected type.
                  ;; The ARG ast. :bindings may contain the new bindings for things like type A, B etc.
          all-unifiable (pop-all-unifiable-asts arg-type new-state bindings)
          _ (log/trace "ALL UNIFIABLE: " all-unifiable)]

      (loop [all-unifiable all-unifiable]
        (if (empty? all-unifiable)
          nil ;; didn't find an argument that works

          (let [{arg :ast state-arg-popped :state new-subs :bindings} (first all-unifiable)
                _ (log/trace "Trying arg:" arg)
                _ (log/trace "With bindings:" new-subs)
                result (try-arguments (rest remaining-arg-types)
                       ;; If arg-type is has unbound t-vars that were bound during unification,
                       ;; add them to the set of bindings.
                       ;; merge these two together.
                                      (schema/compose-substitutions new-subs bindings)
                                      (conj args arg)
                                      state-arg-popped
                                      fn-ast
                                      fn-type)]
            (if (some? result)
              result
              (recur (rest all-unifiable)))))))))



(defn try-apply
  "Tries to apply a function to the state. If fails, returns the original state."
  [{boxed-ast :ast state-fn-popped :state}]

  (log/trace "Applying function:" boxed-ast)

  ;; function ast: clojure code that returns function. the data type of that function to find the right asts.
  (let [{fn-ast ::ast fn-type ::type} boxed-ast
        remaining-arg-types (schema/fn-arg-schemas fn-type)]
    (try-arguments remaining-arg-types {} [] state-fn-popped fn-ast fn-type)))



(defn original-compile-step-apply
  [{:keys [state]}]
  ;; Function applications search for the first AST that returns a function.
  ;; If none found, return state.
  ;; If found, proceed to search for ASTs for each argument to the function.
  ;; If one or more arguments have :s-var types, incrementally bind them.

  ;; pop function off of state. Finds first function.
  ;; (clojure.pprint/pprint (map #(:ast %) (all-pop-function-ast state)))

  (let [{boxed-ast :ast state-fn-popped :state} (pop-function-ast state)]
    (log/trace "Applying function:" boxed-ast)

    ;; if the boxed ast is not found. just return the input state.
    ;; Do nothing. A function wasn't found.
    (if (= :none boxed-ast)
      (update (update state :fn-not-applied-because-no-functions inc) :total-apply-attempts inc)
      ;; function ast: clojure code that returns function. the data type of that function to find the right asts.
      (let [{fn-ast ::ast fn-type ::type} boxed-ast]

        ;; empty map of bindings. don't have anything yet, but when we find out what "type A" is, we know what type A is.
        ;; there are no args yet. Slowly loop through and find the bindings. Once you know the info, you know the 
        (loop [remaining-arg-types (schema/fn-arg-schemas fn-type)
               bindings {}
               args []
               new-state state-fn-popped]
          (if (empty? remaining-arg-types)
            ;; Push an AST which calls the function to the arguments and
            ;; box the AST with the return type of the function.
            (let [ret-s-var {:type :s-var :sym (gensym "s-")}
                  subs (schema/mgu (schema/substitute bindings fn-type)
                                   {:type   :=>
                                    :input  {:type     :cat
                                             :children (mapv ::type args)}
                                    :output ret-s-var})]
              (if (schema/mgu-failure? subs)
                (update (update state :fn-not-applied inc) :total-apply-attempts inc)

                ;; Push the ast to the stack
                (push-ast {::ast  {:op   :invoke
                                   :fn   fn-ast
                                   :args (mapv ::ast args)}
                           ::type (schema/substitute subs ret-s-var)}
                          (update (update new-state :fn-applied inc) :total-apply-attempts inc))))

            ;; Grab next arg we need to find. If we know what the bindings are, we need to substitute those in.
            (let [arg-type (first remaining-arg-types)
                  _ (log/trace "Searching for arg of type:" arg-type)
                  ;; If arg-type is a t-var that we have seen before,
                  ;; bind it to the actual same type as before.
                  arg-type (schema/substitute bindings arg-type)
                  _ (log/trace "In-context arg type:" arg-type)
                  ;; is-s-var (= (:type arg-type) :s-var)
                  ;; If arg-type is still a t-var, pop an ast of any type.
                  ;; Otherwise, pop the AST of the expected type.
                  ;; The ARG ast. :bindings may contain the new bindings for things like type A, B etc.
                  {arg :ast state-arg-popped :state new-subs :bindings}
                  (pop-unifiable-ast arg-type new-state)]
              (log/trace "Found arg:" arg)
              (if (= :none arg)
                ;; if arg is :none, not anything at all, just return the state.
                (update (update state :fn-not-applied inc) :total-apply-attempts inc)

                (recur (rest remaining-arg-types)
                       ;; If arg-type is has unbound t-vars that were bound during unification,
                       ;; add them to the set of bindings.
                       ;; merge these two together.
                       (schema/compose-substitutions new-subs bindings)
                       (conj args arg)
                       state-arg-popped)))))))))

;;todo: add a conditional argument here that checks command line arg :backtrack. If backtrack is true then do this way, otherwise, use old method of apply.

(defmethod compile-step :apply
  [{:keys [state] :as wholemap}]
  ;; Checks the backtracking atom. If it is true, then backtracking will be used, otherwise, the original apply function is used.
  (cond
    
    ;; The backtracking method
    (= @backtracking true)
    (let [allfuncs (all-pop-function-ast state)
          allfuncsinfo (map try-apply allfuncs)
          able-to-be-applied (filter some? allfuncsinfo)
          firstapplied (first able-to-be-applied)]

      (if (empty? able-to-be-applied)
        (update (update state :fn-not-applied inc) :total-apply-attempts inc)
        (update (update firstapplied :fn-applied inc) :total-apply-attempts inc)))

    
    ;; No backtracking
    (= @backtracking false)
    (original-compile-step-apply wholemap)


    :else
    (throw (Exception. (str "Unrecognized Backtracking Type of " app-type)))))


(defmethod compile-step :dna
  [{:keys [state]}]
  ;; If a :dna gene is found, just increment the :dna counter in the state.
  (update state :dna inc))



(defmethod compile-step :fn
  [{:keys [push-unit state type-env]}]
  (let [{:keys [arg-types ret-type]} push-unit]
    (if (empty? arg-types)
      ;; Compile nullary function.
      (let [{ast :ast new-state :state} (pop-unifiable-ast ret-type state)]
        (if (= :none ast)
          state
          (push-ast {::ast  {:op      :fn
                             :methods [{:op     :fn-method
                                        :params []
                                        :body   (::ast ast)}]}
                     ::type {:type   :=>
                             :input  {:type :cat :children []}
                             :output (schema/instantiate (::type ast))}}
                    new-state)))
      ;; Compile n-ary function
      (let [;; Generate a unique symbol for each argument.
            arg-vars (repeatedly (count arg-types) #(gensym "a-"))
            ;; Compile a chunk where the arguments are "in-scope" and can appear in ASTs.
            ;; This is the function's body.
            fn-body-env (into type-env (map #(vector %1 %2) arg-vars arg-types))
            body (:ast (push->ast {:push     (first (:push state))
                                   :locals   (vec (concat (:locals state) arg-vars))
                                   :ret-type ret-type
                                   :type-env fn-body-env}))
            state-no-body-chunk (update state :push rest)
            ;; Filter out unused args
            args+types (mapcat (fn [a t]
                                 (if (schema/occurs? a body)
                                   [[a t]]
                                   []))
                               arg-vars
                               arg-types)]
        (if (= :none body)
          ;; @todo Should we leave the chunk on the push stack and unpack it?
          state-no-body-chunk
          (push-ast {::ast  {:op      :fn
                             :methods [{:op     :fn-method
                                        :params (mapv (fn [[a _]] {:op :binding :name a})
                                                      args+types)
                                        :body   (::ast body)}]}
                     ::type {:type   :=>
                             :input  {:type     :cat
                                      :children (mapv second args+types)}
                             :output (schema/instantiate (::type body))}}
                    state-no-body-chunk))))))

(defmethod compile-step :let
  [{:keys [state type-env]}]
  (let [{var-def :ast new-state :state} (pop-ast state)
        ;; Still pop the "chunk" that is next on the stack.
        ;; @todo Should we leave the chunk on the push stack and unpack it?
        noop-state (update state :push rest)]
    (if (= :none var-def)
      noop-state
      (let [;; Generate a unique symbol for the new variable.
            local-var-symb (gensym "v-")
            ;; Compile a chunk where the local variable is "in-scope" and can appear in ASTs.
            body (:ast (push->ast {:push     (first (:push new-state))
                                   :locals   (vec (conj (:locals state) local-var-symb))
                                   :ret-type {:type :s-var :sym (gensym "S")}
                                   :type-env (conj type-env [local-var-symb (::type var-def)])}))]
        (if (= :none body)
          noop-state
          ;; Compose the new `let` AST from the local variable symbol, def, and body.
          ;; Push the new AST to the state.
          (push-ast {::ast  {:op       :let
                             :bindings [{:op   :binding
                                         :name local-var-symb
                                         :init (::ast var-def)}]
                             :body     (::ast body)}
                     ::type (::type body)}
                    (update new-state :push rest)))))))



(defn default-state-output-fn
  [{:keys [ret-type] :as state}]
  (-> ret-type
      schema/instantiate
      (pop-unifiable-ast state {:allow-macros false})
      :ast))



(defn- state->log
  [state]
  (str "\n" (str/join "\n" (map #(apply pr-str %) state))))



;Push as in pushGP. Translates to AST. 
(defn push->ast
  [{:keys [push locals ret-type type-env dealiases state-output-fn record-sketch?]
    :or   {dealiases      lib/dealiases
           record-sketch? false}}]
  (let [state-output-fn (or state-output-fn default-state-output-fn)]
    ;; starts with initial state. 
    (loop [state (assoc empty-state
                        :push (reverse (into '() push))
                        :locals locals
                        :ret-type ret-type)]

      ;; Gets through whole genome.
      (if (empty? (:push state))

        ;; This is logging, side effects, and bookkeeping.
        (let [_ (log/trace "Final:" (state->log state))
              ;; @todo Experimental - record final stack AST sizes and types.
              _ (when record-sketch?
                  (record-asts! state))
              ast (w/postwalk-replace dealiases (state-output-fn state))]
          (log/trace "EMIT:" ast)
          ;; (clojure.pprint/pprint state) ;; TMH remove later
          {:ast ast
           :state state})
        

        ;; Call compile step on the next element of the genome/push code. Pop the top one off of the push code. Then pass that unit to be compiled.
        (let [{:keys [push-unit state]} (pop-push-unit state)]
          (log/trace "Current:" push-unit (state->log state))
          (recur (compile-step {:push-unit push-unit
                                :type-env  type-env
                                :state     (assoc state :apply-it (:applied push-unit))})))))))

