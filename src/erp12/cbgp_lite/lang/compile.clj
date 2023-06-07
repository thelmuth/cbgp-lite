(ns erp12.cbgp-lite.lang.compile
  (:require [clojure.string :as str]
            [clojure.walk :as w]
            [erp12.cbgp-lite.lang.ast :as a]
            [erp12.cbgp-lite.lang.lib :as lib]
            [erp12.cbgp-lite.lang.schema :as schema]
            [taoensso.timbre :as log]
            [clojure.pprint]))

(def collect-types? (atom false))
(def types-seen (atom {}))

;Here is the type of application
(def app-type (atom :original))
(def baked-in-apply-probability (atom 0.5))

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

;Empty state. Starting point
(def empty-state
  {:asts    (list)
   :push    []
   :locals  []
   ;; @todo Experimental
   :biggest :none
   :newest  :none
   :dna 0})

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

;Push (verb) to the stack. State, everything involved in compilation. The stack and rest of the genome that isn't compiled.
(defn push-ast
  "Push the `ast` to the AST stack in the `state`."
  [ast {:keys [biggest newest ret-type dna] :as state}]
  ;ast is what is pushed onto the stack. State holds onto the stack. 

 ;collect-types is an atom. False by default. If set true. start recording into atom "types-seen". Keeps track of how much every single data type was used.
  (when @collect-types?
    (swap! types-seen
           (fn [m t] (assoc m t (inc (get m t 0))))
           (canonical-type (::type ast))))
  
 ;output-able? checks if the output type of the tree youre checking has the same output type as the problem. If so, it is a candidate. But can result in many small candidates
  ; biggest-out-ast is the largest ast. If there is no asts checked, is set to biggest. Then, the largest ast will be added to the state at the end in the map.
  (let [output-able? (and (unifiable? ret-type (::type ast))
                          (not (macro? (::ast ast))))
        newest-out-ast (if output-able? ast newest)
        biggest-out-ast (if (and output-able?
                                 (or (= biggest :none)
                                     (> (a/ast-size (::ast ast))
                                        (a/ast-size (::ast biggest)))))
                          ast
                          biggest)]
    
    ;(println (map #(keys %) (:asts state)))
    ;; (let [astvals (:asts state)
    ;;       list-of-maps (map #(:erp12.cbgp-lite.lang.compile/type %) astvals)]
    ;;   ;(fn [{:keys [push-unit]}] (:gene push-unit))
    ;;   (println list-of-maps)
    ;;   )
    
    (cond
      (= @app-type :all)
      (compile-step {:push-unit {:gene :apply}
                     :state (assoc state
                                   :asts (conj (:asts state) ast)
                                   :biggest biggest-out-ast
                                   :newest newest-out-ast)})
      
      (= @app-type :original)
      (assoc state
             :asts (conj (:asts state) ast)
             :biggest biggest-out-ast
             :newest newest-out-ast)

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
               :newest newest-out-ast)))
    
    ; (let [{found-DNA :ast updated-state :state} (pop-unifiable-ast :fn state)]
    ;   (if (not= found-DNA :none)
    ;     
    ;; (compile-step {:push-unit {:gene :apply}
    ;;                :state (assoc state
    ;;                              :asts (conj (:asts state) ast)
                                ;;  :biggest biggest-out-ast
    ;;                              :newest newest-out-ast)})
    ;; (assoc state
    ;;        :asts (conj (:asts state) ast)
    ;;        :biggest biggest-out-ast
    ;;        :newest newest-out-ast))
    

    
    ; (if (is-fn? ast
    ;      if (:DNA is on the ast.
    ;       push
    ;       (apply push)
    ;       push
    ;    ))
    ; find a way to save on the genome or stack if there is DNA. True / false. if that key is in the map, dontapply. otherwise, do.
    
 ; can do this in multiple places.
    ; The `ast` map passed to `push-ast` extra info on to use DNA or not.
    ;`compile-step` method for `:var` genes
    ; ^ as an extra key of `:push-unit`
    
    ;New kind of gene for vars which DNA. `default-gene-distribution `in task.clj. Also changes in `make-genetic-source `function of plushy.clj
    

    ; Idea for DNA. Search stack for DNA with pop unifiable AST. If none returned, compile-step... Otherwise, don't.
    ))

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

(defn pop-push-unit
  [state]
  {:push-unit (first (:push state))
   :state     (update state :push rest)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Push to AST Compilation

(declare push->ast)

;Theres a func compiled step, but do a diff thing based on return type. This func is called the dispatch func. If the gene is a lit, then do lit func. If var, do var type.
; All does pushing and figuring out stuff on the stack.
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

;apply: calling the function. We need an AST of a function, and ASTs for each argument of that function. Find right num of args and make sure the args work together.
(defmethod compile-step :apply
  [{:keys [state]}]
  ;; Function applications search for the first AST that returns a function.
  ;; If none found, return state.
  ;; If found, proceed to search for ASTs for each argument to the function.
  ;; If one or more arguments have :s-var types, incrementally bind them.

  ; pop function off of state. Finds first function.
  ; (let [m {:a 1 :b 2}
  ;       a (:a m)]
  ; = to
  
  ; (let [{my-number :a} {:a 1 :b 2}]) Destructuring. Unpack and assign variables to the parts of the map. the symbol.
  ; the let with the boxed ast is = to
  ; m (pop-function-ast state)
  ; boxed-ast (:ast m)
  ; state-fn-popped (:state m)
  ; https://clojure.org/guides/destructuring#_associative_destructuring for destructuring
  (let [{boxed-ast :ast state-fn-popped :state} (pop-function-ast state)]
    (log/trace "Applying function:" boxed-ast)

    ;if the boxed ast is not found. just return the input state. do nothing. a function wasn't found.
    (if (= :none boxed-ast)
      state
      ; :: is syntactic sugar. ::ast means in reference to this file. :ast is keyword. type and ast were all over the place. :: means in reference to this place.
      ; functin ast: clojure code that returns function. the data type of that function to find the right asts.
      (let [{fn-ast ::ast fn-type ::type} boxed-ast]
        
        ;empty map of bindings. don't have anything yet, but when we find out what "type A" is, we know what type A is.
        ;there are no args yet. Slowly loop through and find the bindings. Once you know the info, you know the 
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
                state
                (push-ast {::ast  {:op   :invoke
                                   :fn   fn-ast
                                   :args (mapv ::ast args)}
                           ::type (schema/substitute subs ret-s-var)}
                          new-state)))
            
            ;grab next arg we need to find. If we know what the bindings are, we need to substitute those in.
            (let [arg-type (first remaining-arg-types)
                  _ (log/trace "Searching for arg of type:" arg-type)
                  ;; If arg-type is a t-var that we have seen before,
                  ;; bind it to the actual same type as before.
                  arg-type (schema/substitute bindings arg-type)
                  _ (log/trace "In-context arg type:" arg-type)
                  ;is-s-var (= (:type arg-type) :s-var)
                  ;; If arg-type is still a t-var, pop an ast of any type.
                  ;; Otherwise, pop the AST of the expected type.
                  ;The ARG ast. :bindings may contain the new bindings for things like type A, B etc.
                  {arg :ast state-arg-popped :state new-subs :bindings}
                  (pop-unifiable-ast arg-type new-state)]
              (log/trace "Found arg:" arg)
              (if (= :none arg)
                state
                (recur (rest remaining-arg-types)
                       ;; If arg-type is has unbound t-vars that were bound during unification,
                       ;; add them to the set of bindings.
                       ;merge these two together.
                       (schema/compose-substitutions new-subs bindings)
                       (conj args arg)
                       state-arg-popped)))))))))

(defmethod compile-step :dna
  [{:keys [state]}]
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
            body (push->ast {:push     (first (:push state))
                             :locals   (vec (concat (:locals state) arg-vars))
                             :ret-type ret-type
                             :type-env fn-body-env})
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
            body (push->ast {:push     (first (:push new-state))
                             :locals   (vec (conj (:locals state) local-var-symb))
                             :ret-type {:type :s-var :sym (gensym "S")}
                             :type-env (conj type-env [local-var-symb (::type var-def)])})]
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

;Push as in pushGP. Arrow means translates to AST. 
;(3 5 + DNA inc dec)
(defn push->ast
  [{:keys [push locals ret-type type-env dealiases state-output-fn record-sketch?]
    :or   {dealiases      lib/dealiases
           record-sketch? false}}]
  (let [state-output-fn (or state-output-fn default-state-output-fn)]
    ;starts with initial state. 
    (loop [state (assoc empty-state
                   ;; Ensure a list
                        ;list of push code trying to compile. Loading some of these into empty state.
                   :push (reverse (into '() push))
                   :locals locals
                   :ret-type ret-type)]
      ;Get through whole genome.
      (if (empty? (:push state))
        
        ;This is logging, side effects, and bookkeeping.
        (let [_ (log/trace "Final:" (state->log state))
              ;; @todo Experimental - record final stack AST sizes and types.
              _ (when record-sketch?
                  (record-asts! state))
              ast (w/postwalk-replace dealiases (state-output-fn state))]
          (log/trace "EMIT:" ast)
          ;; (clojure.pprint/pprint state) ;; TMH remove later
          ast)
        ;Call compile step on the next element of the genome/push code. Pop the top one off of the push code. Then pass that unit to be compiled.
        (let [{:keys [push-unit state]} (pop-push-unit state)]
          (log/trace "Current:" push-unit (state->log state))
          (recur (compile-step {:push-unit push-unit
                                :type-env  type-env
                                :state     (assoc state :apply-it (:applied push-unit))})))))))

(comment
  (push->ast {;; The sequence of genes to compile.
              :push     (list {:gene :lit, :val 3, :type {:type 'int?}}
                              {:gene :lit, :val 5, :type {:type 'int?}}
                              {:gene :var, :name '+, :applied false}
                              ;; {:gene :dna}
                              ;; {:gene :dna}
                              {:gene :var, :name 'inc, :applied true}
                              {:gene :var, :name 'dec, :applied true}
                              {:gene :var, :name 'inc, :applied true})
            ;; Local variables. In this case every variable (+, inc, dec) are all globals
            ;; this is empty.
              :locals   []
            ;; The return type of the AST we want to output at the end. 
            ;; I'm assuming integers based on the input sequence you gave.
              :ret-type {:type 'int?}
            ;; The type environment telling the type systems what every variable's data
            ;; type is. In this case I wrote out the 3 variables present in the sequence.
              :type-env {;; + is as function of (int, int) -> int
                         '+   {:type   :=>
                               :input  {:type :cat :children [{:type 'int?} {:type 'int?}]}
                               :output {:type 'int?}}
                       ;; inc is a function of int -> int
                         'inc {:type   :=>
                               :input  {:type :cat :children [{:type 'int?}]}
                               :output {:type 'int?}}
                       ;; dec is a function of int -> int
                         'dec {:type   :=>
                               :input  {:type :cat :children [{:type 'int?}]}
                               :output {:type 'int?}}}})
  )
