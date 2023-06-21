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

;; Here is the type of function application being used. By default, it is :baked-in
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

;; This is the empty state. It serves to be a starting point.
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

;; Push (verb) to the stack. The state, everything involved in compilation.
;; The stack and the rest of the genome that isn't compiled.
(defn push-ast
  "Push the `ast` to the AST stack in the `state`."
  [ast {:keys [biggest newest ret-type dna] :as state}]
  ;; ast is what is pushed onto the stack. State contains the stack. 
  ;; collect-types is an atom that isalse by default.
  ;; If set to true. start recording into atom "types-seen".
  ;; Keeps track of how much every single data type was used.
  (when @collect-types?
    (swap! types-seen
           (fn [m t] (assoc m t (inc (get m t 0))))
           (canonical-type (::type ast))))
  
  ;; output-able? checks if the output type of the tree youre checking has the same output type as the problem.
  ;; If so, it is a candidate. But can result in many small candidates
  ;; biggest-out-ast is the largest ast. If there is no asts checked, is set to biggest.
  ;; Then, the largest ast will be added to the state at the end in the map.
  (let [output-able? (and (unifiable? ret-type (::type ast))
                          (not (macro? (::ast ast))))
        newest-out-ast (if output-able? ast newest)
        biggest-out-ast (if (and output-able?
                                 (or (= biggest :none)
                                     (> (a/ast-size (::ast ast))
                                        (a/ast-size (::ast biggest)))))
                          ast
                          biggest)]
    
    ;; Below checks the app-type to see what kind of function-application to use.
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
               :newest newest-out-ast))
      
      :else
      (throw (Exception. (str "Unrecognized Application Type of " app-type))))))
    
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
  ;; Add a list as param. if remaining is empty, return the list. add if statement in recur for the list.
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
  "Pops all function ASTs regardless of argument/return types.
  Returns a list of all of the functions."
  [state]
  ;; Add a list as param. if remaining is empty, return the list. add if statement in recur for the list.
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

;; (defn find-scope-of-func
;;   "Finds the scope of a function. Returns a list of all of the possible functions it can backtrack to")

;; (defn backtrack
;;   "The backtracking process. Takes state as input. Inside state, is backtrack index, which is the index of the value in the list where the backtracking begins.
;;    Find the scope of this function, and keep track of the functions it has access to. Then, move the func to the front of the stack and recall apply. If fails, go back to old state."
;;   [state]
;;   (let [list-of-funcs (all-pop-function-ast state)]))

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


;; (let [m {:a 1 :b 2}
;;       a (:a m)]
;; = to
;; (let [{my-number :a} {:a 1 :b 2}]) Destructuring. Unpack and assign variables to the parts of the map. the symbol.
;; the let with the boxed ast is = to
;; m (pop-function-ast state)
;; boxed-ast (:ast m)
;; state-fn-popped (:state m)
;; https://clojure.org/guides/destructuring#_associative_destructuring for destructuring

;; :: is syntactic sugar. ::ast means in reference to this file.
;; :ast is keyword. type and ast were all over the place. :: means in reference to this place.





;To do: 
(defn try-apply
  "Tries to apply a function to the state. If fails, returns the original state."
  [{boxed-ast :ast state-fn-popped :state}]
  
  (log/trace "Applying function:" boxed-ast)

 
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
                ;; If it fails here, then just return the state. Have a key in state :fn-not-applied and inc it.
                  ;(update (update state :fn-not-applied inc) :total-apply-attempts inc)
            nil

                ;; push the ast to the stack. Update the state to inc :fn applied.
            (push-ast {::ast  {:op   :invoke
                               :fn   fn-ast
                               :args (mapv ::ast args)}
                       ::type (schema/substitute subs ret-s-var)}
                      new-state
                      )))

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
          (log/trace "NEW-SUBS:" new-subs)
          (if (= :none arg)
                ;; if arg is :none, not anything at all, just return the state. Also update that a func wasn't applied
                  ;(update (update state :fn-not-applied inc) :total-apply-attempts inc)
            nil

            (recur (rest remaining-arg-types)
                       ;; If arg-type is has unbound t-vars that were bound during unification,
                       ;; add them to the set of bindings.
                       ;; merge these two together.
                   (schema/compose-substitutions new-subs bindings)
                   (conj args arg)
                   state-arg-popped)))))))

(defmethod compile-step :apply
  [{:keys [state]}]

  (let [allfuncs (all-pop-function-ast state)
        allfuncsinfo (map try-apply allfuncs)
        able-to-be-applied (filter identity allfuncsinfo)
        firstapplied (first able-to-be-applied)]


    ;(println "This is the state")
    ;(println state)
    ;(println "This is the func list")
    ;(clojure.pprint/pprint allfuncs)
    ;(println "these are the function asts.")
    ;(println allfuncsinfo)
    ;(println "This is the ret val")

    ;(println "This is the top ast: ")
    ;(clojure.pprint/pprint (first (:asts firstapplied)))

    ;(println "this is the return value")
(if (empty? able-to-be-applied)
  (update (update state :fn-not-applied inc) :total-apply-attempts inc)
  (update (update firstapplied :fn-applied inc) :total-apply-attempts inc))))


;; apply attempts to call and apply a function.
;; We need an AST of a function, and then ASTs for each argument of that function.
;; Find right num of args and make sure the args work together.




;; (defmethod compile-step :apply
;;   [{:keys [state]}]
;;   ;; Function applications search for the first AST that returns a function.
;;   ;; If none found, return state.
;;   ;; If found, proceed to search for ASTs for each argument to the function.
;;   ;; If one or more arguments have :s-var types, incrementally bind them.

;;   ;; pop function off of state. Finds first function.
;;   ;; (clojure.pprint/pprint (map #(:ast %) (all-pop-function-ast state)))
;;   ;; (println "next")
;;   ;; (println "next")
;;   ;; (println "next")
;;   ;; (println "next")
;;   ;; (clojure.pprint/pprint (pop-function-ast state))
;; ;;  (let [{boxed-ast :ast state-fn-popped :state} (first (all-pop-function-ast state))]
;;   (let [{boxed-ast :ast state-fn-popped :state} (pop-function-ast state)]
;;     (log/trace "Applying function:" boxed-ast)

;;     ;; if the boxed ast is not found. just return the input state.
;;     ;; Do nothing. A function wasn't found.
;;     (if (= :none boxed-ast)
;;       (update (update state :fn-not-applied-because-no-functions inc) :total-apply-attempts inc)
;;       ;; function ast: clojure code that returns function. the data type of that function to find the right asts.
;;       (let [{fn-ast ::ast fn-type ::type} boxed-ast]

;;         ;; empty map of bindings. don't have anything yet, but when we find out what "type A" is, we know what type A is.
;;         ;; there are no args yet. Slowly loop through and find the bindings. Once you know the info, you know the 
;;         (loop [remaining-arg-types (schema/fn-arg-schemas fn-type)
;;                bindings {}
;;                args []
;;                new-state state-fn-popped]
;;           (if (empty? remaining-arg-types)
;;             ;; Push an AST which calls the function to the arguments and
;;             ;; box the AST with the return type of the function.
;;             (let [ret-s-var {:type :s-var :sym (gensym "s-")}
;;                   subs (schema/mgu (schema/substitute bindings fn-type)
;;                                    {:type   :=>
;;                                     :input  {:type     :cat
;;                                              :children (mapv ::type args)}
;;                                     :output ret-s-var})]
;;               (if (schema/mgu-failure? subs)
;;                 ;; If it fails here, then just return the state. Have a key in state :fn-not-applied and inc it.
;;                 (update (update state :fn-not-applied inc) :total-apply-attempts inc)

;;                 ;; push the ast to the stack. Update the state to inc :fn applied.
;;                 (push-ast {::ast  {:op   :invoke
;;                                    :fn   fn-ast
;;                                    :args (mapv ::ast args)}
;;                            ::type (schema/substitute subs ret-s-var)}
;;                           (update (update new-state :fn-applied inc) :total-apply-attempts inc))))

;;             ;; Grab next arg we need to find. If we know what the bindings are, we need to substitute those in.
;;             (let [arg-type (first remaining-arg-types)
;;                   _ (log/trace "Searching for arg of type:" arg-type)
;;                   ;; If arg-type is a t-var that we have seen before,
;;                   ;; bind it to the actual same type as before.
;;                   arg-type (schema/substitute bindings arg-type)
;;                   _ (log/trace "In-context arg type:" arg-type)
;;                   ;; is-s-var (= (:type arg-type) :s-var)
;;                   ;; If arg-type is still a t-var, pop an ast of any type.
;;                   ;; Otherwise, pop the AST of the expected type.
;;                   ;; The ARG ast. :bindings may contain the new bindings for things like type A, B etc.
;;                   {arg :ast state-arg-popped :state new-subs :bindings}
;;                   (pop-unifiable-ast arg-type new-state)]
;;               (log/trace "Found arg:" arg)
;;               (if (= :none arg)
;;                 ;; if arg is :none, not anything at all, just return the state. Also update that a func wasn't applied
;;                 (update (update state :fn-not-applied inc) :total-apply-attempts inc)
                
;;                 (recur (rest remaining-arg-types)
;;                        ;; If arg-type is has unbound t-vars that were bound during unification,
;;                        ;; add them to the set of bindings.
;;                        ;; merge these two together.
;;                        (schema/compose-substitutions new-subs bindings)
;;                        (conj args arg)
;;                        state-arg-popped)))))))))

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
      
      ;;  (let [megalist (all-pop-function-ast state)
      ;;        ultralist (map #(:state %) megalist)]
      ;;    (println "")
      ;;   (println "start")
      ;;   (clojure.pprint/pprint megalist)
      ;;   (println "end"))


      ;put pop func ast here to test on state.
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
          {:ast ast
           :state state})
        ;Call compile step on the next element of the genome/push code. Pop the top one off of the push code. Then pass that unit to be compiled.
        (let [{:keys [push-unit state]} (pop-push-unit state)]
          (log/trace "Current:" push-unit (state->log state))
          (recur (compile-step {:push-unit push-unit
                                :type-env  type-env
                                :state     (assoc state :apply-it (:applied push-unit))})))))))

(comment

  ;; Changes tracking level for seeing traces (first one) or not (second one)
  (log/set-min-level! :trace)
  (log/set-min-level! :info)


  (def the-type-env
    {;; + is as function of (int, int) -> int
     '+   {:type   :=>
           :input  {:type :cat :children [{:type 'int?} {:type 'int?}]}
           :output {:type 'int?}}
     ;; - is as function of (int, int) -> int
     '-   {:type   :=>
           :input  {:type :cat :children [{:type 'int?} {:type 'int?}]}
           :output {:type 'int?}}
     ;; inc is a function of int -> int
     'inc {:type   :=>
           :input  {:type :cat :children [{:type 'int?}]}
           :output {:type 'int?}}
     ;; dec is a function of int -> int
     'dec {:type   :=>
           :input  {:type :cat :children [{:type 'int?}]}
           :output {:type 'int?}}

     ;; index-of-char is a function of (string, char) -> int
     'index-of-char {:type  :=>
                     :input {:type :cat :children [{:type 'string?} {:type 'char?}]}
                     :output {:type 'int?}}
     'concatv {:type   :scheme
               :s-vars ['a]
               :body   (lib/binary-transform (lib/vector-of (lib/s-var 'a)))}
     'remove-char {:type  :=>
                   :input {:type :cat :children [{:type 'string?} {:type 'char?}]}
                   :output {:type 'string?}}
     'reduce-vec {:type   :scheme
                  :s-vars ['a]
                  :body   (lib/fn-of [(lib/fn-of [(lib/s-var 'a) (lib/s-var 'a)] (lib/s-var 'a))
                                      (lib/vector-of (lib/s-var 'a))]
                                     (lib/s-var 'a))}
     'ignore-second-arg {:type :scheme
                         :s-vars ['a]
                         :body (lib/binary-transform (lib/s-var 'a))}})

  (def an-ast
    (push->ast {;; The sequence of genes to compile.
                :push     (list {:gene :lit, :val 3, :type {:type 'int?}}
                                {:gene :lit, :val 5, :type {:type 'int?}}
                                {:gene :var, :name '-, :applied false}
                              ;; {:gene :dna}
                              ;; {:gene :dna}
                                {:gene :apply}
                                {:gene :var, :name '+, :applied true}
                                {:gene :apply}
                                {:gene :apply}
                                {:gene :lit, :val 100, :type {:type 'int?}}
                                {:gene :apply}
                                {:gene :apply})
            ;; Local variables. In this case every variable (+, inc, dec) are all globals
            ;; this is empty.
                :locals   []
            ;; The return type of the AST we want to output at the end. 
            ;; I'm assuming integers based on the input sequence you gave.
                :ret-type {:type 'int?}
            ;; The type environment telling the type systems what every variable's data
            ;; type is. In this case I wrote out the 3 variables present in the sequence.
                :type-env the-type-env}))

  ;; Get the form from the ast
  (a/ast->form
   (::ast
    (:ast an-ast)))

  an-ast


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Testing backtracking on first example

  (def backtracking-ex1-ast
    (push->ast {;; The sequence of genes to compile.
                :push     (list {:gene :lit, :val 3, :type {:type 'int?}}
                                {:gene :lit, :val 5, :type {:type 'int?}}
                                {:gene :var, :name '+}
                                {:gene :lit, :val "hi" :type {:type 'string?}}
                                {:gene :var, :name 'index-of-char}
                                {:gene :lit, :val true, :type {:type 'boolean?}}
                                ;{:gene :apply}
                                )
            ;; Local variables. In this case every variable (+, inc, dec) are all globals
            ;; this is empty.
                :locals   []
            ;; The return type of the AST we want to output at the end. 
            ;; I'm assuming integers based on the input sequence you gave.
                :ret-type {:type 'int?}
            ;; The type environment telling the type systems what every variable's data
            ;; type is. In this case I wrote out the 3 variables present in the sequence.
                :type-env the-type-env}))


  (def backtracking-ex2-asta
    (push->ast {;; The sequence of genes to compile.
                :push     (list {:gene :lit, :val 3, :type {:type 'int?}}
                                {:gene :lit, :val 5, :type {:type 'int?}}
                                {:gene :var, :name '+}
                                {:gene :lit, :val [1 2], :type {:type :vector :child {:type 'int?}}}
                                {:gene :lit, :val [3 4], :type {:type :vector :child {:type 'int?}}}
                                ;{:gene :lit, :val ["hi" "there"], :type {:type :vector :child {:type 'string?}}}
                                {:gene :var, :name 'concatv}

                                ;{:gene :apply}
                                )
            ;; Local variables. In this case every variable (+, inc, dec) are all globals
            ;; this is empty.
                :locals   []
            ;; The return type of the AST we want to output at the end. 
            ;; I'm assuming integers based on the input sequence you gave.
                :ret-type {:type 'int?}
            ;; The type environment telling the type systems what every variable's data
            ;; type is. In this case I wrote out the 3 variables present in the sequence.
                :type-env the-type-env}))


  (def backtracking-ex2-astb
    (push->ast {;; The sequence of genes to compile.
                :push     (list {:gene :lit, :val 3, :type {:type 'int?}}
                                {:gene :lit, :val 5, :type {:type 'int?}}
                                {:gene :var, :name '+}
                                {:gene :lit, :val [1 2], :type {:type :vector :child {:type 'int?}}}
                                {:gene :lit, :val [3 4], :type {:type :vector :child {:type 'int?}}}
                                {:gene :lit, :val ["hi" "there"], :type {:type :vector :child {:type 'string?}}}
                                {:gene :var, :name 'concatv}

                                ;{:gene :apply}
                                )
            ;; Local variables. In this case every variable (+, inc, dec) are all globals
            ;; this is empty.
                :locals   []
            ;; The return type of the AST we want to output at the end. 
            ;; I'm assuming integers based on the input sequence you gave.
                :ret-type {:type 'int?}
            ;; The type environment telling the type systems what every variable's data
            ;; type is. In this case I wrote out the 3 variables present in the sequence.
                :type-env the-type-env}))




  (def backtracking-ex3-ast1
    (push->ast {;; The sequence of genes to compile.
                :push     (list {:gene :lit, :val 4, :type {:type 'int?}}
                                {:gene :lit, :val 5, :type {:type 'int?}}
                                {:gene :var, :name '+}
                                {:gene :lit, :val "hi" :type {:type 'string?}}
                                {:gene :lit, :val \h :type {:type 'char?}}
                                {:gene :lit, :val [3 4], :type {:type :vector :child {:type 'int?}}}
                                {:gene :lit, :val ["hi" "there"], :type {:type :vector :child {:type 'string?}}}
                                {:gene :var, :name 'concatv}
                                {:gene :var, :name 'remove-char}


                                ;{:gene :apply}
                                )
            ;; Local variables. In this case every variable (+, inc, dec) are all globals
            ;; this is empty.
                :locals   []
            ;; The return type of the AST we want to output at the end. 
            ;; I'm assuming integers based on the input sequence you gave.
                :ret-type {:type 'int?}
            ;; The type environment telling the type systems what every variable's data
            ;; type is. In this case I wrote out the 3 variables present in the sequence.
                :type-env the-type-env}))


  (def start-state-ex3a
    {:asts '(#:erp12.cbgp-lite.lang.compile{:ast {:op :var, :var remove-char}, :type {:type :=>, :input {:type :cat, :children [{:type string?} {:type char?}]}, :output {:type string?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :var, :var concatv}, :type {:type :=>, :input {:type :cat, :children [{:type :vector, :child {:type :s-var, :sym s-17897}} {:type :vector, :child {:type :s-var, :sym s-17897}}]}, :output {:type :vector, :child {:type :s-var, :sym s-17897}}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val ["hi" "there"]}, :type {:type :vector, :child {:type string?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val [3 4]}, :type {:type :vector, :child {:type int?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val \h}, :type {:type char?}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val "hi"}, :type {:type string?}} #:erp12.cbgp-lite.lang.compile{:ast {:op :var, :var +}, :type {:type :=>, :input {:type :cat, :children [{:type int?} {:type int?}]}, :output {:type int?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val 5}, :type {:type int?}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val 4}, :type {:type int?}})
     :push ()
     :locals []
     :ret-type {:type int?}
     :fn-applied 0
     :fn-not-applied 0
     :total-apply-attempts 0
     :fn-not-applied-because-no-functions 0})



  ;; has ["hi" "there"]
  (def start-state-ex2a
    {:asts '(#:erp12.cbgp-lite.lang.compile{:ast {:op :var, :var concatv}, :type {:type :=>, :input {:type :cat, :children [{:type :vector, :child {:type :s-var, :sym s-17707}} {:type :vector, :child {:type :s-var, :sym s-17707}}]}, :output {:type :vector, :child {:type :s-var, :sym s-17707}}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val ["hi" "there"]}, :type {:type :vector, :child {:type string?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val [3 4]}, :type {:type :vector, :child {:type int?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val [1 2]}, :type {:type :vector, :child {:type int?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :var, :var +}, :type {:type :=>, :input {:type :cat, :children [{:type int?} {:type int?}]}, :output {:type int?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val 5}, :type {:type int?}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val 3}, :type {:type int?}})
     :push ()
     :locals []
     :ret-type {:type int?}
     :fn-applied 0
     :fn-not-applied 0
     :total-apply-attempts 0
     :fn-not-applied-because-no-functions 0})


  ;; does not have ["hi" "there"]
  (def start-state-ex2b
    {:asts '(#:erp12.cbgp-lite.lang.compile{:ast {:op :var, :var concatv}, :type {:type :=>, :input {:type :cat, :children [{:type :vector, :child {:type :s-var, :sym s-17538}} {:type :vector, :child {:type :s-var, :sym s-17538}}]}, :output {:type :vector, :child {:type :s-var, :sym s-17538}}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val [3 4]}, :type {:type :vector, :child {:type int?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val [1 2]}, :type {:type :vector, :child {:type int?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :var, :var +}, :type {:type :=>, :input {:type :cat, :children [{:type int?} {:type int?}]}, :output {:type int?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val 5}, :type {:type int?}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val 3}, :type {:type int?}})
     :push ()
     :locals []
     :ret-type {:type int?}
     :fn-applied 0
     :fn-not-applied 0
     :total-apply-attempts 0
     :fn-not-applied-because-no-functions 0})





  ;;Run this, then grab asts and put there, then make the rest identical. 

  (def start-state-ex1
    {:asts '(#:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val true}, :type {:type boolean?}} #:erp12.cbgp-lite.lang.compile{:ast {:op :var, :var index-of-char}, :type {:type :=>, :input {:type :cat, :children [{:type string?} {:type char?}]}, :output {:type int?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val "hi"}, :type {:type string?}} #:erp12.cbgp-lite.lang.compile{:ast {:op :var, :var +}, :type {:type :=>, :input {:type :cat, :children [{:type int?} {:type int?}]}, :output {:type int?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val 5}, :type {:type int?}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val 3}, :type {:type int?}})
     :push ()
     :locals []
     :ret-type {:type int?}
     :fn-applied 0
     :fn-not-applied 0
     :total-apply-attempts 0
     :fn-not-applied-because-no-functions 0})

  start-state-ex1

  ;; Runs apply on the above state
  (compile-step {:push-unit {:gene :apply}
                 :type-env  the-type-env
                 :state     start-state-ex1})

  (compile-step {:push-unit {:gene :apply}
                 :type-env  the-type-env
                 :state     start-state-ex2a})

  (compile-step {:push-unit {:gene :apply}
                 :type-env  the-type-env
                 :state     start-state-ex2b})

  (compile-step {:push-unit {:gene :apply}
                 :type-env  the-type-env
                 :state     start-state-ex3a})

  (def backtracking-ex1-ast-fail
    (push->ast {;; The sequence of genes to compile.
                :push     (list
                           {:gene :lit, :val true, :type {:type 'boolean}}
                           {:gene :var, :name '+}
                           {:gene :lit, :val "hi" :type {:type 'string?}}
                           {:gene :var, :name 'index-of-char}
                           {:gene :lit, :val true, :type {:type 'boolean?}}
                                ;{:gene :apply}
                           )
            ;; Local variables. In this case every variable (+, inc, dec) are all globals
            ;; this is empty.
                :locals   []
            ;; The return type of the AST we want to output at the end. 
            ;; I'm assuming integers based on the input sequence you gave.
                :ret-type {:type 'int?}
            ;; The type environment telling the type systems what every variable's data
            ;; type is. In this case I wrote out the 3 variables present in the sequence.
                :type-env the-type-env}))

  (def start-state-ex1-fail
    {:asts '(#:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val true}, :type {:type boolean?}} #:erp12.cbgp-lite.lang.compile{:ast {:op :var, :var index-of-char}, :type {:type :=>, :input {:type :cat, :children [{:type string?} {:type char?}]}, :output {:type int?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val "hi"}, :type {:type string?}} #:erp12.cbgp-lite.lang.compile{:ast {:op :var, :var +}, :type {:type :=>, :input {:type :cat, :children [{:type int?} {:type int?}]}, :output {:type int?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val true}, :type {:type boolean}})
     :push ()
     :locals []
     :ret-type {:type int?}
     :fn-applied 0
     :fn-not-applied 0
     :total-apply-attempts 0
     :fn-not-applied-because-no-functions 0})

  (compile-step {:push-unit {:gene :apply}
                 :type-env  the-type-env
                 :state     start-state-ex1-fail})


  (def backtracking-ex3-astb
    (push->ast {;; The sequence of genes to compile.
                :push     (list {:gene :lit, :val 4, :type {:type 'int?}}
                                {:gene :lit, :val 5, :type {:type 'int?}}
                                {:gene :var, :name '+}
                                {:gene :lit, :val "hi" :type {:type 'string?}}
                                {:gene :lit, :val \h :type {:type 'char?}}
                                {:gene :lit, :val [3 4], :type {:type :vector :child {:type 'int?}}}
                                {:gene :lit, :val ["hi" "there"], :type {:type :vector :child {:type 'string?}}}
                                {:gene :lit, :val [5 6], :type {:type :vector :child {:type 'int?}}}
                                {:gene :var, :name 'concatv}
                                {:gene :var, :name 'remove-char}


                                ;{:gene :apply}
                                )
            ;; Local variables. In this case every variable (+, inc, dec) are all globals
            ;; this is empty.
                :locals   []
            ;; The return type of the AST we want to output at the end. 
            ;; I'm assuming integers based on the input sequence you gave.
                :ret-type {:type 'int?}
            ;; The type environment telling the type systems what every variable's data
            ;; type is. In this case I wrote out the 3 variables present in the sequence.
                :type-env the-type-env}))

  (def start-state-ex3b
    {:asts '(#:erp12.cbgp-lite.lang.compile{:ast {:op :var, :var remove-char}, :type {:type :=>, :input {:type :cat, :children [{:type string?} {:type char?}]}, :output {:type string?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :var, :var concatv}, :type {:type :=>, :input {:type :cat, :children [{:type :vector, :child {:type :s-var, :sym s-18413}} {:type :vector, :child {:type :s-var, :sym s-18413}}]}, :output {:type :vector, :child {:type :s-var, :sym s-18413}}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val [5 6]}, :type {:type :vector, :child {:type int?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val ["hi" "there"]}, :type {:type :vector, :child {:type string?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val [3 4]}, :type {:type :vector, :child {:type int?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val \h}, :type {:type char?}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val "hi"}, :type {:type string?}} #:erp12.cbgp-lite.lang.compile{:ast {:op :var, :var +}, :type {:type :=>, :input {:type :cat, :children [{:type int?} {:type int?}]}, :output {:type int?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val 5}, :type {:type int?}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val 4}, :type {:type int?}})
     :push ()
     :locals []
     :ret-type {:type int?}
     :fn-applied 0
     :fn-not-applied 0
     :total-apply-attempts 0
     :fn-not-applied-because-no-functions 0})

  (compile-step {:push-unit {:gene :apply}
                 :type-env  the-type-env
                 :state     start-state-ex3b})




  (def backtracking-ex3-astc
    (push->ast {;; The sequence of genes to compile.
                :push     (list {:gene :lit, :val 4, :type {:type 'int?}}
                                {:gene :lit, :val 5, :type {:type 'int?}}
                                {:gene :var, :name '+}
                                {:gene :lit, :val "hi" :type {:type 'string?}}
                                  ;{:gene :lit, :val \h :type {:type 'char?}}
                                {:gene :lit, :val [3 4], :type {:type :vector :child {:type 'int?}}}
                                {:gene :lit, :val ["hi" "there"], :type {:type :vector :child {:type 'string?}}}
                                {:gene :lit, :val [5 6], :type {:type :vector :child {:type 'int?}}}
                                {:gene :var, :name 'concatv}
                                {:gene :var, :name 'remove-char}


                                ;{:gene :apply}
                                )
            ;; Local variables. In this case every variable (+, inc, dec) are all globals
            ;; this is empty.
                :locals   []
            ;; The return type of the AST we want to output at the end. 
            ;; I'm assuming integers based on the input sequence you gave.
                :ret-type {:type 'int?}
            ;; The type environment telling the type systems what every variable's data
            ;; type is. In this case I wrote out the 3 variables present in the sequence.
                :type-env the-type-env}))

  (def start-state-ex3c
    {:asts '(#:erp12.cbgp-lite.lang.compile{:ast {:op :var, :var remove-char}, :type {:type :=>, :input {:type :cat, :children [{:type string?} {:type char?}]}, :output {:type string?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :var, :var concatv}, :type {:type :=>, :input {:type :cat, :children [{:type :vector, :child {:type :s-var, :sym s-18417}} {:type :vector, :child {:type :s-var, :sym s-18417}}]}, :output {:type :vector, :child {:type :s-var, :sym s-18417}}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val [5 6]}, :type {:type :vector, :child {:type int?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val ["hi" "there"]}, :type {:type :vector, :child {:type string?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val [3 4]}, :type {:type :vector, :child {:type int?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val "hi"}, :type {:type string?}} #:erp12.cbgp-lite.lang.compile{:ast {:op :var, :var +}, :type {:type :=>, :input {:type :cat, :children [{:type int?} {:type int?}]}, :output {:type int?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val 5}, :type {:type int?}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val 4}, :type {:type int?}})
     :push ()
     :locals []
     :ret-type {:type int?}
     :fn-applied 0
     :fn-not-applied 0
     :total-apply-attempts 0
     :fn-not-applied-because-no-functions 0})

  (compile-step {:push-unit {:gene :apply}
                 :type-env  the-type-env
                 :state     start-state-ex3c})

  (def backtracking-ex3-astd
    (push->ast {;; The sequence of genes to compile.
                :push     (list {:gene :lit, :val 4, :type {:type 'int?}}
                                {:gene :lit, :val 5, :type {:type 'int?}}
                                {:gene :var, :name '+}
                                {:gene :lit, :val "hi" :type {:type 'string?}}
                                  ;{:gene :lit, :val \h :type {:type 'char?}}
                                {:gene :lit, :val [3 4], :type {:type :vector :child {:type 'int?}}}
                                {:gene :lit, :val ["hi" "there"], :type {:type :vector :child {:type 'string?}}}
                                {:gene :lit, :val [5 6], :type {:type :vector :child {:type 'int?}}}
                                {:gene :var, :name 'remove-char}
                                {:gene :var, :name 'concatv}



                                ;{:gene :apply}
                                )
            ;; Local variables. In this case every variable (+, inc, dec) are all globals
            ;; this is empty.
                :locals   []
            ;; The return type of the AST we want to output at the end. 
            ;; I'm assuming integers based on the input sequence you gave.
                :ret-type {:type 'int?}
            ;; The type environment telling the type systems what every variable's data
            ;; type is. In this case I wrote out the 3 variables present in the sequence.
                :type-env the-type-env}))

  (def start-state-ex3d
    {:asts '(#:erp12.cbgp-lite.lang.compile{:ast {:op :var, :var concatv}, :type {:type :=>, :input {:type :cat, :children [{:type :vector, :child {:type :s-var, :sym s-18757}} {:type :vector, :child {:type :s-var, :sym s-18757}}]}, :output {:type :vector, :child {:type :s-var, :sym s-18757}}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :var, :var remove-char}, :type {:type :=>, :input {:type :cat, :children [{:type string?} {:type char?}]}, :output {:type string?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val [5 6]}, :type {:type :vector, :child {:type int?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val ["hi" "there"]}, :type {:type :vector, :child {:type string?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val [3 4]}, :type {:type :vector, :child {:type int?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val "hi"}, :type {:type string?}} #:erp12.cbgp-lite.lang.compile{:ast {:op :var, :var +}, :type {:type :=>, :input {:type :cat, :children [{:type int?} {:type int?}]}, :output {:type int?}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val 5}, :type {:type int?}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val 4}, :type {:type int?}})
     :push ()
     :locals []
     :ret-type {:type int?}
     :fn-applied 0
     :fn-not-applied 0
     :total-apply-attempts 0
     :fn-not-applied-because-no-functions 0})

  (compile-step {:push-unit {:gene :apply}
                 :type-env  the-type-env
                 :state     start-state-ex3d})

  (def backtracking-ex1-empty
    (push->ast {;; The sequence of genes to compile.
                :push     (list
                           {:gene :lit, :val true, :type {:type 'boolean}}
                           {:gene :lit, :val "hi" :type {:type 'string?}}
                           {:gene :lit, :val true, :type {:type 'boolean?}}
                                ;{:gene :apply}
                           )
            ;; Local variables. In this case every variable (+, inc, dec) are all globals
            ;; this is empty.
                :locals   []
            ;; The return type of the AST we want to output at the end. 
            ;; I'm assuming integers based on the input sequence you gave.
                :ret-type {:type 'int?}
            ;; The type environment telling the type systems what every variable's data
            ;; type is. In this case I wrote out the 3 variables present in the sequence.
                :type-env the-type-env}))

  (def start-state-ex1-empty
    {:asts '(#:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val true}, :type {:type boolean?}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val "hi"}, :type {:type string?}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val true}, :type {:type boolean}})
     :push ()
     :locals []
     :ret-type {:type int?}
     :fn-applied 0
     :fn-not-applied 0
     :total-apply-attempts 0
     :fn-not-applied-because-no-functions 0})

  (compile-step {:push-unit {:gene :apply}
                 :type-env  the-type-env
                 :state     start-state-ex1-empty})


  (def backtracking-ex4-ast
    (push->ast {;; The sequence of genes to compile.
                :push     (list {:gene :lit, :val [[1] [2 3] [4 5]], :type {:type :vector
                                                                            :child {:type :vector :child {:type 'int?}}}}
                                {:gene :var, :name 'concatv}
                                {:gene :var, :name 'reduce-vec}

                                ;{:gene :apply}
                                )
            ;; Local variables. In this case every variable (+, inc, dec) are all globals
            ;; this is empty.
                :locals   []
            ;; The return type of the AST we want to output at the end. 
            ;; I'm assuming integers based on the input sequence you gave.
                :ret-type {:type 'int?}
            ;; The type environment telling the type systems what every variable's data
            ;; type is. In this case I wrote out the 3 variables present in the sequence.
                :type-env the-type-env}))

  (def start-state-ex4
    {:asts '(#:erp12.cbgp-lite.lang.compile{:ast {:op :var, :var reduce-vec}, :type {:type :=>, :input {:type :cat, :children [{:type :=>, :input {:type :cat, :children [{:type :s-var, :sym s-10838} {:type :s-var, :sym s-10838}]}, :output {:type :s-var, :sym s-10838}} {:type :vector, :child {:type :s-var, :sym s-10838}}]}, :output {:type :s-var, :sym s-10838}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :var, :var concatv}, :type {:type :=>, :input {:type :cat, :children [{:type :vector, :child {:type :s-var, :sym s-10837}} {:type :vector, :child {:type :s-var, :sym s-10837}}]}, :output {:type :vector, :child {:type :s-var, :sym s-10837}}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val [[1] [2 3] [4 5]]}, :type {:type :vector, :child {:type :vector, :child {:type int?}}}})
     :push ()
     :locals []
     :ret-type {:type int?}
     :fn-applied 0
     :fn-not-applied 0
     :total-apply-attempts 0
     :fn-not-applied-because-no-functions 0})

  (compile-step {:push-unit {:gene :apply}
                 :type-env  the-type-env
                 :state     start-state-ex4})


  (def backtracking-ex5-ast
    (push->ast {;; The sequence of genes to compile.
                :push     (list {:gene :lit, :val [3 4 4 2 1], :type {:type :vector :child {:type 'int?}}}
                                {:gene :var, :name 'ignore-second-arg}
                                {:gene :var, :name 'reduce-vec}

                                ;{:gene :apply}
                                )
            ;; Local variables. In this case every variable (+, inc, dec) are all globals
            ;; this is empty.
                :locals   []
            ;; The return type of the AST we want to output at the end. 
            ;; I'm assuming integers based on the input sequence you gave.
                :ret-type {:type 'int?}
            ;; The type environment telling the type systems what every variable's data
            ;; type is. In this case I wrote out the 3 variables present in the sequence.
                :type-env the-type-env}))

  (def start-state-ex5
    {:asts '(#:erp12.cbgp-lite.lang.compile{:ast {:op :var, :var reduce-vec}, :type {:type :=>, :input {:type :cat, :children [{:type :=>, :input {:type :cat, :children [{:type :s-var, :sym s-10845} {:type :s-var, :sym s-10845}]}, :output {:type :s-var, :sym s-10845}} {:type :vector, :child {:type :s-var, :sym s-10845}}]}, :output {:type :s-var, :sym s-10845}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :var, :var ignore-second-arg}, :type {:type :=>, :input {:type :cat, :children [{:type :s-var, :sym s-10844} {:type :s-var, :sym s-10844}]}, :output {:type :s-var, :sym s-10844}}} #:erp12.cbgp-lite.lang.compile{:ast {:op :const, :val [3 4 4 2 1]}, :type {:type :vector, :child {:type int?}}})
     :push ()
     :locals []
     :ret-type {:type int?}
     :fn-applied 0
     :fn-not-applied 0
     :total-apply-attempts 0
     :fn-not-applied-because-no-functions 0})

  (compile-step {:push-unit {:gene :apply}
                 :type-env  the-type-env
                 :state     start-state-ex5})

  )
