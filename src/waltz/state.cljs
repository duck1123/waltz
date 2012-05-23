(ns waltz.state
  (:refer-clojure :exclude [set]))


;;; Internal API

(defn state* []
  {:in []
   :out []
   :constraints []})

(defn in* [state fn]
  (update-in state [:in] conj fn))

(defn out* [state fn]
  (update-in state [:out] conj fn))

(defn constraint [m fn]
  (update-in m [:constraint] conj fn))

(defn debug-log [{:keys [machine] :as sm} v & vs]
  (when (and js/console (@machine :debug))
    (let [s (apply str (@machine :name) " :: " v vs)]
      (.log js/console s))))

(defn ->coll [v]
  (if (coll? v)
    v
    [v]))


;;; Low-level public API

;; Functions, operating on state machine environment.

(defn get-in-sm [{:keys [env]} ks]
  "Fetch a possibly nested value from state machine environment."
  (get-in @env ks))

(defn assoc-sm [{:keys [env]} ks v]
  "Associate a possibly nested value in state machine environment."
  (swap! env #(assoc-in % ks v)))

(defn update-sm [{:keys [env]} & fntail]
  "Update a possibly nested value in state machine environment."
  (swap! env #(apply update-in % fntail)))

(defn current [sm]
  "Fetch a set of current states the machine is in."
  (get-in-sm sm [:current]))

(defn in? [sm state]
  "Validate if a state machine is in the given state."
  ((current sm) state))

;; Functions, operating on state machine structure.

(defn can-transition? [{:keys [machine]} state]
  (let [trans (get-in @machine [:states state :constraints])]
    (if trans
      (every? #(% state) trans)
      true)))

(defn has-state? [{:keys [machine]} state]
  (get-in @machine [:states state]))

(defn has-event? [{:keys [machine]} event]
  (get-in @machine [:events event]))

(defn add-state [{:keys [machine]} name v]
  (swap! machine #(assoc-in % [:states name] v)))

(defn add-event [{:keys [machine]} name v]
  (swap! machine #(assoc-in % [:events name] v)))


;;; Public API

(defrecord StateMachine
    ^{:doc "Each state machine is defined by the actual 'machine',
a container for states and transitions and 'env' -- variable
part of the machine, holding a set of states the machine is in."}
  [env machine])

(defn clone [sm & [env]]
  "Clone a state machine, resetting the environment to provided value."
  {:pre [(associative? env)]}
  (assoc sm :env
         ;; Hook-in any additional options given.
         (atom (assoc env :current #{}))))

(defn machine [n & [env {:keys [debug] :or {debug true}}]]
  "Create a new named state machine, optionally initializing the environment."
  {:pre [(associative? env)]}
  (let [m (atom {:debug debug
                 :name (name n)
                 :states {}
                 :events {}})]
        (clone (StateMachine. nil m) env)))

(defn watch [{:keys [env] :as sm} f]
  "Watch environment changes in a given state machine."
  (add-watch env :change
             (fn [_ref _key old new]
               ;; Only trigger the callback on :current env changes.
               (when (not= (:current old) (:current new))
                 (f old new))))
  sm)

(defn unwatch [{:keys [env] :as sm}]
  "Remove env. changes watch from a given state machine."
  (remove-watch env :change)
  sm)

(defn trigger [{:keys [machine] :as sm} ts & context]
  "Trigger a given event in a state machine."
  (doseq [trans (->coll ts)]
    (when-let [t (get-in @machine [:events trans])]
      (let [res (apply t (conj context sm))]
        (debug-log sm "(trans " (str trans) ") -> " (boolean res) " :: context " (pr-str context))))))

(defn set [{:keys [machine] :as sm} states & context]
  "Transition the state machine through a given list of states."
  (doseq [state (->coll states)]
    (when (can-transition? sm state)
      (let [cur-in (get-in @machine [:states state :in])]
        (update-sm sm [:current] conj state)
        (debug-log sm "(set " (str state) ") -> " (pr-str (current sm)))
        (when (seq cur-in)
          (debug-log sm "(in " (str state) ")")
          (doseq [func cur-in]
            (apply func (conj context sm)))))))
  sm)

(defn unset [{:keys [machine] :as sm} states & context]
  "Transition the state machine *from* a given list of states."
  (doseq [state (->coll states)]
    (when (in? sm state)
      (let [cur-out (get-in @machine [:states state :out])]
        (update-sm sm [:current] disj state)
        (debug-log sm "(unset " (str state ")") " -> " (pr-str (current sm)))
        (when (seq cur-out)
          (debug-log sm "(out " (str state) ")")
          (doseq [func cur-out]
            (apply func (conj context sm)))))))
  sm)

(defn transition [sm to-unset to-set & context]
  "Transition the state machine *between* two given states."
  (when (in? sm to-unset)
    (apply unset sm to-unset context)
    (apply set sm to-set context)))