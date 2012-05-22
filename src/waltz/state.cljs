(ns waltz.state
  (:refer-clojure :exclude [set]))

(declare get-name)

(defn debug-log [sm v & vs]
  (when (and js/console (@sm :debug))
    (let [s (apply str (get-name sm) " :: " v vs)]
      (.log js/console s))))

(defn ->coll [v]
  (if (coll? v)
    v
    [v]))

(defn state* []
  {:in []
   :out []
   :constraints []})

(defn get-in-sm [[state _] ks]
  (get-in @state ks))

(defn get-name [sm]
  (get-in-sm sm [:name]))

(defn assoc-sm [[state _] ks v]
  (swap! state #(assoc-in % ks v)))

(defn update-sm [[state _] & fntail]
  (swap! state #(apply update-in % fntail)))

(defn current [sm]
  (get-in-sm sm [:current]))

(defn in? [sm state]
  ((current sm) state))

(defn has-state? [sm state]
  (get-in-sm sm [:states state]))

(defn has-event? [sm trans]
  (get-in-sm sm [:events trans]))

(defn add-state [sm name v]
  (assoc-sm sm [:states name] v))

(defn add-event [sm name v]
  (assoc-sm sm [:events name] v))

(defn in* [state fn]
  (update-in state [:in] conj fn))

(defn out* [state fn]
  (update-in state [:out] conj fn))

(defn constraint [state fn]
  (update-in state [:constraint] conj fn))

(defn can-transition? [sm state]
  (let [trans (get-in-sm sm [:states state :constraints])]
    (if trans
      (every? #(% state) trans)
      true)))

(defn set-debug [sm dbg]
  (assoc-sm sm :debug dbg))

;;; Public API

(def ^{:private true
       :doc "A global registry of state machines."}
  registry (atom {}))

(defn by-name [n]
  "Returns a registered state machine for a given name."
  (@registry n))

(defn machine [n]
  "Create an abstract state machine."
  {:pre [(keyword? n)
         (nil? (@registry n))]}
  (let [m (atom {:debug true
                 :name (name n)
                 :states {}
                 :events {}})]
    (swap! registry assoc n m)))

(defn init [m]
  "Create an instance of an abstract state machine."
  ([(atom {:current #{}}) m]))

(defn trigger [sm ts & context]
  "Trigger a given event in a state machine."
  (doseq [trans (->coll ts)]
    (when-let [t (get-in-sm sm [:events trans])]
      (let [res (apply t context)]
        (debug-log sm "(trans " (str trans) ") -> " (boolean res) " :: context " (pr-str context))))))

(defn set [sm states & context]
  "Transition the state machine through a given list of states."
  (doseq [state (->coll states)]
    (when (can-transition? sm state)
      (let [cur-in (get-in-sm sm [:states state :in])]
        (update-sm sm [:current] conj state)
        (debug-log sm "(set " (str state) ") -> " (pr-str (current sm)))
        (when (seq cur-in)
          (debug-log sm "(in " (str state) ")")
          (doseq [func cur-in]
            (apply func context))))))
  sm)

(defn unset [sm states & context]
  "Transition the state machine *from* a given list of states."
  (doseq [state (->coll states)]
    (when (in? sm state)
      (let [cur-out (get-in-sm sm [:states state :out])]
        (update-sm sm [:current] disj state)
        (debug-log sm "(unset " (str state ")") " -> " (pr-str (current sm)))
        (when (seq cur-out)
          (debug-log sm "(out " (str state) ")")
          (doseq [func cur-out]
            (apply func context))))))
  sm)

(defn set-ex [sm to-unset to-set & context]
  "Set a state machine to an *exclusive* state, unsetting any current
state it might be in."
  (apply unset sm to-unset context)
  (apply set sm to-set context))
