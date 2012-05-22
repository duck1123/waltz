(ns waltz.macros)

(defn- state* []
  {:in []
   :out []
   :constraints []})

(defmacro defstate
  [m name & body]
  `(let [s# (-> ~(state*) ~@body)]
     (waltz.state/add-state ~m ~name s#)))

(defmacro defevent
  [m name params & body]
  `(waltz.state/add-event ~m ~name (fn ~params
                                      ~@body)))

(defmacro in [m & body]
  (if (second body)
    `(waltz.state/update-in ~m [:in] (fn ~@body))
    `(waltz.state/update-in ~m [:in] ~@body)))

(defmacro out [m & body]
  (if (second body)
    `(waltz.state/update-in ~m [:out] (fn ~@body))
    `(waltz.state/update-in ~m [:out] ~@body)))
