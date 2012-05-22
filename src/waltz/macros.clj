(ns waltz.macros)

(defmacro defstate
  [m name & body]
  `(let [s# (-> (waltz.state/state*)
                ~@body)]
     (waltz.state/add-state ~m ~name s#)))

(defmacro defevent
  [m name params & body]
  `(waltz.state/add-event ~m ~name (fn ~params
                                      ~@body)))

(defmacro in [m & body]
  (if (second body)
    `(waltz.state/in* ~m (fn ~@body))
    `(waltz.state/in* ~m ~@body)))

(defmacro out [m & body]
  (if (second body)
    `(waltz.state/out* ~m (fn ~@body))
    `(waltz.state/out* ~m ~@body)))