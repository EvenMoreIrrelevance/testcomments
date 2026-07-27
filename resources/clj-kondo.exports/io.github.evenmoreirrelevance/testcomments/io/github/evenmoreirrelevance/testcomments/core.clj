(ns io.github.evenmoreirrelevance.testcomments.core
  (:require [clojure.walk :as walk]))

(defn -elide-nonbindings [list-bindings]
  (take-while #(not= '& %) list-bindings))

(defn -potential-map-bindings
  [subform]
  (vec
    (concat
      (keys subform)
      (filter some? [(get subform '&) (get subform :as) (get subform :select)])
      (map #(symbol (name %))
        (mapcat #(-elide-nonbindings (get subform %)) [:strs :strs! :keys :keys! :syms :syms!])))))

(comment
  (-potential-map-bindings '{& a :as b})
  *e)

(defn -introduced-bindings
  [form]
  (let [!found (volatile! #{})]
    (walk/prewalk (fn [subform]
                    (when (and
                            (simple-symbol? subform)
                            (not= '& subform))
                      (vswap! !found conj subform))
                    (if-not (map? subform)
                      subform
                      (-potential-map-bindings subform)))
      form)
    @!found))

(defmacro bind
  [definition & forms]
  (let [introduced (-introduced-bindings definition)]
    `(let [v# (do ~@forms)
           ~definition v#]
       ~@(for [s introduced] `(def ~s ~s))
       v#)))

(defmacro test-comment
  [test-name & forms]
  `(clojure.test/deftest ~test-name ~@forms))

(defmacro value
  [form & body]
  `(let [~'it ~form] ~@body))
