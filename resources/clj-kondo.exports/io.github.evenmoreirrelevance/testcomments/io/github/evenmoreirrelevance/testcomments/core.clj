(ns io.github.evenmoreirrelevance.testcomments.core
  (:require [io.github.evenmoreirrelevance.testcomments.util :as util]
            [clojure.walk :as walk]))

(defn -potential-map-bindings
  [subform]
  (vec
    (concat
      (filter some? [(get subform '&) (get subform :as)])
      (keys subform)
      (map symbol (:strs subform))
      (map #(symbol (name %))
        (concat (:keys subform) (:syms subform))))))

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
       [~@(for [s introduced]
            `(def ~s ~s))
        v#])))

(defmacro test-comment
  [_test-name [form-head :as wrapped-form]]
  (util/ensure-false
    "wrapped form not a comment"
    (when-not (= `comment (symbol (resolve form-head)))
      {:form-head form-head
       :form wrapped-form}))
  wrapped-form)

(defmacro value
  [form & body]
  `(let [~'it ~form] ~@body))
