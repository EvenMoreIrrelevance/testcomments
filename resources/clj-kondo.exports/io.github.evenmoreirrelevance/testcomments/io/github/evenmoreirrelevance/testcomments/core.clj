(ns io.github.evenmoreirrelevance.testcomments.core
  (:require [io.github.evenmoreirrelevance.testcomments.util :as util]
            [clojure.walk :as walk]))

(defmacro bind
  [definition & forms]
  (let [introduced (let [!found (volatile! [])]
                     (walk/postwalk (fn [f]
                                      (when (simple-symbol? f)
                                        (vswap! !found conj f))
                                      f)
                       definition)
                     @!found)]
    `(let [v# (do ~@forms)
           ~definition v#]
       [~@(for [s introduced]
            `(def ~s ~s))])))

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
