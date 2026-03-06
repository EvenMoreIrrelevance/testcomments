(ns io.github.evenmoreirrelevance.testcomments.core
  (:require [io.github.evenmoreirrelevance.testcomments.util :as util]
            [clojure.test :as test]
            [clojure.walk :as walk]
            [clojure.template :as template]))

(when-let [remove-all-tests! (get (ns-publics *ns*) 'remove-all-tests!)]
  (remove-all-tests!))

(defmulti prepend-in-test-comment
  "Transforms the expansion of the forms following `form`, represented
   as a sequence of forms that will eventually be spliced into a `do`
   or similar constructs. 

   Dispatches on the resolved head of the form if it's a var."
  (fn [_acc form]
    (let [maybe-var (and
                      (seq? form)
                      (symbol? (first form))
                      (resolve (first form)))]
      (when (var? maybe-var)
        maybe-var))))

(defmethod prepend-in-test-comment :default [acc _form] acc)

(defn xp-test-comment-context
  [forms]
  (not-empty (reduce prepend-in-test-comment nil (reverse forms))))

(template/do-template [the-var] (defmethod prepend-in-test-comment the-var [acc form]
                                  (cons form acc))
  #'test/testing
  #'test/is
  #'test/are)

(defn -introduced-bindings
  [form]
  (let [!found (volatile! [])]
    (walk/postwalk (fn [f]
                     (when (simple-symbol? f)
                       (vswap! !found conj f))
                     f)
      form)
    (vec (distinct @!found))))

(defmacro values
  "Behaves like a `let`, but returns a map where `:result` is bound to the value of the last form in `body`,
   and `:values` is bound to a map from the symbols in the left hand sides of the bindings to their value.
   
   In a `test-comment` context, behaves like a `let` that evaluates its body in a `test-comment` context."
  {:clj-kondo/lint-as 'clojure.core/let}
  [[& bindings] & body]
  (let [[bindforms _] (or
                        (util/strict-uninterleave 2 bindings)
                        (throw (ex-info "expected bindings to be pairs" {:bindings bindings})))]
    `(let [~@bindings
           vals# {:values ~(into {} (map (fn [sym] [(keyword sym) sym]))
                             (-introduced-bindings bindforms))}]
       (util/catching (assoc vals# :result (do ~@body))
         (Throwable t#
           (throw (ex-info "something was thrown while evaluating `values`" vals# t#)))))))

(defmethod prepend-in-test-comment #'values
  [acc [_ bindings & body]]
  (let [expanded-body (xp-test-comment-context body)]
    (if-not expanded-body
      acc
      (cons `(let [~@bindings] ~@expanded-body) acc))))

(defmacro -bind-it
  [form & body]
  (util/ensure-false
    "`it` is already bound in the lexical environment"
    (util/assoc-truthy nil :binding (get &env 'it nil)))
  `(let [~'it ~form]
     ~@body))

(defmacro value
  "Like `values` but anaphorically binds to `it`, and returns the single value bound to `:value`
   rather than a `:values` map.
   
   Errors out if `it` is already bound."
  [form & body]
  `(-bind-it ~form
     (let [value# {:value ~'it}]
       (util/catching (assoc value# :result (do ~@body))
         (Throwable t#
           (throw (ex-info "something was thrown while evaluating `value`:" value# t#)))))))

(defmethod prepend-in-test-comment #'value
  [acc [_ form & body]]
  (let [expanded-body (xp-test-comment-context body)]
    (if-not expanded-body
      acc
      (cons `(-bind-it ~form ~@expanded-body) acc))))

(defmacro bind
  "Binds the output of `forms` to the given vars, returning it.
   In a `test-comment` context, this only establishes lexical bindings!"
  [definition & forms]
  `(let [v# (do ~@forms)
         ~definition v#]
     ~@(for [s (-introduced-bindings definition)]
         `(def ~s ~s))
     v#))

(defmethod prepend-in-test-comment #'bind
  [acc [_ & binding]]
  `[(let [~@binding] ~@acc)])

(defmacro effect
  "Evaluates `forms` for a side effect, also in a `test-comment` context."
  [& forms]
  `(do ~@forms))

(defmethod prepend-in-test-comment #'effect
  [acc [_ & forms]]
  (cons `(do ~@forms) acc))

(defn -xp-test-comment
  [test-name [form-head & comment-forms :as wrapped-form]]
  (util/ensure-false
    "wrapped form must be a comment"
    (when-not (#{`comment `clj/comment} (symbol (resolve form-head)))
      {:form-head form-head
       :form wrapped-form}))
  (when-let [test-content (xp-test-comment-context comment-forms)]
    `(test/deftest ~test-name
       ~@test-content)))

(do
  (defmacro test-comment
    "Wraps a `comment` form, expanding into a `deftest` with the given `test-name`."
    [& syntax]
    (apply -xp-test-comment syntax))
  (alter-meta! #'test-comment merge
    (select-keys (meta #'-xp-test-comment) [:arglists])))

(test-comment test-xp-test-comment-content
  (comment
    (value (xp-test-comment-context
             '[(effect "should io.github.evenmoreirrelevancet")
               "should not io.github.evenmoreirrelevancet"])
      (test/is (= it '((do "should io.github.evenmoreirrelevancet")))))

    (value (xp-test-comment-context
             '[(value 3 (test/is (= it 4)))])
      (test/is (= it '((io.github.evenmoreirrelevance.testcomments.core/-bind-it 3 (test/is (= it 4)))))))

    (value (xp-test-comment-context
             '[(bind [-tres -cuatros] [3 4])

               -tres

               (effect (prn "should print"))
               (prn "should not print")

               (test/testing "`testing` forms are picked up"
                 (test/is (= -tres 3))
                 (test/is (not= -cuatros 3)))

               (test/is (not= -tres -cuatros))
               *e])
      (test/is
        (= it
          '[(clojure.core/let [[-tres -cuatros] [3 4]]
              (do (prn "should print"))
              (test/testing "`testing` forms are picked up" (test/is (= -tres 3)) (test/is (not= -cuatros 3)))
              (test/is (not= -tres -cuatros)))])))
    *e))

(defn remove-all-tests!
  "Removes all tests in the namespace, defaulting to `*ns*`."
  ([] (remove-all-tests! *ns*))
  ([the-ns]
   (run! (partial ns-unmap the-ns)
     (eduction (keep (fn [[name var]] (when (:test (meta var)) name)))
       (ns-publics the-ns)))))

(test-comment test-test-comment
  (comment
    (value 3
      (test/is (= it 3)))

    (values [-tres 3
             -cuatros 4]
      (test/testing "`testing` forms are picked up"
        (test/is (= -tres 3))
        (test/is (not= -cuatros 3)))

      (test/is (not= -tres -cuatros)))

    *e))

(comment
  (test/run-tests)

  *e)
