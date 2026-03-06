(ns io.github.evenmoreirrelevance.testcomments.core
  (:require [io.github.evenmoreirrelevance.testcomments.util :as util]
            [clojure.test :as test]
            [clojure.walk :as walk]
            [clojure.template :as template]))

(defmulti expand-in-test-comment
  "Expands the form in a `test-comment` context,
   dispatching on the symbolic representation of the head
   if it resolves to a var.
 
   Note that if no var is resolved, the form is simply ignored.

   Also note that expansion isn't to a fixpoint like with macros;
   to trigger another expansion, you must call `expand-in-test-comment` manually, 
   and to propagate the expansion to subforms you'll need to use `expand-test-comment-forms`."
  (fn [form]
    (let [maybe-var (and
                      (seq? form)
                      (symbol? (first form))
                      (resolve (first form)))]
      (when (var? maybe-var)
        (symbol maybe-var)))))

(defmethod expand-in-test-comment :default [_form] nil)

(defn expand-test-comment-forms
  "Expands `forms` in a `test-comment` context, discarding `nil` expansions.
   Returns `nil` if no form expands into anything meaningful."
  [forms]
  (not-empty (keep expand-in-test-comment forms)))

(template/do-template [the-var] (defmethod expand-in-test-comment the-var [form] form)
  `test/testing
  `test/is
  `test/are)

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

(defmethod expand-in-test-comment `values
  [[_ bindings & body]]
  (let [expanded-body (expand-test-comment-forms body)]
    (when expanded-body
      `(let [~@bindings] ~@expanded-body))))

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

(defmethod expand-in-test-comment `value
  [[_ form & body]]
  (when-let [expanded-body (expand-test-comment-forms body)]
    `(-bind-it ~form ~@expanded-body)))

(defmacro bind
  "Binds the output of `forms` to the given vars, returning it. 
   Is evaluated in a `test-comment` context."
  [definition & forms]
  `(let [v# (do ~@forms)
         ~definition v#]
     ~@(for [s (-introduced-bindings definition)]
         `(def ~s ~s))
     v#))

; this actually works just fine given that `form` is now safe from `prepend-in-test-comment`.
(defmethod expand-in-test-comment `bind 
  [form] 
  form)

(defmacro effect
  "Evaluates `forms` for a side effect, also in a `test-comment` context."
  [& forms]
  `(do ~@forms))

(defmethod expand-in-test-comment `effect
  [[_ & forms]] 
  `(do ~@forms))

(defmacro value= 
  [form equal]
  `(value ~form (test/is (= ~equal ~'it))))

(defmethod expand-in-test-comment `value=
  [[_ form equal]]
  (expand-in-test-comment `(value ~form (test/is (= ~'it ~equal)))))

(defn -xp-test-comment
  [test-name [form-head & comment-forms :as wrapped-form]]
  (util/ensure-false
    "wrapped form must be a comment"
    (when-not (#{`comment `clj/comment} (symbol (resolve form-head)))
      {:form-head form-head
       :form wrapped-form}))
  (when-let [test-content (expand-test-comment-forms comment-forms)]
    `(test/deftest ~test-name
       ~@test-content)))

(do
  (defmacro test-comment
    "Wraps a `comment` form, expanding into a `deftest` with the given `test-name`."
    [& syntax]
    (apply -xp-test-comment syntax))
  (alter-meta! #'test-comment merge
    (select-keys (meta #'-xp-test-comment) [:arglists])))

(test-comment test--introduced-bindings
  (comment
    (value (-introduced-bindings '[{:keys [a ::b c]
                                    :syms [foo/bar]} d])
      (test/testing "handles namespaced keywords in `:keys` and namespaced symbols in `:syms`"
        (test/is (= (set it) '#{a b c d bar}))))

    (value (-introduced-bindings '[[a & rest] {& b}])
      (test/testing "discards `&` syntax marker"
        (test/is (= (set it) '#{a b rest}))))

    (value (-introduced-bindings '[{{& a :as b} 'miss}])
      (test/testing "descends into map correctly")
      (test/is (= (set it) '#{a b})))

    *e))

(test-comment test-xp-test-comment-content
  (comment
    (value (expand-test-comment-forms
             '[(effect "should io.github.evenmoreirrelevancet")
               "should not io.github.evenmoreirrelevancet"])
      (test/is (= it '((do "should io.github.evenmoreirrelevancet")))))

    (value= (expand-test-comment-forms
              '[(value 3 (test/is (= it 4)))])
      '((io.github.evenmoreirrelevance.testcomments.core/-bind-it 3 (test/is (= it 4)))))

    (value= (expand-test-comment-forms
              '[(bind [-tres -cuatros] [3 4])

                -tres

                (effect (prn "should print"))
                (prn "should not print")

                (test/testing "`testing` forms are picked up"
                  (test/is (= -tres 3))
                  (test/is (not= -cuatros 3)))

                (test/is (not= -tres -cuatros))
                *e])
      '((bind [-tres -cuatros] [3 4])
        (do (prn "should print"))
        (test/testing "`testing` forms are picked up" (test/is (= -tres 3)) (test/is (not= -cuatros 3)))
        (test/is (not= -tres -cuatros))))

    ; contentious behavior
    (values [expand-n-eval (fn [bad-form]
                             {:form bad-form
                              :expanded (expand-test-comment-forms bad-form)
                              :evaluated (util/catching {:ok? (eval bad-form)}
                                           (RuntimeException e {:err? e}))})
             bad-ns (expand-n-eval '(tezt/is (= 1 2)))
             bad-var (expand-n-eval '(test/EMI_no_such_var 3))]
      (test/testing "things that would induce a compilation failure are ignored instead"
        (test/testing "bad namespace"
          (test/is (some? (:err? (:evaluated bad-ns))))
          (test/is (nil? (:expanded bad-ns))))
        (test/testing "bad var"
          (test/is (some? (:err? (:evaluated bad-var))))
          (test/is (nil? (:expanded bad-var))))))

    (values [expanded
             (expand-test-comment-forms
               `[(bind ^:dynamic ^:private var# 3)
                 (effect (binding [var# 4] var#))])
             evaluated
             (util/catching (eval (cons `do expanded))
               (Throwable t t))]
      (test/testing "dynamic vars handled correctly in `bind`"
        (test/is (= evaluated 4))))

    (comment
      ; fails due to lack of `&env` info in the dispatch for `prepend-in-test-comment`
      (value (expand-test-comment-forms
               '(values [effect (constantly :bar)]
                  (effect :lol)))
        (test/testing "&env accounted for in `xp-test-comment-context`."
          (= nil it)))

      "these fail")

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
    
    (values [-tres 3
             -cuatros 4]
      (test/testing "`testing` forms are picked up"
        (test/is (= -tres 3))
        (test/is (not= -cuatros 3)))

      (test/is (not= -tres -cuatros)))

    *e))

(comment
  (remove-all-tests!)
  (test/run-tests)

  *e)
