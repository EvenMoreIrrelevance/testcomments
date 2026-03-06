# testcomments

Evil macros at the service of good testing.

## Rationale

Writing proper unit tests is *boooring*, but using `comment` forms is idiomatic, and writing evil macros is *fun* ... at least for me!

All the horrendous tricks that a Lisp allows are put into use to do something that would make poor ol' Rich go pale from how simple it isn't and how easy it aims to be, but that I find to be extremely practical.

## Simple Usage

Your main entry point is `test-comment`, which wraps a plain old `comment` form, which will work like any other `comment` form would in your IDE of choice, but that will be transformed into a `deftest` form.
By default, in the emitted `deftest` only `clojure.test/testing`, `clojure.test/is` and `clojure.test/are` forms are evaluated, but a couple macros are bundled
to change this behavior.

- `effect` ensures the wrapped forms are always evaluated nonetheless;
- `values` and its conveniency shorthand `value` introduce lexical bindings transparently to the transform; they also evaluate to the REPL in a convenient fashion that shows the value being bound;
- `bind` establishes *lexical* bindings up to the end of the context; on the other hand, it creates `def`s at the REPL.

## Extension

`prepend-in-test-comment` is provided as an extension point for custom behaviors, but it's a bit of an advanced case. Please refer to the source code for usage.
