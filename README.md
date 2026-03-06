# testcomments

Evil macros at the service of good testing.

[![Clojars Project](https://img.shields.io/clojars/v/io.github.evenmoreirrelevance/testcomments.svg)](https://clojars.org/io.github.evenmoreirrelevance/testcomments)

[![bb compatible](https://raw.githubusercontent.com/babashka/babashka/master/logo/badge.svg)](https://book.babashka.org#badges)

## Rationale

Writing proper unit tests is *boooring*, but using `comment` forms is idiomatic, and writing evil macros is *fun* ... at least for me!

All the horrendous tricks that a Lisp allows are put into use to do something that would make poor ol' Rich go pale from how simple it isn't and how easy it aims to be, but that I find to be extremely practical.

## Simple Usage

Your main entry point is `test-comment`, which wraps a plain old `comment` form, which will work like any other `comment` form would in your IDE of choice, but that will be transformed into a `deftest` form.
By default, in the emitted `deftest` only `clojure.test/testing`, `clojure.test/is` and `clojure.test/are` forms are evaluated, but a couple macros are bundled
to change this behavior.

- `effect` ensures the wrapped forms are always evaluated nonetheless;
- `values` and its conveniency shorthand `value` introduce lexical bindings transparently to the transform; they also evaluate to the REPL in a convenient fashion that shows the value being bound;
- `bind` establishes `def` bindings, and conveniently returns what's bound.

## Extension

`prepend-in-test-comment` is provided as an extension point for custom behaviors, but it's a bit of an advanced case. Please refer to the source code for usage.
