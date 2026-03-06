- lots of sharp edges:
    * Does `bind` need to avoid ns pollution so desperately? Maybe we should just emit `def`s regardless,
      as you'd expect the `def`s not to hit regular code anyway.
    * `&env` handling in `prepend-in-test-comment`
    * potential to ignore forms with unresolved heads that would fail to compile, e.g. `(tezt/is (= :foo :bar))`,
      but linters against such things are commonplace and the philosophy is to discard as much as we can anyway.
- AOT and reloaded workflow check.
