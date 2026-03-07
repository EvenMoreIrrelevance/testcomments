- `&env` handling in `expand-in-test-comment`.
    * c.f.r. the failing test in `test-xp-test-comment-content` for the issue I see,
      i.e. that the lexical environment is not accounted for.
      This issue should only arise if one `:refer`s things.
    * Solving this requires passing `&env` (or at least the set of bound locals) around, 
      and most crucially computing it for `values` after the `let` so that we can ignore the form properly.
    * For now compatibility concerns would be limited as this currently only aims to be Babashka and Clojure compatible.
- AOT and reloaded workflow checks.
