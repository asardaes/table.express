# table.express 0.1.1.9000

- Added joining verbs and a corresponding vignette describing them.
- Added the `distinct` verb.
- `transmute` is no longer an alias for `select`. This is to make the latter more flexible in some
  bare selection cases (like combining several `tidyselect` calls), and leave the former more simple
  and preferable for actual transmutation cases.
- Both `mutate_sd` and `transmute_sd` now support a list of functions as input, but with slightly
  different semantics; check the vignette.
- Now both `mutate_sd` and `filter_sd` support the `.COL` pronoun for `.SDcols` predicates, but they
  remain eager in this regard.
- `select` and `filter_on` gained a `.negate` parameter.
- `mutate` gained a `.sequential` parameter to enable usage of newly created columns.
- Added some debugging statements when option `table.express.verbose` is `TRUE`, or
  `.verbose = TRUE` in `start_expr`/`chain`.
- Semi-breaking change: `*_sd` verbs now have `.SDcols` before their ellipsis. This should only
  require code adjustment for `transmute_sd` when `.SDcols` was left missing.
- Updates to main vignette.
