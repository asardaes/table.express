# table.express 0.1.1.9000

- Added joining verbs and a corresponding vignette describing them (#1).
- `transmute` is no longer an alias for `select`. This is to make the latter more flexible in some
  bare selection cases (like combining several `tidyselect` calls), and leave the former more simple
  and preferable for actual transmutation cases (#4).
- Now both `mutate_sd` and `filter_sd` support the `.COL` pronoun for `.SDcols` predicates, but they
  remain eager in this regard.
- `select` and `filter_on` gained a `.negate` parameter (#9 and #6).
- `mutate` gained a `.sequential` parameter to enable usage of newly created columns.
- Added some debugging statements when option `table.express.verbose` is `TRUE`, or
  `.verbose = TRUE` in `start_expr`/`chain`.
- Minor updates to main vignette.
- Semi-breaking change: `*_sd` verbs now have `.SDcols` before their ellipsis. This should only
  require code adjustment for `transmute_sd` when `.SDcols` was left missing.

Special thanks to @leungi for checking consistency with `dplyr` in several cases.

# table.express 0.1.1

- Let `transmute_sd` build expressions with `lapply` when `.how` is a function and `.SDcols` is
  character or numeric.
- Support numeric input in `select`.
- Removed `tracemem` example from vignette to avoid CRAN problems.

# table.express 0.1.0

- Initial version with single-table verbs.
