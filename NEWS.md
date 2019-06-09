# table.express 0.1.1.9000

- The `.SDcols` expressions in `mutate_sd`/`filter_sd` are no longer eager, but the ones in
  `transmute_sd` are if they use the `.COL` pronoun. Check the updated vignette (section Eager
  verbs).

# table.express 0.1.1

- Let `transmute_sd` build expressions with `lapply` when `.how` is a function and `.SDcols` is
  character or numeric.
- Support numeric input in `select`.
- Removed `tracemem` example from vignette to avoid CRAN problems.

# table.express 0.1.0

- Initial version with single-table verbs.
