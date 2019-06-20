# table.express 0.1.1.9000

- Added joining verbs and a corresponding vignette describing them.
- Added `.enlist` parameter to `select` for cases when the expression should not be wrapped in a
  call to `list`.
- Minor bug fixes in `select` for unnamed arguments or a named call involving `:`.

# table.express 0.1.1

- Let `transmute_sd` build expressions with `lapply` when `.how` is a function and `.SDcols` is
  character or numeric.
- Support numeric input in `select`.
- Removed `tracemem` example from vignette to avoid CRAN problems.

# table.express 0.1.0

- Initial version with single-table verbs.
