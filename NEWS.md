# table.express 0.3.0.9000

- Improved compatibility with packages that use `dplyr` without importing `data.table` (#21).
- Added `summarize`/`summarise` verbs.

# table.express 0.3.0

- Verbs can now be used without `start_expr`/`chain`/`end_expr` with some considerations, check the
  updated vignettes (#3). Importantly, this makes `table.express` **conflict** with `dtplyr`.
- Fixed a bug that prevented automatically built chains to work after a `left_join`.
- The `*_sd` verbs now support lambdas as formulas (#19).
- Added `max_by` and `min_by` verbs.
- `mutate_sd` gained parameter `.pairwise`.
- `distinct` gained parameters `.keep` and `.n`.
- `transmute` gained parameter `.enlist` (#20).
- `filter_on` now allows empty names for data tables that already have keys (#17).
- All filtering verbs now have a `which` parameter (#17).
- Added `nest_expr` for certain scenarios that need the captured `data.table`; check the vignette
  (#16).
- Simplified `semi_join`.

# table.express 0.2.0

- Added joining verbs and a corresponding vignette describing them (#1).
- Added the `distinct` verb.
- `transmute` is no longer an alias for `select`. This is to make the latter more flexible in some
  bare selection cases (like combining several `tidyselect` calls), and leave the former more simple
  and preferable for actual transmutation cases (#4).
- Both `mutate_sd` and `transmute_sd` now support a list of functions as input, but with slightly
  different semantics; check the vignette (#8 and #11).
- Now both `mutate_sd` and `filter_sd` support the `.COL` pronoun for `.SDcols` predicates, but they
  remain eager in this regard.
- `select` and `filter_on` gained a `.negate` parameter (#9 and #6).
- `mutate` gained a `.sequential` parameter to enable usage of newly created columns.
- Added some debugging statements when option `table.express.verbose` is `TRUE`, or
  `.verbose = TRUE` in `start_expr`/`chain`.
- Updates to main vignette.
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
