# table.express 0.3.0

- Verbs can now be used without `start_expr`/`chain`/`end_expr` with some considerations, check the
  updated vignettes. Importantly, this makes `table.express` **conflict** with `dtplyr`.
- Fixed a bug that prevented automatically built chains to work after a `left_join`.
- The `*_sd` verbs now support lambdas as formulas.
- Added `max_by` and `min_by` verbs.
- `mutate_sd` gained parameter `.pairwise`.
- `distinct` gained parameters `.keep` and `.n`.
- `transmute` gained parameter `.enlist`.
- `filter_on` now allows empty names for data tables that already have keys.
- All filtering verbs now have a `which` parameter.
- Added `nest_expr` for certain scenarios that need the captured `data.table`; check the vignette.
- Simplified `semi_join`.
