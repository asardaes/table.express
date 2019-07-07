# table.express 0.2.0.9000

- Fixed a bug that prevented automatically built chains to work after a `left_join`.
- `distinct` gained `.keep_all` and `.keep_old` parameters.
- `filter_on` now allows empty names for data tables that already have keys.
- All filtering verbs now have a `which` parameter.
- Added `nest_expr` for certain scenarios that need the captured `data.table`.
- Simplified `semi_join`, and it's no longer eager.
