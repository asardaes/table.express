# table.express 0.1.1.9000

- Added joining verbs and a corresponding vignette describing them.
- `transmute` is no longer an alias for `select`. This is to make the latter more flexible in some
  bare selection cases (like combining several `tidyselect` calls), and leave the former more simple
  and preferable for actual transmutation cases.
- Now both `mutate_sd` and `filter_sd` support the `.COL` pronoun for `.SDcols` predicates, but they
  remain eager in this regard.
- `filter_on` gained a `.negate` parameter.
- `mutate` gained a `.sequential` parameter to enable usage of newly created columns.
- Minor updates to main vignette.
