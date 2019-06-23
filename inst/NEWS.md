# table.express 0.1.1.9000

- Added joining verbs and a corresponding vignette describing them.
- `transmute` is no longer an alias for `select`. This is to make the latter more flexible in some
  bare selection cases (like combining several `tidyselect` calls), and leave the former more simple
  and preferable for actual transmutation cases.
- `filter_on` gains a `.negate` parameter.
