# table.express 0.1.1.9000

- The `.SDcols` expressions in `mutate_sd`/`filter_sd` are no longer eager, but the ones in
  `transmute_sd` are if they use the `.COL` pronoun. Check the updated vignette (section Eager
  verbs).
