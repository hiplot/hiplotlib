p <- call_ezcox(
  data = data,
  covariates = unlist(conf$dataArg[[1]][[1]]$value),
  controls = unlist(conf$dataArg[[1]][[2]]$value),
  vars_to_show = unlist(conf$dataArg[[1]][[3]]$value),
  merge_models = conf$extra$merge_models,
  drop_controls = conf$extra$drop_controls,
  add_caption = conf$extra$add_caption
)

export_single(p)
