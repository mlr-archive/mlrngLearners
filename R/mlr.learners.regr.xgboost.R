mlr.learners$add(LearnerRegr$new(
  name = "xgboost",
  package = "xgboost",
  par.set = ParamSetFlat$new(
    params = list(
      # FIXME: Old Comments:
      # we pass all of what goes in 'params' directly to ... of xgboost
      # makeUntypedLearnerParam(id = "params", default = list()),
      ParamFactor$new(id = "booster", default = "gbtree", values = c("gbtree", "gblinear", "dart")),
      ParamUntyped$new(id = "watchlist", default = NULL),
      ParamReal$new(id = "eta", default = 0.3, lower = 0, upper = 1),
      ParamReal$new(id = "gamma", default = 0, lower = 0),
      ParamInt$new(id = "max_depth", default = 6L, lower = 1L),
      ParamReal$new(id = "min_child_weight", default = 1, lower = 0),
      ParamReal$new(id = "subsample", default = 1, lower = 0, upper = 1),
      ParamReal$new(id = "colsample_bytree", default = 1, lower = 0, upper = 1),
      ParamReal$new(id = "colsample_bylevel", default = 1, lower = 0, upper = 1),
      ParamInt$new(id = "num_parallel_tree", default = 1L, lower = 1L),
      ParamReal$new(id = "lambda", default = 1, lower = 0),
      ParamReal$new(id = "lambda_bias", default = 0, lower = 0),
      ParamReal$new(id = "alpha", default = 0, lower = 0),
      ParamUntyped$new(id = "objective", default = "reg:linear"),
      ParamUntyped$new(id = "eval_metric", default = "rmse"),
      ParamReal$new(id = "base_score", default = 0.5),
      ParamReal$new(id = "max_delta_step", lower = 0, default = 0),
      ParamReal$new(id = "missing", default = NULL, when = "both",
        special.vals = list(NA, NA_real_, NULL)),
      ParamUntyped$new(id = "monotone_constraints", default = 0, lower = -1, upper = 1),
      ParamReal$new(id = "tweedie_variance_power", lower = 1, upper = 2, default = 1.5, special.vals = NA),
      ParamInt$new(id = "nthread", lower = 1L),
      ParamInt$new(id = "nrounds", default = 1L, lower = 1L),
      # FIXME nrounds seems to have no default in xgboost(), if it has 1, par.vals is redundant
      ParamUntyped$new(id = "feval", default = NULL),
      ParamInt$new(id = "verbose", default = 1L, lower = 0L, upper = 2L),
      ParamInt$new(id = "print_every_n", default = 1L, lower = 1L, special.vals = NA),
      ParamInt$new(id = "early_stopping_rounds", default = NULL, lower = 1L, special.vals = list(NULL)),
      ParamFlag$new(id = "maximize", default = NULL, special.vals = list(NULL)),
      ParamFactor$new(id = "sample_type", default = "uniform", values = c("uniform", "weighted"), special.vals = NA),
      ParamFactor$new(id = "normalize_type", default = "tree", values = c("tree", "forest"), special.vals = NA),
      ParamReal$new(id = "rate_drop", default = 0, lower = 0, upper = 1, special.vals = NA),
      ParamReal$new(id = "skip_drop", default = 0, lower = 0, upper = 1, special.vals = NA),
      # FIXME: Still relevant?? TODO: uncomment the following after the next CRAN update, and set max_depth's lower = 0L
      # ParamFlag$new(id = "one_drop", default = FALSE, special.vals = NA),
      # ParamFactor$new(id = "tree_method", default = "exact", values = c("exact", "hist"), special.vals = NA),
      # ParamFactor$new(id = "grow_policy", default = "depthwise", values = c("depthwise", "lossguide"), special.vals = NA),
      # ParamInt$new(id = "max_leaves", default = 0L, lower = 0L, special.vals = NA),
      # ParamInt$new(id = "max_bin", default = 256L, lower = 2L, special.vals = NA),
      ParamUntyped$new(id = "callbacks", default = list())
    ),
    restriction = quote(
      (is.na(tweedie_variance_power) | objective == "reg:tweedie") &
      (is.na(print_every_n) | verbose == 1L) &
      (is.na(sample_type) | booster == "dart") &
      (is.na(normalize_type) | booster == "dart") &
      (is.na(rate_drop) | booster == "dart") &
      (is.na(skip_drop) | booster == "dart") &
      # (is.na(one_drop) | booster == "dart") &
      # (is.na(tree_method) | booster != "gblinear") &
      # (is.na(grow_policy) | tree_method == "hist") &
      # (is.na(max_leaves) | grow_policy == "lossguide") &
      # (is.na(max_bin) | tree_method == "hist")
    )
  ),
  par.vals = list(nrounds = 1L, verbose = 0L),
  properties = c("feat.numeric", "weights", "featimp", "missings"),

  train = function(task, subset, weights = NULL, ...) {
    parlist = list(...)

    if (is.null(parlist$objective))
      parlist$objective = "reg:linear"

    data = getTaskData(task, subset = subset, type = "train", props = self$properties)
    d = BBmisc::dropNamed(data, drop = task$target)
    truth = task$truth()
    parlist$data = xgboost::xgb.DMatrix(data = data.matrix(d), label = data.matrix(truth))

    if (!is.null(weights))
      xgboost::setinfo(parlist$data, "weight", weights)

    if (is.null(parlist$watchlist))
      parlist$watchlist = list(train = parlist$data)

    do.call(xgboost::xgb.train, parlist)
  },

  predict = function(model, newdata, ...) {
    predict(model$rmodel, newdata = data.matrix(newdata), ...)
  },

  model.extractors = list(
    featureImportance = function(model, task, subset, ...) {
      mod = model$rmodel
      imp = xgboost::xgb.importance(feature_names = model$task$features,
        model = mod, ...)
      fiv = imp$Gain
      setNames(fiv, imp$Feature)
    }
  )
))
