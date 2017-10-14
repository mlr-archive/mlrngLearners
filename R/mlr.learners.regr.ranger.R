mlr.learners$add(LearnerRegr$new(
  name = "ranger",
  package = "ranger",
  par.set = ParamSetFlat$new(
    params = list(
      ParamInt$new(id = "num.trees", lower = 1L, default = 500L),
      # FIXME: Add default value when data dependent defaults are implemented: mtry=floor(sqrt(#independent vars))
      ParamInt$new(id = "mtry", lower = 1L),
      ParamInt$new(id = "min.node.size", lower = 1L, default = 5L),
      ParamFlag$new(id = "replace", default = TRUE),
      ParamReal$new(id = "sample.fraction", lower = 0L, upper = 1L),
      ParamUntyped$new(id = "split.select.weights", lower = 0, upper = 1),
      # FIXME: split.select.weights was vector param, untyped correct?
      ParamFactor$new(id = "splitrule", values = c("variance", "maxstat"), default = "variance"),
      ParamReal$new(id = "alpha", lower = 0L, upper = 1L, default = 0.5, special.vals = NA),
      ParamReal$new(id = "minprop", lower = 0L, upper = 1L, default = 0.1, special.vals = NA),
      ParamUntyped$new(id = "always.split.variables"),
      ParamFlag$new(id = "respect.unordered.factors", default = FALSE),
      ParamFactor$new(id = "importance", values = c("none", "impurity", "permutation"), default = "none"),
      ParamFlag$new(id = "write.forest", default = TRUE),
      ParamFlag$new(id = "scale.permutation.importance", default = FALSE, special.vals = NA),
      ParamInt$new(id = "num.threads", lower = 1L, when = "both"),
      ParamFlag$new(id = "save.memory", default = FALSE),
      ParamFlag$new(id = "verbose", default = TRUE, when = "both"),
      ParamInt$new(id = "seed", when = "both"),
      ParamFlag$new(id = "keep.inbag", default = FALSE)
    ),
    restriction = quote(
      (is.na(scale.permutation.importance) | importance == "permutation") &
      (is.na(alpha) | splitrule == "maxstat") &
      (is.na(minprop) | splitrule == "maxstat")
    ),
    par.set = makeParamSet(
      mrequires = quo= quote(splitrule == "maxstat")),
  ),
  par.vals = list(num.threads = 1L, verbose = FALSE, respect.unordered.factors = TRUE),
  properties = c("formula", "feat.numeric", "feat.factor", "feat.ordered", "oobpreds", "featimp", "parallel"),
  
  train = function(task, subset, ...) {
    data = getTaskData(task, subset = subset, type = "train", props = self$properties)
    ranger::ranger(formula = task$formula, data = data, ...)
  },
  
  predict = function(model, newdata, ...) {
    p = predict(object = model$rmodel, data = newdata, ...)
    unname(p$predictions)
  },
  
  model.extractors = list(
    OOBPredictions = function(model, task, subset, ...) {
      model$predictions
    },
    FeatureImportance = function(model, task, subset, ...) {
      has.fiv = self$par.vals$importance
      if (is.null(has.fiv) || has.fiv == "none") {
        stop("You must set the learners parameter value for importance to
          'impurity' or 'permutation' to compute feature importance")
      }
      ranger::importance(model)
    }
  )
))
