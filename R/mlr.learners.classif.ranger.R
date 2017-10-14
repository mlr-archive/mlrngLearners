mlr.learners$add(LearnerClassif$new(
  name = "ranger",
  package = "ranger",
  par.set = ParamSetFlat$new(
    params = list(
      ParamInt$new(id = "num.trees", lower = 1L, default = 500L),
      # FIXME: Add default value when data dependent defaults are implemented: mtry=floor(sqrt(#independent vars))
      ParamInt$new(id = "mtry", lower = 1L),
      ParamInt$new(id = "min.node.size", lower = 1L, deault = 1L),
      ParamFlag$new(id = "replace", default = TRUE),
      ParamReal$new(id = "sample.fraction", lower = 0L, upper = 1L),
      ParamUntyped$new(id = "split.select.weights", lower = 0, upper = 1),
      ParamFactor$new(id = "splitrule", values = c("gini", "extratrees"), default = "gini"),
      # FIXME: split.select.weights was vector param, untyped correct?
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
      (is.na(scale.permutation.importance) | importance == "permutation")
    ),
    par.vals = list(num.threads = 1L, verbose = FALSE, respect.unordered.factors = TRUE),
    properties = c("twoclass", "multiclass", "prob", "feat.numeric", "feat.factor", "feat.ordered", "featimp", "weights", "parallel", "formula"),
    
    train = function(task, subset, weights = NULL, ...) {
      tn = task$target
      data = getTaskData(task, subset = subset, type = "train", target.as = "factor", props = self$properties)
      ranger::ranger(formula = task$formula, data = data, probability = (self$predict.type == "prob"),
        case.weights = weights, ...)
    },
    
    predict = function(model, newdata, ...) {
      pt = self$predict.type
      if (pt == "response") {
        p = predict(model$rmodel, data = newdata, type = "response", ...)
        return(as.character(p$predictions))
      } else { # FIXME: Probability estimation needs to be fixed
        p = predict(model$rmodel, data = newdata, predict.all = TRUE, ...)
        return(p$predictions)
      }
    },
    
    model.extractors = list( # FIXME: not working right now
      OOBPredictions = function(model, task = NULL, subset = NULL, ...) {
        model$predictions
      },
      featureImportance = function(model, task = NULL, subset = NULL, ...) { # FIXME: not working right now
        has.fiv = self$par.vals$importance
        if (is.null(has.fiv) || has.fiv == "none") {
          stop("You must set the parameter value for 'importance' to
        'impurity' or 'permutation' to compute feature importance.")
        }
        ranger::importance(model$rmodel)
      }
    )
  ))
  