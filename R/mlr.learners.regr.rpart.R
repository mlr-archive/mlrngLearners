mlr.learners$add(LearnerRegr$new(
  name = "rpart",
  package = "rpart",
  par.set = ParamSetFlat$new(
    params = list(
      ParamInt$new(id = "minsplit", default = 20L, lower = 1L),
      ParamReal$new(id = "cp", default = 0.01, lower = 0, upper = 1),
      ParamInt$new(id = "maxcompete", default = 4L, lower = 0L),
      ParamInt$new(id = "maxsurrogate", default = 5L, lower = 0L),
      ParamFactor$new(id = "usesurrogate", default = 2L, values = 0:2),
      ParamFactor$new(id = "surrogatestyle", default = 0L, values = 0:1),
      ParamInt$new(id = "maxdepth", default = 30L, lower = 1L, upper = 30L), # we use 30 as upper limit, see docs of rpart.control
      ParamInt$new(id = "xval", default = 10L, lower = 0L)
    )
  ),
  par.vals = list(),
  properties = c("missings", "feat.numeric", "feat.factor", "feat.ordered", "oobpreds", "featimp", "se", "formula"), # FIXME: Does rpart really support se estimation?
  train = function(task, subset, ...) {
    data = getTaskData(task, subset = subset, type = "train", props = self$properties)
    rpart::rpart(task$formula, data, ...)
  },
  predict = function(model, newdata, ...) {
    unname(predict(model$rmodel, newdata = newdata, type = "vector", ...))
  }
))
