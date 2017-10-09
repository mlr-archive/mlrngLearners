mlr.learners$add(
  LearnerClassif$new(
    name = "ksvm",
    package = "kernlab",
    par.set = ParamSetFlat$new(
      params = list(
        ParamFlag$new(id = "scaled", default = TRUE),
        ParamFactor$new(id = "type", default = "C-svc", values = c("C-svc", 
          "nu-svc", "C-bsvc", "spoc-svc", "kbb-svc")),
        ParamFactor$new(id = "kernel", default = "rbfdot", values = c("vanilladot",
          "polydot", "rbfdot", "tanhdot", "laplacedot", "besseldot", "anovadot", "splinedot")),
        ParamReal$new(id = "C", lower = 0, default = 1, special.vals = NA),
        ParamReal$new(id = "nu", lower = 0, default = 0.2, special.vals = NA),
        ParamReal$new(id = "epsilon", default = 0.1, special.vals = NA),
        ParamReal$new(id = "sigma", lower = 0, special.vals = NA),
        ParamInt$new(id = "degree", default = 3L, lower = 1L, special.vals = NA),
        ParamReal$new(id = "scale", default = 1, lower = 0, special.vals = NA),
        ParamReal$new(id = "offset", default = 1, special.vals = NA),
        ParamInt$new(id = "order", default = 1L, special.vals = NA),
        ParamReal$new(id = "tol", default = 0.001, lower = 0),
        ParamFlag$new(id = "shrinking", default = TRUE),
        ParamUntyped$new(id = "class.weights", len = NA_integer_, lower = 0),
        ParamFlag$new(id = "fit", default = TRUE),
        ParamInt$new(id = "cache", default = 40L, lower = 1L)
      ),
      restriction = quote(
        is.na(C) | type %in% c("C-svc", "C-bsvc", "spoc-svc", "kbb-svc") &
        is.na(nu) | type == "nu-svc" &
        is.na(epsilon) | type %in% c("eps-svr", "nu-svr", "eps-bsvm") &
        is.na(sigma) | kernel %in% c("rbfdot", "anovadot", "besseldot", "laplacedot") &
        is.na(degree) | kernel %in% c("polydot", "anovadot", "besseldot") &
        is.na(scale) | kernel %in% c("polydot", "tanhdot") &
        is.na(offset) | kernel %in% c("polydot", "tanhdot") &
        is.na(order) | kernel == "besseldot"
      ) 
    ),
    par.vals = list(fit = FALSE),
    properties = c("twoclass", "multiclass", "feat.numeric", "feat.factor", "prob"),
    train = function(task, subset, weights = NULL, ...) {
      kpar = learnerArgsToControl(control = list, degree = self$par.vals$degree,
        offset = self$par.vals$offset, scale = self$par.vals$scale, sigma = self$par.vals$sigma,
        order = self$par.vals$order, length = self$par.vals$length, lambda = self$par.vals$lambda,
        normalized = self$par.vals$normalized)
      f = task$formula
      pm = self$predict.type == "prob"
      if (base::length(kpar) > 0L)
        kernlab::ksvm(f, data = getTaskData(task, subset, props = self$properties),
          kpar = kpar, prob.model = pm, ...)
      else
        kernlab::ksvm(f, data = getTaskData(task, subset, props = self$properties),
          prob.model = pm, ...)
    },
    predict = function(model, newdata, ...) { #FIXME: not working right now
      type = switch(self$predict.type, prob = "probabilities", "response")
      kernlab::predict(model$rmodel, newdata = newdata, type = type, ...)
    }
  ))
