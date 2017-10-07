context("classif.ksvm")

test_that("basic tests", {
  lrn = mlr.learners$get("classif.ksvm")
  expect_learner(lrn)
})
