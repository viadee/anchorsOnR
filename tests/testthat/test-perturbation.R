context("perturbation")

test_that("featureless tabular perturbation can be applied",{
  perturb.fun = makePerturbFun("tabular.featureless")
  expect_class(perturb.fun, c("tabular.featuresless", "RPerturbFunTabular", "RPerturbFun", "perturbFun"))

  data(iris)
  instance = iris[5,]
  perturbInstance = perturbate(perturb.fun, iris, arules::discretizeDF(iris), instance, c(5))
  testthat::expect_true(inherits(perturbInstance, c("data.frame")))
  testthat::expect_identical(instance[5], perturbInstance[5])
  testthat::expect_false(all(instance[-5] == perturbInstance[-5]))
})
