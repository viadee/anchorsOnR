context("data.frame")
library(MASS)
library(mlr)

test_that("anchors explanation only produces at max one entry per case and feature", {
  # Split up the data set
  iris_train <- iris

  # Create Random Forest model on iris data
  task = makeClassifTask(data = iris, target = "Species", id = "iris")
  lrn = makeLearner("classif.lda")
  model = mlr::train(learner = lrn, task = task)


  # Create explanation function
  explainer <- anchors(iris_train, model)

  # Explain new observation. This should yield a data.frame with at max 4 rows, because it's one case, that has 4 features, one label.
  explanations <- explain(iris_train[1,], explainer)
  expect_lt(nrow(explanations),5)
})
