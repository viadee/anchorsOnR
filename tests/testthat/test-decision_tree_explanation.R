expect_all_true <- function(x) {
  sapply(x, function(y) expect_true(y))
}

test_that("simple decision trees get explained correctly",{
  # TODO have this test run with precision = 1
  # Then, the outcome's structure should exactly equal the decision tree

  library(mlr)
  library(randomForest)

  set.seed(123)

  data(iris)
  task = makeClassifTask(data = iris, target = "Species", id = "iris")
  lrn = makeLearner("classif.rpart")
  model = mlr::train(learner = lrn, task = task)

  # rpart.plot::rpart.plot(getLearnerModel(model))
  # 1) root 150 100 setosa (0.33333333 0.33333333 0.33333333)
  # 2) Petal.Length< 2.45 50   0 setosa (1.00000000 0.00000000 0.00000000) *
  # 3) Petal.Length>=2.45 100  50 versicolor (0.00000000 0.50000000 0.50000000)
  # 6) Petal.Width< 1.75 54   5 versicolor (0.00000000 0.90740741 0.09259259) *
  # 7) Petal.Width>=1.75 46   1 virginica (0.00000000 0.02173913 0.97826087) *

  perturbator = makePerturbFun("tabular.featureless")

  discIris = iris
  discIris = arules::discretizeDF(discIris)
  # Prepare explainer to explain model with anchors
  explainer = anchors(iris, model, perturbator, discX=discIris)

  # Produce explanations for selected instances
  setosa = iris[iris[,"Species"]=="setosa",][1,]
  versicolor = iris[iris[,"Species"]=="versicolor",][1,]
  virginica = iris[iris[,"Species"]=="virginica",][1,]
  toExplain = rbind(versicolor, setosa, virginica)

  # Check the model classifies instances correctly, just to be sure
  model_pred <- predict(model, newdata = toExplain)


  # Call anchors
  explanations = explain(toExplain, explainer)

  setosa_explanation <- explanations[explanations$case == rownames(setosa),]
  versicolor_explanation <- explanations[explanations$case == rownames(versicolor),]
  virginica_explanation <- explanations[explanations$case == rownames(virginica),]

  expect_equal(nrow(setosa_explanation), 1)
  # TODO expect_equal(nrow(versicolor_explanation), 2)
  expect_equal(nrow(virginica_explanation), 2)

  expect_true(setosa_explanation$feature == "Petal.Length")
  # TODO expect_all_true(versicolor_explanation$feature %in% c("Petal.Width", "Petal.Length"))
  expect_all_true(virginica_explanation$feature %in% c("Petal.Width", "Petal.Length"))

  #printExplanations(explainer, explanations)
})
