library("anchors")
library(mlr)
library(randomForest)

data(iris)

# our goal is to predict the species
task = makeClassifTask(data = iris, target = "Species", id = "iris")

# setting up a learner
#lrn.rpart = makeLearner("classif.rpart")
lrn = makeLearner("classif.randomForest")

# train the learner on the training set
model = mlr::train(learner = lrn, task = task)

# Setting up a perturbation function. As we want explain a tabular instance (an observation in our dataset iris), we stick to a featureless tabular perturbation function
perturbator = makePerturbFun("tabular.featureless")

discIris = iris

# Prepare bins: a list of bins per column, the nth-entry provides the bins of the nth-column
# Per bin: define whether numeric (for now: only works with numeric), the cuts of the bins, and whether the right or the left border of a bin is included
## TODO: for usability, should add helper function to build the bins
bins = list()
r = sapply(1:(ncol(iris) - 1), function(x) {
  bin <<- list()
  bin$numeric <<- T
  cuts = arules::discretize(discIris[, x], onlycuts = T)
  bin$cuts <<- cuts[2:(length(cuts) - 1)]
  bin$right <<- F
  bins[[x]] <<- bin
})

# Prepare explainer to explain model with anchors
explainer = anchors(iris, model, perturbator, bins = bins)

# Produce explanations for selected instances
toExplain = iris[iris[, "Species"] == "versicolor", ]
explanations = explain(toExplain[1:2, ], explainer)

printExplanations(explainer, explanations)
