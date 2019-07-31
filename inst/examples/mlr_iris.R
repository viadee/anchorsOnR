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

discIris=iris
discIris = arules::discretizeDF(discIris)
# Prepare explainer to explain model with anchors
explainer = anchors(iris, model, perturbator, discX=discIris)

# Produce explanations for selected instances
toExplain = iris[iris[,"Species"]=="versicolor",]
explanations = explain(toExplain[1:2,], explainer)

printExplanations(explainer, explanations)


