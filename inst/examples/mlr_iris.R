library(mlr)
library(randomForest)

data(iris)

train = iris

# our goal is to predict the species
task = makeClassifTask(data = train, target = "Species", id = "iris")

# setting up a learner
#lrn.rpart = makeLearner("classif.rpart")
lrn = makeLearner("classif.randomForest")

# train the learner on the training set
model = mlr::train(learner = lrn, task = task)

# Setting up a perturbation function. As we want explain a tabular instance (an observation in our dataset iris), we stick to a featureless tabular perturbation function
perturbator = makePerturbFun("tabular.featureless")

# Explain model with anchors
explainer = anchors(train, model, perturbator)

train = train[train[,"Species"]=="versicolor",]

explanations = explain(train[1:2,], explainer)

printExplanations(explainer, explanations)




