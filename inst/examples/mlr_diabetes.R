library(anchors)
library(mlr)
library(mlbench)

data(PimaIndiansDiabetes)
diabetes = PimaIndiansDiabetes

# our goal is to predict whether individuum has diabetes
task = makeClassifTask(data = diabetes, target = "diabetes", id = "diabetes")

# setting up a learner
lrn.rpart = makeLearner("classif.rpart")

# train the learner on the training set
model = mlr::train(learner = lrn.rpart, task = task)

# Setting up a perturbation function. As we want explain a tabular instance (an observation in our dataset iris), we stick to a featureless tabular perturbation function
perturbator = makePerturbFun("tabular.featureless")

# discretizing the dataset
discDiabetes = diabetes
discDiabetes[,5] = NA
discDiabetes = arules::discretizeDF(discDiabetes)
discDiabetes[,"insulin"] = arules::discretize(diabetes[,"insulin"], breaks = 2 )


# Explain model with anchors
explainer = anchors(diabetes, model, perturbator, discX = discDiabetes)

explanations = explain(diabetes[1:2,], explainer)

printExplanations(explainer, explanations)

plotExplanations(explanations, colnames(diabetes)[-ncol(diabetes)])

