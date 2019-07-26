library(mlr)
library(mlbench)

data(PimaIndiansDiabetes)
diabetes = PimaIndiansDiabetes

# splitting the iris dataset into test and train data
smp_size <- floor(0.75 * nrow (diabetes))
set.seed(123)
train_ind <- sample(seq_len(nrow(diabetes)), size = smp_size)
train <- diabetes[train_ind, ]
test <- diabetes[-train_ind, ]

# our goal is to predict the species
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
discDiabetes = discretizeDF(discDiabetes)
discDiabetes[,"insulin"] = discretize(diabetes[,"insulin"], breaks = 2 )


# Explain model with anchors
explainer = anchors(train, model, perturbator, discX = discDiabetes)

explanations = explain(test[1,], explainer)

printExplanations(explainer, explanations)

plotExplanations(explanations)

