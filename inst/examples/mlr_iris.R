library(mlr)

# splitting the iris dataset into test and train data
smp_size <- floor(0.975 * nrow (iris))
set.seed(123)
train_ind <- sample(seq_len(nrow(iris)), size = smp_size)
train <- iris[train_ind, ]
test <- iris[-train_ind, ]

# our goal is to predict the species
task = makeClassifTask(data = train, target = "Species", id = "iris")

# setting up a learner
lrn.rpart = makeLearner("classif.rpart")

# train the learner on the training set
model = mlr::train(learner = lrn.rpart, task = task)

# Setting up a perturbation function. As we want explain a tabular instance (an observation in our dataset iris), we stick to a featureless tabular perturbation function
perturbator = makePerturbFun("tabular.featureless")

# Explain model with anchors
explainer = anchors(train, model, perturbator)

explanations = explain(test[1,], explainer)

printExplanations(explainer, explanations)

plotExplanations(explanations)

