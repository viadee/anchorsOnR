library(mlr)

data("iris")
dim(iris)
head(iris)
str(iris)
summary(iris)

# preparing the species column to be our prediction target
iris$Species = as.factor(iris$Species)

# splitting the iris dataset into test and train data
smp_size <- floor(0.75 * nrow (iris))
set.seed(123)
train_ind <- sample(seq_len(nrow(iris)), size = smp_size)
train <- iris[train_ind, ]
test <- iris[-train_ind, ]

# our goal is to predict the species
task = makeClassifTask(data = train, target = "Species", id = "iris")

# setting up a learner
lrn.h2o = makeLearner("classif.h2o.gbm")

# train the learner on the training set
mod = train(learner = lrn.h2o, task = task)
mod

# testing the model on the validation set
pred = predict(object = mod, newdata = test)
pred

# extract one instance to explain
instance = pred$data[1,]
instance = iris[rownames(iris) == rownames(instance),]

# discretize dataset
irisDisc = discretizeDF(iris)
instanceDisc = irisDisc[rownames(irisDisc) == rownames(instance),]

# ###################### ANCHORS STARTS HERE ######################

# initialise anchors
ctrl = initAnchors(ip = "localhost", port = 6666, startAnchors = TRUE)

# Setting up a perturbation function. As we want explain a tabular instance (an observation in our dataset iris), we stick to a featureless tabular perturbation function
perturbator = makePerturbFun("tabular.featureless")
ctrl = registerAnchorsComponent(ctrl, "perturbate", perturbate, perturbator)

# explain instance
ctrl = registerAnchorsComponent(ctrl, "predict", predict, mod)
ctrl = registerAnchorsComponent(ctrl, "performance", performance, list(acc))
rules = explain(ctrl, iris, irisDisc, labelIdx = 5, instanceIdx =  as.integer(rownames(instance)))
# print the result in human readible form
printLocalExplanation(anchorResult = rules, instance = instance, dataset = iris, datasetDisc = irisDisc, labelIdx = 5)

shutdown(ctrl)


