train <- read.csv(file="./inst/examples/ExampleTitanic/train.csv")
train <- train[, -c(1,4,9,11)]
test <- read.csv(file="./inst/examples/ExampleTitanic/test.csv")
test <-  test[, -c(1,3,8,10)]
#### MLR model ####

task = makeClassifTask(data = train, target = "Survived", id = "titanic")

# setting up a learner
lrn.rpart = makeLearner("classif.rpart")

# train the learner on the training set
mod = train(learner = lrn.rpart, task = task)
mod

# testing the model on the validation set
# pred = predict(object = mod, newdata = test[, -c(1,3,8,10)])
pred = predict(object = mod, newdata = train[760, ])
pred

instance = pred$data[1,]
instance = train[760,]

# discretize dataset
trainDisc = discretizeDF(train, methods=list(
  Pclass = list(method = "interval", breaks = 3),
  Survived = list(method = "interval", breaks = 2),
  Sex = list(method = "interval", breaks = 2),
  Age = list(method = "interval", breaks = 10),
  SibSp = list(method = "interval", breaks = 2),
  Parch = list(method = "interval", breaks = 3),
  Fare = list(method = "interval", breaks = 10),
  Embarked = list(method = "interval", breaks = 4)
))
instanceDisc = trainDisc[rownames(trainDisc) == rownames(instance),]
#### Anchor ####
# initialise anchors
ctrl = initAnchors(ip = "localhost", port = 6666, startAnchors = TRUE)


# Setting up a perturbation function. As we want explain a tabular instance (an observation in our dataset iris), we stick to a featureless tabular perturbation function
perturbator = makePerturbFun("tabular.featureless")
ctrl = registerAnchorsComponent(ctrl, "perturbate", perturbate, perturbator)

# explain instance
ctrl = registerAnchorsComponent(ctrl, "predict", predict, mod)
ctrl = registerAnchorsComponent(ctrl, "performance", performance, list(acc))
rules = explain(ctrl, train, trainDisc, labelIdx = 1, instanceIdx = as.integer(rownames(instance)))
# print the result in human readible form
printLocalExplanation(anchorResult = rules, instance = instance, dataset = train, datasetDisc = trainDisc, labelIdx = instanceInd)


# Result Anchors java for 759 in Java / row 760 in training dataset R
# Sex='female'
# Age='33.0'
# SibSp='0'
# Parch='0'
# Ticket='110152'
# Fare='86.5'
# Cabin='true'
# Embarked='S'
# WITH LABEL Survived='1'
# ====Result====
#   IF Sex = 'female' {0.62,-0.64} AND
# Fare IN INCL RANGE [53.1,512.3292] {0.16,-0.25} AND
# Pclass = '1' {0.23,-0} AND
# SibSp = '0' {0.01,-0.04}
# THEN PREDICT 1
# WITH PRECISION 1.0 AND COVERAGE 0.05

# ====Explained Instance====
#   Survived = '1'
# Sex = '1'
# Age = '33'
# SibSp = '0'
# Parch = '0'
# Fare = '86.5'
# Embarked = '4'
# WITH LABEL Pclass = '1'
# ====Result====
#   IF Age IN INLC RANGE [32.3,40.2) {0.0149253731343284,##.## Coverage} AND
#     SibSp IN INLC RANGE [0,4) {0,##.## Coverage} AND
#       Fare IN INLC RANGE [51.2,102) {-0.00163633658947786,##.## Coverage} AND
#         Survived IN INLC RANGE [0.5,1] {0.00163633658947786,##.## Coverage} AND
#           Parch IN INLC RANGE [0,2) {-0.00163633658947786,##.## Coverage} AND
#             Sex IN INLC RANGE female {0.00163633658947786,##.## Coverage} AND
#               Pclass IN INLC RANGE [1,1.67) {0.985074626865672,##.## Coverage}
#                 THEN PREDICT '1'
#                 WITH PRECISION 1 AND COVERAGE ##.## Coverage
