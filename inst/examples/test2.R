library(mlr)

data("mtcars")
dim(mtcars)
head(mtcars)
str(mtcars)
summary(mtcars)

mtcars$vs = as.factor(mtcars$vs)
smp_size <- floor(0.75 * nrow (mtcars))
set.seed(123)
train_ind <- sample(seq_len(nrow(mtcars)), size = smp_size)

train <- mtcars[train_ind, ]
test <- mtcars[-train_ind, ]

task = makeClassifTask(data = train, target = "vs", id = "mtcars")

lrn.rpart = makeLearner("classif.rpart")

mod = train(learner = lrn.rpart, task = task)
mod

pred = predict(object = mod, newdata = test)
pred

# extract one instance to explain
instance = pred$data[1,]
instance = mtcars[rownames(mtcars) == rownames(instance),]

#discretize dataset
mtcarsDisc = mtcars
mtcarsDisc
mtcarsDisc$mpg = discretize(mtcars$mpg, method = "interval", breaks = 8)


# extract one instance to explain
instance = pred$data[1,]
instance = iris[rownames(iris) == rownames(instance),]

# discretize dataset
irisDisc = discretizeDF(iris)
instanceDisc = irisDisc[rownames(irisDisc) == rownames(instance),]


# initialise anchors
ctrl = initAnchors(ip = "localhost", port = 6666, startAnchors = TRUE)

# Setting up a perturbation function. As we want explain a tabular instance (an observation in our dataset iris), we stick to a featureless tabular perturbation function
perturbator = makePerturbFun("tabular.featureless")
ctrl = registerAnchorsComponent(ctrl, "perturbate", perturbate, perturbator)

# explain instance
ctrl = registerAnchorsComponent(ctrl, "predict", predict, mod)
ctrl = registerAnchorsComponent(ctrl, "performance", performance, list(acc))
rules = anchors.explain(ctrl, iris, irisDisc, labelIdx = 5, instanceIdx =  as.integer(rownames(instance)))
# print the result in human readible form
printLocalExplanation(anchorResult = rules, instance = instance, dataset = iris, datasetDisc = irisDisc, labelIdx = 5)


