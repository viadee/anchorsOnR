# Downloaded from https://github.com/christophM/interpretable-ml-book/blob/master/data/cervical.RData
load(paste0(getwd(), "/inst/examples/ExampleCancer/cervical.RData"))
instanceInd <- 2
library(lime)
library(mlr)

#### Preparation ####
# Preparing the dataset by converting the classes to type factor
cervical$Biopsy = as.factor(cervical$Biopsy)
smp_size <- floor(0.75 * nrow (cervical))
set.seed(123)
train_ind <- sample(seq_len(nrow(cervical)), size = smp_size)
train <- cervical[train_ind, ]
test <- cervical[-train_ind, ]

# discretize dataset
cervicalDisc = arules::discretizeDF(cervical, methods=list(
  Smokes = list(method = "interval", breaks = 2),
  Smokes..years. = list(method = "interval", breaks = 10),
  Hormonal.Contraceptives = list(method = "interval", breaks = 2),
  Hormonal.Contraceptives..years. = list(method = "interval", breaks = 10),
  IUD = list(method = "interval", breaks = 2),
  IUD..years. = list(method = "interval", breaks = 10),
  STDs = list(method = "interval", breaks = 2),
  STDs..number. = list(method = "interval", breaks = 4),
  STDs..Number.of.diagnosis = list(method = "interval", breaks = 3),
  STDs..Time.since.first.diagnosis = list(method = "interval", breaks = 5),
  STDs..Time.since.last.diagnosis = list(method = "interval", breaks = 5)
  ))
instanceDisc = irisDisc[rownames(irisDisc) == rownames(instance),]
#### mlr model ####

#defining the task
task = makeClassifTask(data = train, target = "Biopsy", id = "cervical")

# setting up a learner
lrn.rpart = makeLearner("classif.randomForest", predict.type = "prob")

# train the learner on the training set
mod = train(learner = lrn.rpart, task = task)
pred = predict(object = mod, newdata = test)

#### LIME ####
explainer <- lime(cervical[train_ind,], mod, bin_continuous = TRUE, quantile_bins = FALSE)
explanation <- lime::explain(cervical[c(instanceInd), ], explainer, n_labels = 2, n_features = 12)

lime_explanation <- plot_features(explanation, ncol = 1)

#### Anchors on R ####


# initialise anchors
ctrl = initAnchors(ip = "localhost", port = 6666, startAnchors = TRUE)

instance = pred$data[1,]
instance = cervical[rownames(cervical) == rownames(instance),]

# Setting up a perturbation function. As we want explain a tabular instance (an observation in our dataset iris), we stick to a featureless tabular perturbation function
perturbator = makePerturbFun("tabular.featureless")
ctrl = registerAnchorsComponent(ctrl, "perturbate", perturbate, perturbator)

# explain instance
ctrl = registerAnchorsComponent(ctrl, "predict", predict, mod)
ctrl = registerAnchorsComponent(ctrl, "performance", performance, list(acc))
rules = explain(ctrl, cervical, cervicalDisc, labelIdx = instanceInd, instanceIdx = as.integer(rownames(instance)))
# print the result in human readible form
printLocalExplanation(anchorResult = rules, instance = instance, dataset = cervical, datasetDisc = cervicalDisc, labelIdx = instanceInd)

shutdown(ctrl)
