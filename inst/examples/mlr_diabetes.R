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

# Prepare bins: a list of bins per column, the nth-entry provides the bins of the nth-column
# Per bin: define whether numeric (for now: only works with numeric), the cuts of the bins, and whether the right or the left border of a bin is included
discDiabetes = diabetes
bins=list()
r=sapply(1:(ncol(diabetes)-1), function(x){
  bin<<-list()
  bin$numeric<<-T
  if(x==5){
    # for this feature, only 2 breaks work
    cuts = arules::discretize(discDiabetes[,x], onlycuts=T, breaks = 2)
    bin$cuts<<-cuts[2:(length(cuts)-1)]
  }else{
    cuts = arules::discretize(discDiabetes[,x], onlycuts=T)
    bin$cuts<<-cuts[2:(length(cuts)-1)]
  }

  bin$right<<-F
  bins[[x]]<<-bin
})

# Explain model with anchors
explainer = anchors(diabetes, model, perturbator, bins = bins)

explanations = explain(diabetes[1:2,], explainer)

printExplanations(explainer, explanations)


