library(anchors)
library(mlr)
library(mlbench)

data(PimaIndiansDiabetes)
diabetes = PimaIndiansDiabetes

# Manipulate the dataset to have at least one categorical predictor
diabetes$pregnant <- as.factor(ifelse(diabetes$pregnant == 0, "none",
                                         ifelse(diabetes$pregnant == 1, "one",
                                                ifelse(diabetes$pregnant < 4, "few", "many"))))

# our goal is to predict whether individuum has diabetes
task = makeClassifTask(data = diabetes, target = "diabetes", id = "diabetes")

# setting up a learner
lrn.rpart = makeLearner("classif.rpart")

# train the learner on the training set
model = mlr::train(learner = lrn.rpart, task = task)

# Visualize
rpart.plot::rpart.plot(getLearnerModel(model))

# Prepare bins: a list of bins per column, the nth-entry provides the bins of the nth-column
# Per bin: define whether numeric (for now: only works with numeric), the cuts of the bins, and whether the right or the left border of a bin is included
bins=list()
r=sapply(1:(ncol(diabetes)-1), function(x){
  bin<<-list()
  if (x==1) {
    bin$doDiscretize<<-F
    bin$numeric<<-F
    bin$classes <- list(c("none"), c("one", "few"), c("manyy"))
  }
  else if(x==5){
    # for this feature, only 2 breaks work
    bin$numeric<<-T
    cuts = arules::discretize(diabetes[,x], onlycuts=T, breaks = 2)
    bin$cuts<<-cuts[2:(length(cuts)-1)]
  }else{
    bin$numeric<<-T
    cuts = arules::discretize(diabetes[,x], onlycuts=T)
    bin$cuts<<-cuts[2:(length(cuts)-1)]
  }

  bin$right<<-F
  bins[[x]]<<-bin
})

# Explain model with anchors
explainer = anchors(diabetes, model, bins = bins)

toExplain <- diabetes[1:2,]
explanations = explain(toExplain, explainer)

printExplanations(explainer, explanations)


