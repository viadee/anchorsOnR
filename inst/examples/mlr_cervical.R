library(mlr)

load(paste0(getwd(), "/inst/examples/ExampleCancer/cervical.RData"))

# our goal is to predict whether individuum has cancer
task = makeClassifTask(data = cervical, target = "Biopsy", id = "Biopsy")

# setting up a learner
lrn.rpart = makeLearner("classif.rpart")

# train the learner on the training set
model = mlr::train(learner = lrn.rpart, task = task)


# Setting up a perturbation function. As we want explain a tabular instance (an observation in our dataset iris), we stick to a featureless tabular perturbation function
perturbator = makePerturbFun("tabular.featureless")

# discretizing the dataset
## TODO: add better discretes
discCervical = cervical
discCervical[,"Smokes"] = NA
discCervical[,"Smokes..years."] = NA
discCervical[,"Hormonal.Contraceptives"] = NA
discCervical[,"Hormonal.Contraceptives..years."] = NA
discCervical[,"IUD"] = NA
discCervical[,"IUD..years."] = NA
discCervical[,"STDs"] = NA
discCervical[,"STDs..number."] = NA
discCervical[,"STDs..Number.of.diagnosis"] = NA
discCervical[,"STDs..Time.since.first.diagnosis"] = NA
discCervical[,"STDs..Time.since.last.diagnosis"] = NA
discCervical = arules::discretizeDF(discCervical)
discCervical[,"Smokes"] = arules::discretize(cervical[,"Smokes"], breaks =1 )
discCervical[,"Smokes..years."] = arules::discretize(cervical[,"Smokes..years."], breaks =1)
discCervical[,"Hormonal.Contraceptives"] = arules::discretize(cervical[,"Hormonal.Contraceptives"], breaks =1)
discCervical[,"Hormonal.Contraceptives..years."] = arules::discretize(cervical[,"Hormonal.Contraceptives..years."], breaks =2)
discCervical[,"IUD"]= arules::discretize(cervical[,"IUD"], breaks =1 )
discCervical[,"IUD..years."]= arules::discretize(cervical[,"IUD..years."], breaks =1)
discCervical[,"STDs"]= arules::discretize(cervical[,"STDs"], breaks =1)
discCervical[,"STDs..number."]= arules::discretize(cervical[,"STDs..number."], breaks =1)
discCervical[,"STDs..Number.of.diagnosis"]= arules::discretize(cervical[,"STDs..Number.of.diagnosis"], breaks =1)
discCervical[,"STDs..Time.since.first.diagnosis"]= arules::discretize(cervical[,"STDs..Time.since.first.diagnosis"], breaks =1)
discCervical[,"STDs..Time.since.last.diagnosis"]= arules::discretize(cervical[,"STDs..Time.since.last.diagnosis"], breaks =1)

# Explain model with anchors
explainer = anchors(cervical, model, perturbator, discX = discCervical)

explanations = explain(cervical[1:2,], explainer)

printExplanations(explainer, explanations)

