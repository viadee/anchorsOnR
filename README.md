[![Build Status](https://travis-ci.org/viadee/anchorsOnR.svg?branch=master)](https://travis-ci.org/viadee/anchorsOnR)
[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

# anchorsOnR  

This package implements the Anchors XAI algorithm as proposed by Marco Tulio Ribeiro (2018). The original paper *"Anchors: High-Precision Model-Agnostic Explanations"* can be found [*here*](https://homes.cs.washington.edu/~marcotcr/aaai18.pdf). It provides a short characterization of anchors, which reads as follows: 

> An anchor explanation is a rule that sufficiently “anchors” the prediction locally – such that changes to the rest of the feature values of the instance do not matter. In other words, for instances on which the anchor holds, the prediction is (almost) always the same.

> The anchor method is able to explain any black box classifier, with two or more classes. All we require is that the classifier implements a function that takes [a data instance] and outputs [an integer] prediction.

Thus, anchors are highly precise explanations in form of human readable IF-THEN rules, that describe which values caused the model's outcome for one specific instance. They prodive a clear coverage, i.e., state exactly to which other instances the apply. 

This R package interfaces the [*anchorJ Java Implementation*](https://github.com/viadee/javaAnchorExplainer).

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

### Prerequisites

The R package requires a Java SE Runtime Environment (JRE) with [Java version 8](https://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html) or higher.

If you want to fiddle around with the anchorsOnR source code, make sure to have the `devtools` R-package installed.

```{r}
install.packages("devtools")
```

### Installing anchorsOnR

Now, install the *anchors* package directly from github as follows:
```{r}
devtools::install_github("viadee/anchorsOnR")
```
The following dependencies are required to use this package (unmodified, distributed and maintained by their respective authors through the established channels such as CRAN): 
* checkmate (BSD 3 clause)
* jsonlite (MIT)
* BBmisc (BSD 3 clause)
* uuid (MIT)
* magrittr (MIT)

### Using the Algorithm

The anchors API was designed in the style of the [lime R package](https://github.com/thomasp85/lime). The best way to illustrate the process of model-agnostic explanation in *anchors* is by example. Assume we aim to understand predictions made on the iris dataset. 

#### Obtaining a Model

Towards that goal, we first train an *mlr* learner as a black box model to explain

```{r}
library(anchors)
library(mlr)

data(iris)

# our goal is to predict the species
task = makeClassifTask(data = iris, target = "Species", id = "iris")
# setting up a learner
lrn = makeLearner("classif.rpart")
# train the learner on the training set
model = train(learner = lrn, task = task)
```

The created decision tree can be used to compare and validate the algorithm's results, since it can be easily visualized (as the approach is *model agnostic* any model could be explained, including such that are not inherently visualizable).

![Iris decision tree visualized](irisDecisionTree.png)

#### Calling anchorsOnR

Having created a model whose behavior is to be explained, we can obtain the explanations by first creating an *explainer* and using it on a specific instance (or multiple instances):
```{r}
explainer = anchors(iris, model, target = "Species")

explanations = explain(iris[100,], explainer)
```
The explain function spins up and eventually closes a background JVM in which the anchor server is tasked with determining the anchors in your dataset.

The explanation can be printed and looks similar to the following output:
```{r}
printExplanations(explainer, explanations)

# ====Explained Instance 100 ====
# Sepal.Length = 5.7
# Sepal.Width = 2.8
# Petal.Length = 4.1
# Petal.Width = 1.3
# WITH LABEL  = 'versicolor'
# ====Result====
# IF Petal.Length = 4.1 (ADDED PRECISION: 0.1736, ADDED COVERAGE: -0.26) AND
# Petal.Width = 1.3 (ADDED PRECISION: 0.8263, ADDED COVERAGE: -0.458)
# THEN PREDICT 'versicolor'
# WITH PRECISION 1 AND COVERAGE 0.282
```
Here it is clear, why this approach is called Anchors. The result is a rule, that describes the decision making of a machine learning model anchored around a particular instance of interest, but generalized as far as possible. 

We can check the result with the visualized decision tree and see that the anchor in fact explains the model locally. 

#### Discretization

The previous example shows one of anchors' disadvantages. Rules get very specific for numeric values. Discretization may help to group multiple values into one class that gets used as a proxy feature.

We can simply define the cut points for each feature and pass it to anchors:
```{r}
bins = list()
bins[[1]] = list(cuts = c(4.3, 5.4, 6.3, 7.9))
bins[[2]] = list(cuts = c(2.0, 2.9, 3.2, 4.4))
bins[[3]] = list(cuts = c(1, 2.6333, 4.9, 6.9))
bins[[4]] = list(cuts = c(0.1, 0.8666, 1.6, 2.5))

explainer = anchors(iris, model, target = "Species", bins = bins)

explanations = explain(iris[100,], explainer)
```

The output looks different now and is more easy to interpret:
```{r}
printExplanations(explainer, explanations)

# ====Result====
# IF Petal.Length IN [2.6333,4.9) (ADDED PRECISION: 0.1676, ADDED COVERAGE: -0.245) AND
# Petal.Width IN [0.8666,1.6) (ADDED PRECISION: 0.8323, ADDED COVERAGE: -0.518)
# THEN PREDICT 'versicolor'
# WITH PRECISION 1 AND COVERAGE 0.237
```


## Authors

* **Thorben Hellweg** [thllwg](https://github.com/thllwg)
* **Tobias Goerke** [TobiasGoerke](https://github.com/TobiasGoerke)
* **Magdalena Lang** [MagdalenaLang1](https://github.com/MagdalenaLang1)
* **Ronja Köhling**

## License

BSD 3-Clause License

