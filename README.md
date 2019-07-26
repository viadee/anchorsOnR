[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

# anchorsOnR 

This project provides an R interface to the java implementation of the Anchors explanation algorithm for machine learning models. It enables High-Precision Model-Agnostic Explanations in a human readable form. 

The initial proposal "Anchors: High-Precision Model-Agnostic Explanations" by Marco Tulio Ribeiro (2018) can be found
[*here*](https://homes.cs.washington.edu/~marcotcr/aaai18.pdf). The java implementation of the anchors algorithm, which is interfaced by this package is provided [*here*](https://github.com/viadee/javaAnchorExplainer):

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

The R package requires a Java SE Runtime Environment (JRE) with [Java version] 8](https://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html) or higher.

If you want to fiddle around with the anchorsOnR source code, make sure to have the `devtools` package installed.

```{r}
install.packages("devtools")
```

### Installing

Now, install the *anchors* package directly from github as follows:
```{r}
devtools::install_github("viadee/anchorsOnR")
```
Following dependencies are required and loaded in the package: 
* checkmate
* rjson
* jsonlite
* BBmisc
* uuid
* arules
* ggplot2

### Using the Algorithm
The anchors API resembles the API of the R package [lime](https://github.com/thomasp85/lime).
The best way to illustrate the process of model-agnostic explanation in *anchors* is by example. Assume we aim to understand predictions made on the iris dataset. Towards that goal, we first train an *mlr* learner:

```{r}
library(mlr)
library(anchors)

# we are training a model on the iris data set
data(iris)
train = iris

# our goal is to predict the species
task = makeClassifTask(data = train, target = "Species", id = "iris")

# setting up a learner
lrn.rpart = makeLearner("classif.rpart")

# train the learner on the training set
model = mlr::train(learner = lrn.rpart, task = task)
```
In anchors, explanations are derived by feature value perturbation (i.e. chaning model feature values).
As we want explain a tabular instance (an observation in our dataset iris), we choose the default featureless tabular perturbation function.
```{r}
perturbator = makePerturbFun("tabular.featureless")
```
Finally, we create an anchors explainer and explain the model with it:
```{r}
explainer = anchors(train, model, perturbator)

explanations = explain(train[1,], explainer)
```
The explain function starts and shuts down a background JVM in which the anchor server is tasked with determining the anchors in your dataset.
After the explanations are derived from the model, one can visualize them in text form or as a heatmap-like graph.
```{r}
printExplanations(explainer, explanations)
#> ====Explained Instance  109 ====
#> Sepal.Length = 6.7
#> Sepal.Width = 2.5
#> Petal.Length = 5.8
#> Petal.Width = 1.8
#> WITH LABEL Species = '3'
#> ====Result====
#> IF Sepal.Length IN INLC RANGE [6.2,7.9] (ADDED PRECISION: 0.985074626865672) AND
#> Sepal.Width IN INLC RANGE [2.9,3.2) (ADDED PRECISION: 0) AND
#> Petal.Length IN INLC RANGE [4.8,6.9] (ADDED PRECISION: 0) AND
#> Petal.Width IN INLC RANGE [1.5,2.5] (ADDED PRECISION: 0.0149253731343284)
#> THEN PREDICT '3'
#> WITH PRECISION 1
```

```{r}
plotExplanations(explanations)
```
![Explanation Heatmap](https://raw.githubusercontent.com/viadee/anchorsOnR/master/docs/images/plotExplanations.png)


## Authors

* **Thorben Hellweg** [thllwg](https://github.com/thllwg)

## License

BSD 3-Clause License

