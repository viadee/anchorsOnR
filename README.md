[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

# anchorsOnR 

This project provides an R interface to the java implementation of the Anchors explanation algorithm for machine learning models. It enables High-Precision Model-Agnostic Explanations in a human readable form. 

The initial proposal "Anchors: High-Precision Model-Agnostic Explanations" by Marco Tulio Ribeiro (2018) can be found
[*here*](https://homes.cs.washington.edu/~marcotcr/aaai18.pdf). The java implementation of the anchors algorithm, which is interfaced by this package is provided [*here*](https://github.com/viadee/javaAnchorExplainer).

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

The R package requires a Java SE Runtime Environment (JRE) with [Java version 8](https://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html) or higher.

If you want to fiddle around with the anchorsOnR source code, make sure to have the `devtools` R-package installed.

```{r}
install.packages("devtools")
```

### Installing

Now, install the *anchors* package directly from github as follows:
```{r}
devtools::install_github("viadee/anchorsOnR")
```
The following dependencies are required to use this package (unmodified, distributed and maintained by their respective authors through the established channels such as CRAN): 
* checkmate (BSD 3 clause)
* rjson (GPL-2)
* jsonlite (MIT)
* BBmisc (BSD 3 clause)
* uuid (MIT)
* arules (GPL-3)
* magrittr (MIT)

### Using the Algorithm
The anchors API resembles the API of the R package [lime](https://github.com/thomasp85/lime).
The best way to illustrate the process of model-agnostic explanation in *anchors* is by example. Assume we aim to understand predictions made on the iris dataset. Towards that goal, we first train an *mlr* learner as a black box model to explain:

```{r}
library(mlr)
library(anchors)

data(iris)
train = iris

# our goal is to predict the species
task = makeClassifTask(data = train, target = "Species", id = "iris")

# set up a learner
lrn.rpart = makeLearner("classif.rpart")

# train the learner on the training set
model = train(learner = lrn.rpart, task = task)
```
In anchors, explanations are derived by feature value perturbation (i.e. systematically changing model feature values to see which changes are necessary to flip the model's prediction). 
As we want explain a tabular instance (an observation in our dataset iris), we choose the default tabular perturbation function.

```{r}
perturbator = makePerturbFun("tabular.featureless")
```
Finally, we create an anchors explainer and apply it to a case to be explained:
```{r}
explainer = anchors(train, model, perturbator)

explanations = explain(train[52,], explainer)
```
The explain function starts and shuts down a background JVM in which the anchor server is tasked with determining the anchors in your dataset.
After the explanations are derived from the model, one can visualize them in rule form or as a heatmap-like graph.
```{r}
printExplanations(explainer, explanations)
#> ====Explained Instance  52 ====
#> Sepal.Length = 6.4
#> Sepal.Width = 3.2
#> Petal.Length = 4.5
#> Petal.Width = 1.5
#> WITH LABEL Species = 'versicolor'
#> ====Result====
#> IF Petal.Length IN INLC RANGE [2.63,4.9) (ADDED PRECISION: 0.810945273631841, ADDED COVERAGE: -0.411)
#> THEN PREDICT '2'
#> WITH PRECISION 0.810945273631841 AND COVERAGE 0.589
```
Here it is clear, why the method is called Anchors. The result is a rule, that describes the decision making of a machine learning model anchored around a particular instance of interest, but generalized as far as possible. Looking around from our chosen case it becomes clear, that in this machine learning model similar cases are correctly classified as class '2' nearly exclusively using the Petal.Length measurement. Given the rule it is also easy to calculate the number of cases covered and hence provide a relative coverage of the rule (which is a major improvement compared to LIME).

## Authors

* **Thorben Hellweg** [thllwg](https://github.com/thllwg)
* **Tobias Goerke**
* **Ronja Köhling**

## License

BSD 3-Clause License

