#' Explain model predictions
#'
#' Once an explainer has been created using the [anchors()] function it can be used
#' to explain the result of the model on new observations. The `explain()`
#' function takes new observation along with the explainer and returns a
#' data.frame with prediction explanations, one observation per row. The
#' returned explanations can then be visualised in a number of ways, e.g. with
#' [plot_features()].
#'
#' @param x New observations to explain, of the same format as used when
#' creating the explainer
#'
#' @param explainer An `explainer` object to use for explaining the observations
#'
#' @param labels The specific labels (classes) to explain in case the model is
#' a classifier.
#'
#'
#' @param ... Parameters passed on to the `predict_model()` method
#'
#' @return A data.frame encoding the explanations one row per explained
#' observation. The columns are:
#'
#' - `model_type`: The type of the model used for prediction.
#' - `case`: The case being explained (the rowname in `cases`).
#' - `model_r2`: The quality of the model used for the explanation
#' - `model_intercept`: The intercept of the model used for the explanation
#' - `model_prediction`: The prediction of the observation based on the model
#'   used for the explanation.
#' - `feature`: The feature used for the explanation
#' - `feature_value`: The value of the feature used
#' - `feature_weight`: The weight of the feature in the explanation
#' - `feature_desc`: A human readable description of the feature importance.
#' - `data`: Original data being explained
#' - `prediction`: The original prediction from the model
#'
#' Furthermore classification explanations will also contain:
#'
#' - `label`: The label being explained
#' - `label_prob`: The probability of `label` as predicted by `model`
#'
#' @examples
#' # Explaining a model based on tabular data
#' library(anchors)
#' library(mlr)
#' data(iris)
#' # our goal is to predict the species
#' task = makeClassifTask(data = iris, target = "Species", id = "iris")
#' # setting up a learner
#' lrn = makeLearner("classif.lda")
#' # train the learner on the training set
#' model = train(learner = lrn, task = task)
#' explainer = anchors(iris, model, target = "Species")
#' explanations = explain(iris[100,], explainer)
#' @export
#'
explain <- function(x, explainer, labels = NULL, ...) {
  UseMethod("explain", x)
}

model_type.explainer <- function(x) {
  model_type(x$model)
}

output_type <- function(x) {
  switch(
    model_type(x),
    classification = "prob",
    regression = "raw",
    stop(model_type(x), " models are not supported yet", call. = FALSE)
  )
}
