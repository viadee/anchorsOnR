#' Create a model explanation function based on training data
#'
#' This is the main function of the `anchors` package. It is a factory function
#' that returns a new function that can be used to explain the predictions made
#' by black box models. This is a generic with methods for the different data
#' types supported by lime.
#'
#' @param x The training data used for training the model that should be
#' explained.
#'
#' @param model The model whose output should be explained
#'
#' @param perturbator The pertubator used to perturbate the instances to be explained.
#'
#' @param discX A discretized training dataset. If none is provided, autodiscretisation
#' is applied to \code{x}.
#'
#' @param target Target column in x. Only provide this if model is not of class "WrappedModel".
#'
#' @param ... Arguments passed on to methods
#'
#' @return Return an explainer which can be used together with [explain()] to
#' explain model predictions.
#'
#' @name anchors
#' @export
anchors <- function(x, model, perturbator, discX = NULL, target = NULL, ...) {
  UseMethod('anchors', x)
}

