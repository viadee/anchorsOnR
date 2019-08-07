#' Create a model explanation function based on training data
#'
#' This is the main function of the `anchors` package. It is a factory function
#' that returns a new function that can be used to explain the predictions made
#' by black box models. This is a generic with methods for the different data
#' types supported by anchors.
#'
#' @param x The training data used for training the model that should be
#' explained.
#'
#' @param model The model whose output should be explained
#'
#' @param perturbator Pertubator to perturbate the instances to be explained.
#'
#' @param target Target column in [x].
#' Provide this if model is not of class "WrappedModel".
#'
#' @param maxAnchors how many features can be maximal anchored?
#' @param beams size of the current candidates for beam search
#' @param delta The delta value describing the probability of identifying the best arm == confidence
#' @param epsilon The maximum tolerated error == tolerance
#' @param tau The desired precision an anchor needs to achieve. If no candidate achieves at least this precision, the one with the best precision will be returned
#' @param tauDiscrepancy Usually, it is practically infeasible to sample until the mean and the
#' upper/lower bounds simultaneously fall below or above the tau threshold.
#' Therefore, this variable may be introduced to control this discrepancy.
#' @param initSamples The number of evaluations sampled for each candidate before it gets evaluated by
#' the best arm identification algorithm. While theoretically, a guarantee that no
#' candidates get discarded due to too few samples is provided by delta, using this
#' argument has practical advantages.
#' @param allowSuboptimalSteps if set to false, candidates that are returned by the best arm identification get
#' removed when their precision is lower than their parent's
#'
#' @param ... Arguments passed on to methods
#'
#' @return Returns an explainer which can be used together with [explain()] to
#' explain model predictions.
#'
#' @name anchors
#' @export
anchors <- function(x, model, perturbator = NULL, bins = NULL,
                    p = 0.5, coverage_perturbation_count = 1000,
                    target = NULL, maxAnchors = NULL, beams = 2L,
                    delta = 0.1, epsilon = 0.1, tau = 0.9, tauDiscrepancy = 0.05,
                    initSamples = 10L, allowSuboptimalSteps = TRUE, batchSize = 100L, ...) {
  UseMethod('anchors', x)
}

