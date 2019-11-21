#' Represents the configuration for an anchors tabular scenario.
#'
#' For this, a model and a perturbation space is required.
#' To construct the perturbation space, a training set is used.
#' Also, discretization can be defined to increase explanation quality.
#'
#' @rdname anchors
#' @name anchors
#' @export
anchors.data.frame <- function(x, model, perturbator = NULL, bins = NULL,
                               p = 0.5, coverage_perturbation_count = 1000,
                               target = NULL, maxAnchors = NULL, beams = 2L,
                               delta = 0.1, epsilon = 0.1, tau = 0.9,
                               tauDiscrepancy = 0.05, initSamples = 10L,
                               allowSuboptimalSteps = TRUE, batchSize = 100L,
                               verbose = 0,
                               ...) {

  explainer <- c(as.list(environment()), list(...))

  # Check target
  if (!is.null(target)) {
    explainer$target = target
  } else if (inherits(model, "WrappedModel")) {
    explainer$target = model$task.desc$target
  }

  if (!is.null(explainer$target) && !(explainer$target %in% names(x))) {
    BBmisc::stopf("Cannot access specified target. If no mlr model is to
                    be explained, please provide the column name of the
                    target variable.")
  }

  # Set bins/discretization
  predictorCount <- length(explainer$x)
  if (!is.null(explainer$target))
    predictorCount = predictorCount - 1
  if (is.null(bins)) {
    bins <- create.empty.discretization(predictorCount)
  }
  explainer$bins <- validate.bins(bins, predictorCount)

  # Create discretized DF with removed target column
  if (!is.null(explainer$target)) {
    targetIndex <- which(colnames(x) == explainer$target)
    if (length(targetIndex) != 1 && targetIndex < 0)
      stop("Could not find unambiguous target column")

    # Create discretized DF
    explainer$discretizedDF <- discretize.data.frame(explainer$x[, -targetIndex],
                                                     explainer$bins)
  } else {
    explainer$discretizedDF <- discretize.data.frame(explainer$x[,],
                                                     explainer$bins)
  }


  # Check perturbator
  if (is.null(perturbator))
    perturbator <- perturbTabular
  explainer$perturbator <- perturbator

  explainer$p = p
  explainer$coverage_perturbation_count = coverage_perturbation_count

  if (is.null(maxAnchors))
    maxAnchors = ncol(explainer$x)-1 # target cannot be anchor
  if (maxAnchors>=ncol(explainer$x))
    BBmisc::stopf("You cannot have more anchors than features.")
  explainer$maxAnchors = maxAnchors
  explainer$beams = beams
  explainer$delta = delta
  explainer$epsilon = epsilon
  explainer$tau = tau
  explainer$tauDiscrepancy = tauDiscrepancy
  explainer$initSamples = initSamples
  explainer$allowSuboptimalSteps = allowSuboptimalSteps
  explainer$batchSize = batchSize
  #explainer$emptyRuleEvaluations = emptyRuleEvaluations
  explainer$verbose = verbose

  structure(explainer, class = c('data_frame_explainer', 'explainer', 'list'))
}
