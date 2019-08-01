#' Represents the configuration for an anchors tabular scenario.
#'
#' For this, a model and a perturbation space is required.
#' To construct the perturbation space, a training set is used.
#' Also, discretization can be defined to increase explanation quality.
#'
#' @rdname anchors
#' @name anchors
#' @importFrom stats predict sd quantile density
#' @export
anchors.data.frame <-
  function(x,
           model,
           perturbator = NULL,
           bins = NULL,
           target = NULL,
           ...) {
    explainer <- c(as.list(environment()), list(...))

    # Set train data
    explainer$trainingsData <- x

    # Check target
    if (!is.null(target)) {
      explainer$target = target
    } else if (inherits(model, "WrappedModel")) {
      explainer$target = model$task.desc$target
    }
    if (!is.null(explainer$target) && !(explainer$target %in% names(x))) {
      BBmisc::stopf("Cannot access specified target. Pleaser either provide no target, a valid one or a WrappedModel.")
    }

    # Set bins/discretization
    predictorCount <- length(explainer$trainingsData)
    if (!is.null(explainer$target))
      predictorCount = predictorCount - 1
    if (is.null(bins)) {
      bins <- create.empty.discretization(predictorCount)
    }
    validate.bins(bins, predictorCount)
    explainer$bins <- bins

    if (is.null(perturbator))
      perturbator <- makePerturbFun("tabular.featureless")
    explainer$perturbator <- perturbator

    # # Feature type per column
    # explainer$feature_type <- setNames(sapply(x, function(f) {
    #   if (is.integer(f)) {
    #     if (length(unique(f)) == 1)
    #       'constant'
    #     else
    #       'integer'
    #   } else if (is.numeric(f)) {
    #     if (length(unique(f)) == 1)
    #       'constant'
    #     else
    #       'numeric'
    #   } else if (is.character(f)) {
    #     'character'
    #   } else if (is.factor(f)) {
    #     'factor'
    #   } else if (is.logical(f)) {
    #     'logical'
    #   } else if (inherits(f, 'Date') || inherits(f, 'POSIXt')) {
    #     'date_time'
    #   } else {
    #     stop('Unknown feature type', call. = FALSE)
    #   }
    # }), names(x))
    # if (any(explainer$feature_type == 'constant')) {
    #   warning('Data contains numeric columns with zero variance', call. = FALSE)
    # }

    structure(explainer,
              class = c('data_frame_explainer', 'explainer', 'list'))
  }
