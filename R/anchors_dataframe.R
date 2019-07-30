#' @rdname anchors
#' @name anchors
#' @param bin_continuous Should continuous variables be binned when making the explanation
#' @param n_bins The number of bins for continuous variables if `bin_continuous = TRUE`
#' @param quantile_bins Should the bins be based on `n_bins` quantiles or spread evenly over the range of the training data
#' @param use_density If `bin_continuous = FALSE` should continuous data be sampled using a kernel density estimation. If not, continuous features are expected to follow a normal distribution.
#' @importFrom stats predict sd quantile density
#' @export
#'
anchors.data.frame <- function(x, model, perturbator = NULL, discX = NULL, target = NULL, preprocess = NULL, bin_continuous = TRUE, n_bins = 4, quantile_bins = TRUE, use_density = TRUE, ...) {
  if (is.null(preprocess)) preprocess <- function(x) x
  checkmate::assert_function(preprocess)
  if (is.null(perturbator)) perturbator <- makePerturbFun("tabular.featureless")
  explainer <- c(as.list(environment()), list(...))
  explainer$trainingsData <- x
  if (!inherits(model, "WrappedModel")){
    explainer$target = target
  } else {
    explainer$target = which(colnames(x)==model$task.desc$target)
  }
  if (is.null(discX)){
    # test auto discretize
    discX = try({
      arules::discretizeDF(x[,-explainer$target])
    }, silent = TRUE)
    if (inherits(discX, "try-error")){
      BBmisc::stopf("Unsupervised discretization not available for the passed dataset. Please discretize the dataset and pass it along as the funtions argument.")
    }
  }
  explainer$discTrainingsData <- discX
  explainer$perturbator <- perturbator
  explainer$feature_type <- setNames(sapply(x, function(f) {
    if (is.integer(f)) {
      if (length(unique(f)) == 1) 'constant' else 'integer'
    } else if (is.numeric(f)) {
      if (length(unique(f)) == 1) 'constant' else 'numeric'
    } else if (is.character(f)) {
      'character'
    } else if (is.factor(f)) {
      'factor'
    } else if (is.logical(f)) {
      'logical'
    } else if (inherits(f, 'Date') || inherits(f, 'POSIXt')) {
      'date_time'
    } else {
      stop('Unknown feature type', call. = FALSE)
    }
  }), names(x))
  if (any(explainer$feature_type == 'constant')) {
    warning('Data contains numeric columns with zero variance', call. = FALSE)
  }
  explainer$bin_cuts <- setNames(lapply(seq_along(x), function(i) {
    if (explainer$feature_type[i] %in% c('numeric', 'integer')) {
      if (quantile_bins) {
        bins <- quantile(x[[i]], seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)
        bins <- bins[!duplicated(bins)]
        if (length(bins) < 3) {
          warning(names(x)[i], ' does not contain enough variance to use quantile binning.
                  Using standard binning instead.', call. = FALSE)
          d_range <- range(x[[i]], na.rm = TRUE)
          bins <- seq(d_range[1], d_range[2], length.out = n_bins + 1)
        }
        bins
      } else {
        d_range <- range(x[[i]], na.rm = TRUE)
        seq(d_range[1], d_range[2], length.out = n_bins + 1)
      }
    }
  }), names(x))
  explainer$feature_distribution <- setNames(lapply(seq_along(x), function(i) {
    switch(
      explainer$feature_type[i],
      integer = ,
      numeric = if (bin_continuous) {
        table(cut(x[[i]], unique(explainer$bin_cuts[[i]]), labels = FALSE, include.lowest = TRUE))/nrow(x)
      } else if (use_density) {
        density(x[[i]])
      } else {
        c(mean = mean(x[[i]], na.rm = TRUE), sd = sd(x[[i]], na.rm = TRUE))
      },
      character = ,
      logical = ,
      factor = table(x[[i]])/nrow(x),
      NA
    )
  }), names(x))
  structure(explainer, class = c('data_frame_explainer', 'explainer', 'list'))
}
