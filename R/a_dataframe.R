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
  #explainer = list(...)
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
          warning(names(x)[i], ' does not contain enough variance to use quantile binning. Using standard binning instead.', call. = FALSE)
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

#' @rdname explain
#' @name explain
#'
#' @importFrom gower gower_dist
#' @importFrom stats dist
#' @export
explain.data.frame <- function(x, explainer, labels = NULL, n_labels = NULL,
                               feature_select = 'auto', ...) {
  checkmate::assert_true(is.data_frame_explainer(explainer))
  m_type <- model_type(explainer)
  o_type <- output_type(explainer)
  if (m_type == 'regression') {
    if (!is.null(labels) || !is.null(n_labels)) {
      warning('"labels" and "n_labels" arguments are ignored when explaining regression models')
    }
    n_labels <- 1
    labels <- NULL
  }

  explainer$connection <- initAnchors()

  trainSet = explainer$trainingsData[,-explainer$target]
  trainSetDisc = explainer$discTrainingsData

  rules = list()

  explanations = data.frame(matrix(ncol = 12, nrow = 0))
  colnames(explanations) = c("model_type", "case", "label", "label_prob", "feature", "feature_value",
                             "feature_weight","added_coverage", "feature_desc", "data","prediction", "precision")

  cat("Explaining ",nrow(x)," observations. This may take a while."); cat("\n");
  for (i in 1:nrow(x)){
    cat("[Explaining] Instance ", i, ": ")
    instance = x[i,]
    prediction = predict_model(explainer$model, instance, type = o_type)
    instancePrediction = performance_model(prediction, measures = list(acc)) #TODO: FIXME

    # set meta data for IPC
    id = uuid::UUIDgenerate()
    count = 1
    status = "request"

    instanceJSON = rjson::toJSON(list("id" = c(id), "count" = c(count), "status" = c(status), "precision" = c(0), "instance" = length(instance)-1))
    con = explainer$connection
    writeLines(instanceJSON, con)

    responseRaw = character(0)
    response = character(0)


    while(length(responseRaw) == 0){
      # check for response
      responseRaw = readLines(con)
      if(identical(responseRaw, character(0))) next

      # get response
      response = rjson::fromJSON(responseRaw)
      #count = response$count + 1

      # route command based on status
      type = response$status

      if (type == "eval_request"){
        cat(".")
        anchors = unlist(response$anchors)
        samplesToEvaluate = response$samplesToEvaluate
        # Create pertubations for rule

        instancesDf = do.call(rbind, lapply(1:samplesToEvaluate, function(x){
          perturbate(explainer$perturbator, trainSet, trainSetDisc, instance, c(anchors, explainer$target))
        }))
        pred = predict_model(explainer$model, instancesDf, ...) #, type = o_type

        precision = performance_model(pred, measures = list(acc))[[1]] #FIXME

        matchingLabels = precision * samplesToEvaluate

        # Send precision to anchors
        instanceJSON = rjson::toJSON(list("id" = c(id), "count" = c(count), "status" = c("eval_response"), "matchingLabels" = c(matchingLabels), "precision" = c(precision)))
        writeLines(instanceJSON, con)

      } else if (type == "coverage_request") {
        cat(".")
        features = unlist(response$features)

        perturbationsDf = do.call(rbind, lapply(1:1000, function(x){
          perturbate(makePerturbFun("tabular.featurelessDisc"), trainSet, trainSetDisc, instance, c(integer(0), explainer$target))
        }))

        featureVec = as.data.frame(unclass(instance[,features]))
        colnames(featureVec) = features
        reducedPerturbations = as.data.frame(unclass(perturbationsDf[,features]))
        colnames(reducedPerturbations) = features
        for(i in 1:ncol(reducedPerturbations)){

          lvls = levels(reducedPerturbations[,i])

          lvl = which(sapply(lvls, function(x){
            if(stringr::str_detect(x,"[(\\[]\\d+\\.?(\\d+)?,\\d+\\.?(\\d+)?[)\\]]")){
              return(isInIntervall(x, featureVec[i]))
            } else {
              return(x == featureVec[i])
            }
          }))

          # related to issue #8 | FIXME!
          if (length(lvl)>1){
            lvl = lvl[1]
          }

          featureVec[i] = names(lvl)
        }


        matchingRows = nrow(suppressMessages(plyr::match_df(reducedPerturbations, featureVec)))
        coverage = matchingRows / nrow(reducedPerturbations)


        # Send coverage to anchors
        coverageJSON = rjson::toJSON(list("id" = c(id), "count" = c(count), "status" = c("coverage_response"), "coverage" = c(coverage)))
        writeLines(coverageJSON, con)

      } else if (type == "response"){
        break;
      }

      responseRaw = character(0)
    }
    if ("anchorResult" %in% names(response)){
      cat(" \r"); cat("[Explained] Instance "); cat("\n");
      rules = response$anchorResult[[1]]
      featuresWeight = sapply(rules$canonicalFeatures, getFeatureWeight, candidates = rules, instance = instance, dataset = explainer$trainingsData, datasetDisc = explainer$discTrainingsData)
      addedCoverage = sapply(rules$canonicalFeatures, getAddedCoverage, candidates = rules, instance = instance, dataset = explainer$trainingsData, datasetDisc = explainer$discTrainingsData)
      featuresText = sapply(rules$canonicalFeatures, getFeatureText, candidates = rules, instance = instance, dataset = explainer$trainingsData, datasetDisc = explainer$discTrainingsData)


      for(j in names(featuresText)){
        ridx = 1 + nrow(explanations)
        explanations[ridx, "model_type"] = "Classification" #explainer$model$task.desc$type
        explanations[ridx, "case"] = rownames(instance)
        explanations[ridx, "label"] = instance[,explainer$target]
        explanations[ridx, "label_prob"] = rules$precision
        explanations[ridx, "feature"] = colnames(instance)[as.numeric(j)]
        explanations[ridx, "feature_value"] = instance[,as.numeric(j)]
        explanations[ridx, "feature_weight"] = featuresWeight[[j]]
        explanations[ridx, "added_coverage"] = addedCoverage[[j]]
        explanations[ridx,"feature_desc"] = featuresText[[j]]
        explanations[ridx, "data"] = collapse(unlist(instance))
        explanations[ridx, "prediction"] = prediction$data$response
        explanations[ridx, "precision"] = rules$precision
        explanations[ridx, "coverage"] = rules$coverage
      }

      rules = append(rules, list(response$anchorResult[[1]]))

    } else {
      stopf("R_IllegalFormatException: Could not find field \"anchorResult\" in Server response")
    }
  }

  shutdown(explainer)

  return(explanations)

}

is.data_frame_explainer <- function(x) inherits(x, 'data_frame_explainer')

