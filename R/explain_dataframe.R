#' Explains a specific instance of a tabular explanation scenario.
#'
#' This scenario is described by the previously created anchors.tabular.
#'
#' @rdname explain
#' @name explain
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
explain.data.frame <- function(x, explainer, labels = NULL, ...) {

  checkmate::assert_true(is.data_frame_explainer(explainer))
  m_type <- model_type(explainer)
  o_type <- output_type(explainer)
  if (m_type == 'regression') {
    if (!is.null(labels)) {
      warning('"labels" argument is ignored when explaining regression models')
    }
    stop("Regression models are not yet supported")
  }

  # Remove target column from single instance
  if (!is.null(explainer$target)) {
    targetIndex <- which(colnames(x) == explainer$target)
    if (length(targetIndex) != 0){
      if (length(targetIndex) != 1 && targetIndex < 0) {
        # Explained instance should not necessarily need target column
        #stop("Could not find unambiguous target column")
      } else {
        x <- x[, -targetIndex]
      }
    }
  }

  if (is.null(labels))
    labels <- predict_model(explainer$model, x)

  if (is.null(labels))
    stop("Either labels or a target column to be explained need to be specified")

  explanations = data.frame(matrix(ncol = 12, nrow = 0))
  colnames(explanations) = c(
    "model_type",
    "case",
    "label",
    "label_prob",
    "feature",
    "feature_value",
    "feature_weight",
    "added_coverage",
    "feature_desc",
    "feature_desc_short",
    "data",
    "precision"
  )

  bins = explainer$bins
  rules = list()

  ## TODO: should we pass this without parameters??
  # (takes default: ip = "localhost", port = 6666)
  # -> should consider introducing a settings object to always pass to initAnchors
  backend_connection <- initAnchors(explainer = explainer)
  explainer$connection <- backend_connection

  message("Explaining ", nrow(x), " observations. This may take a while.")

  for (i in 1:nrow(x)) {
    message("[Explaining] Instance ", i, ": ", appendLF = FALSE)

    instance = x[i,]
    discretized_instance = discretize.data.frame(instance, bins)
    # Removed target column from train
    trainData <- explainer$x[, names(explainer$x) %in% names(instance)]

    # Featureless perturbations that are required to obtain coverage of a rule
    # dataset, discretized_dataset, instance, discretized_instance,
    coverage_perturbations <-
      do.call(rbind, lapply(1:explainer$coverage_perturbation_count, function(x) {
        perturbate(perturbTabularDisc,
                   trainData,
                   explainer$discretizedDF,
                   instance,
                   discretized_instance,
                   integer(0),
                   1)
      }))

    # set meta data for IPC
    communication_id = uuid::UUIDgenerate()

    # Trigger first call and start anchors
    initialize.explanation.request(backend_connection, communication_id, length(instance))

    while (TRUE) {
      response <- await.server.response(backend_connection)

      # TODO tomorrow
      if (is.null(response$status)) {
        stop("Java backend sent an invalid response")
      }

      if (response$status == "response") {
        # Server sends stop
        break
      }

      if (response$status == "exception") {
        stop(paste("The server threw an exception:", response$reason))
      }

      if (response$status == "eval_request") {
        # Anchors requests perturbation and model call
        message(".", appendLF = FALSE)

        anchors = unlist(response$anchors)
        samplesToEvaluate = response$samplesToEvaluate

        # Create pertubations for rule
        instancesDf = do.call(rbind, lapply(1:samplesToEvaluate, function(x) {
          perturbate(explainer$perturbator,
                     trainData,
                     explainer$discretizedDF,
                     instance,
                     discretized_instance,
                     anchors,
                     explainer$p
          )
        }))

        pred = predict_model(explainer$model, instancesDf, ...)
        matchingLabels = length(pred[pred==labels[i]])
        # Note that for some reason (convention?), within anchors, we call accurancy precision!
        precision = matchingLabels/samplesToEvaluate

        # Send precision to anchors
        respond.with.precision(backend_connection,
                               communication_id,
                               matchingLabels,
                               precision)
      } else if (response$status == "coverage_request") {
        message("+", appendLF = FALSE)
        coverage <-
          calculate.coverage(instance,
                             unlist(response$features),
                             bins,
                             coverage_perturbations)

        # Send coverage to anchors
        respond.with.coverage(backend_connection, communication_id, coverage)
      }
    }


    if ("anchorResult" %in% names(response)) {
      message(" \r", appendLF = FALSE)
      message("[Explained] Instance ")

      rules = response$anchorResult[[1]]
      featuresWeight = sapply(
        rules$orderedFeatures,
        getFeatureWeight,
        candidates = rules,
        instance = instance,
        dataset = explainer$x
      )
      addedCoverage = sapply(
        rules$orderedFeatures,
        getAddedCoverage,
        candidates = rules,
        instance = instance,
        dataset = explainer$x
      )
      featuresText = sapply(
        rules$orderedFeatures,
        getFeatureText,
        candidates = rules,
        instance = instance,
        dataset = explainer$x,
        bins = explainer$bins
      )

      featuresText = sapply(
        rules$orderedFeatures,
        getFeatureText,
        candidates = rules,
        instance = instance,
        dataset = explainer$x,
        bins = explainer$bins,
        short=F
      )

      featuresTextShort = sapply(
        rules$orderedFeatures,
        getFeatureText,
        candidates = rules,
        instance = instance,
        dataset = explainer$x,
        bins = explainer$bins,
        short=T
      )

      ridx = 1 + nrow(explanations)
      explanations[ridx, "model_type"] = "Classification"
      explanations[ridx, "case"] = rownames(instance)
      explanations[ridx, "label"] = as.character(labels[i])
      explanations[ridx, "label_prob"] = rules$precision
      explanations[ridx, "feature"] = "base"
      explanations[ridx, "feature_weight"] = rules$precision - sum(unlist(featuresWeight))
      explanations[ridx, "added_coverage"] = 0
      explanations[ridx, "data"] = BBmisc::collapse(unlist(instance))
      explanations[ridx, "precision"] = rules$precision
      explanations[ridx, "coverage"] = rules$coverage

      for (j in names(featuresText)) {
        ridx = 1 + nrow(explanations)
        explanations[ridx, "model_type"] = "Classification"
        explanations[ridx, "case"] = rownames(instance)
        explanations[ridx, "label"] = as.character(labels[i]) # TODO why doesn't factor "survive"?
        explanations[ridx, "label_prob"] = rules$precision
        explanations[ridx, "feature"] = colnames(instance)[as.numeric(j)]
        explanations[ridx, "feature_value"] = instance[, as.numeric(j)]
        explanations[ridx, "feature_weight"] = featuresWeight[[j]]
        explanations[ridx, "added_coverage"] = addedCoverage[[j]]
        explanations[ridx, "feature_desc"] = featuresText[[j]]
        explanations[ridx, "feature_desc_short"] = featuresTextShort[[j]]
        explanations[ridx, "data"] = BBmisc::collapse(unlist(instance))
        explanations[ridx, "precision"] = rules$precision
        explanations[ridx, "coverage"] = rules$coverage
      }


      class(explanations) = c("explanations", class(explanations))

      rules = append(rules, list(response$anchorResult[[1]]))
    } else {
      BBmisc::stopf(
        "R_IllegalFormatException: Could not find field \"anchorResult\" in Server response"
      )
    }
  }

  shutdown(explainer)

  return(explanations)

}

is.data_frame_explainer <-
  function(x)
    inherits(x, 'data_frame_explainer')
