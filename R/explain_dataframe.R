#' Explains a specific instance of a tabular explanation scenario.
#'
#' This scenario is described by the previously created anchors.tabular.
#'
#' @rdname explain
#' @name explain
#'
#' @export
explain.data.frame <- function(x, explainer, labels = NULL,
                               feature_select = 'auto', probKeep=0.5, ...) {

  checkmate::assert_true(is.data_frame_explainer(explainer))
  m_type <- model_type(explainer)
  o_type <- output_type(explainer)
  if (m_type == 'regression') {
    if (!is.null(labels) || !is.null(n_labels)) {
      warning('"labels" and "n_labels" arguments are ignored when explaining regression models')
    }
    stop("Regression models are not yet supported")
  }

  if (!is.null(explainer$target)) {
    targetIndex <- which(colnames(x) == explainer$target)
    if (length(targetIndex) != 1 && targetIndex < 0)
      stop("Could not find unambiguous target column")

    if (is.null(labels))
      labels <- predict_model(explainer$model, x)$data$response

    x <- x[, -targetIndex]
  }

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
    trainData <- explainer$trainingsData[, names(explainer$trainingsData) %in% names(instance)]

    # Featureless perturbations that are required to obtain coverage of a rule
    coverage_perturbations <-
      do.call(rbind, lapply(1:1000, function(x) {
        perturbate(
          makePerturbFun("tabular.featurelessDisc"),
          trainData,
          bins,
          instance,
          integer(0),
          probKeep
        )
      }))


    # set meta data for IPC
    communication_id = uuid::UUIDgenerate()

    # Trigger first call and start anchors
    initialize.explanation.request(backend_connection, communication_id, length(instance))

    while (TRUE) {
      response <- await.server.response(backend_connection)

      if (response$status == "response") {
        # Server sends stop
        break
      }

      if (response$status == "eval_request") {
        # Anchors requests perturbation and model call
        message(".", appendLF = FALSE)

        anchors = unlist(response$anchors)
        samplesToEvaluate = response$samplesToEvaluate

        # Create pertubations for rule
        instancesDf = do.call(rbind, lapply(1:samplesToEvaluate, function(x) {
          perturbate(
            explainer$perturbator,
            trainData,
            bins,
            instance,
            anchors,
            probKeep
          )
        }))

        pred = predict_model(explainer$model, instancesDf, ...)
        pred$data$truth = labels[i]
        precision = performance_model(pred, measures = list(acc))[[1]]

        # TODO wouldn't it better to straight away only send the correctly predicted labels?
        matchingLabels = precision * samplesToEvaluate

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
        rules$canonicalFeatures,
        getFeatureWeight,
        candidates = rules,
        instance = instance,
        dataset = explainer$trainingsData
      )
      addedCoverage = sapply(
        rules$canonicalFeatures,
        getAddedCoverage,
        candidates = rules,
        instance = instance,
        dataset = explainer$trainingsData
      )
      featuresText = sapply(
        rules$canonicalFeatures,
        getFeatureText,
        candidates = rules,
        instance = instance,
        dataset = explainer$trainingsData,
        bins = explainer$bins
      )

      featuresText = sapply(
        rules$canonicalFeatures,
        getFeatureText,
        candidates = rules,
        instance = instance,
        dataset = explainer$trainingsData,
        bins = explainer$bins,
        short=F
      )

      featuresTextShort = sapply(
        rules$canonicalFeatures,
        getFeatureText,
        candidates = rules,
        instance = instance,
        dataset = explainer$trainingsData,
        bins = explainer$bins,
        short=T
      )

      for (j in names(featuresText)) {
        ridx = 1 + nrow(explanations)
        explanations[ridx, "model_type"] = "Classification" #explainer$model$task.desc$type
        explanations[ridx, "case"] = rownames(instance)
        explanations[ridx, "label"] = as.character(labels[i]) # TODO why doesn't factor "survive"?
        explanations[ridx, "label_prob"] = rules$precision
        explanations[ridx, "feature"] = colnames(instance)[as.numeric(j)]
        explanations[ridx, "feature_value"] = instance[, as.numeric(j)]
        explanations[ridx, "feature_weight"] = featuresWeight[[j]]
        explanations[ridx, "added_coverage"] = addedCoverage[[j]]
        explanations[ridx, "feature_desc"] = featuresText[[j]]
        explanations[ridx, "feature_desc_short"] = featuresTextShort[[j]]
        explanations[ridx, "data"] = collapse(unlist(instance))
        explanations[ridx, "precision"] = rules$precision
        explanations[ridx, "coverage"] = rules$coverage
      }

      rules = append(rules, list(response$anchorResult[[1]]))
    } else {
      stopf(
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
