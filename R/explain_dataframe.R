#' @rdname explain
#' @name explain
#'
#' @importFrom gower gower_dist
#' @importFrom stats dist
#' @export
explain.data.frame <-
  function(x,
           explainer,
           labels = NULL,
           n_labels = NULL,
           feature_select = 'auto',
           probKeepPerturbations = 0.6,
           ...) {
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

    ## TODO: should we pass this without parameters?? (takes default: ip = "localhost", port = 6666) -> should consider introducing a settings object to always pass to initAnchors
    backend_connection <- initAnchors()
    explainer$connection <- backend_connection

    trainSet = explainer$trainingsData[, -explainer$target]
    bins = explainer$bins
    rules = list()

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
      "data",
      "prediction",
      "precision"
    )

    cat("Explaining ", nrow(x), " observations. This may take a while.")
    cat("\n")


    for (i in 1:nrow(x)) {
      cat("[Explaining] Instance ", i, ": ")
      instance = x[i, ]
      prediction = predict_model(explainer$model, instance, type = o_type)
      # FIXME - what is wrong here? I don't see a problem
      instancePrediction = performance_model(prediction, measures = list(acc))

      # Featureless perturbations that are required to obtain coverage of a rule
      coverage_perturbations <-
        do.call(rbind, lapply(1:1000, function(x) {
          perturbate(
            makePerturbFun("tabular.featurelessDisc"),
            trainSet,
            bins,
            instance,
            c(integer(0), explainer$target),
            probKeepPerturbations
          )
        }))

      # set meta data for IPC
      communication_id = uuid::UUIDgenerate()

      # Trigger first call and start anchors
      initialize.explanation.request(backend_connection, communication_id, length(instance) - 1)

      while (TRUE) {
        response <- await.server.response(backend_connection)

        if (response$status == "response") {
          # Server sends stop
          break

        }

        if (response$status == "eval_request") {
          # Anchors requests perturbation and model call
          cat(".")

          anchors = unlist(response$anchors)
          samplesToEvaluate = response$samplesToEvaluate

          # Create pertubations for rule
          # TODO move to samplesToEvaluate to parameter
          instancesDf = do.call(rbind, lapply(1:samplesToEvaluate, function(x) {
            perturbate(
              explainer$perturbator,
              trainSet,
              bins,
              instance,
              c(anchors, explainer$target),
              probKeepPerturbations
            )
          }))

          pred = predict_model(explainer$model, instancesDf, ...) #, type = o_type

          precision = performance_model(pred, measures = list(acc))[[1]] #FIXME

          # TODO wouldn't it better to straight away only send the correctly predicted labels?
          matchingLabels = precision * samplesToEvaluate

          # Send precision to anchors
          respond.with.precision(backend_connection,
                                 communication_id,
                                 matchingLabels,
                                 precision)
        } else if (response$status == "coverage_request") {
          cat("+")
          coverage <-
            calculate.coverage(instance,
                               unlist(response$features),
                               coverage_perturbations)

          # Send coverage to anchors
          respond.with.coverage(backend_connection, communication_id, coverage)
        }
      }


      if ("anchorResult" %in% names(response)) {
        cat(" \r")
        cat("[Explained] Instance ")
        cat("\n")

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

        for (j in names(featuresText)) {
          ridx = 1 + nrow(explanations)
          explanations[ridx, "model_type"] = "Classification" #explainer$model$task.desc$type
          explanations[ridx, "case"] = rownames(instance)
          explanations[ridx, "label"] = instance[, explainer$target]
          explanations[ridx, "label_prob"] = rules$precision
          explanations[ridx, "feature"] = colnames(instance)[as.numeric(j)]
          explanations[ridx, "feature_value"] = instance[, as.numeric(j)]
          explanations[ridx, "feature_weight"] = featuresWeight[[j]]
          explanations[ridx, "added_coverage"] = addedCoverage[[j]]
          explanations[ridx, "feature_desc"] = featuresText[[j]]
          explanations[ridx, "data"] = collapse(unlist(instance))
          explanations[ridx, "prediction"] = prediction$data$response
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
