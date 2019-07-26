#' Explain a given instance of a dataset.
#'
#' \code{explain} returns a set of rules that explain the decision of a model on a given instance.
#'
#' @param ctrl a \code{control} object of class "anchors_control".
#' @param dataset The dataset in which the observation resides
#' @param datasetDisc The discretized dataset. Must be same size as \code{dataset}
#' @param labelIdx The number of the target column in the dataset
#' @param instanceIdx The number of the target column in the dataset
#' @return Set of rules
#' @export
OLD_explain <- function(ctrl, dataset, datasetDisc, labelIdx, instanceIdx){
  checkmate::assertClass(ctrl, "anchors_control")
  if (is.null(ctrl$perturbate))
    stopf("No perturbation function provided.")
  if (is.null(ctrl$predict))
    stopf("No prediction function provided.")
  checkmate::assertDataFrame(dataset)
  checkmate::assertDataFrame(datasetDisc)
  if (dim(dataset) != dim(datasetDisc))
    stopf("Discretized dataset does not have the same dimensions as the original dataset.")
  checkmate::assertInt(instanceIdx)
  checkmate::assertInt(labelIdx)

  # extract instance to explain
  instance = dataset[instanceIdx,]

  label = colnames(dataset)[labelIdx]

  # calculate initial fitnes
  instancePrediction = do.call(ctrl$performance, args = list(do.call(predict, list("object" = ctrl$predict.pars[[1]], "newdata" = instance)), ctrl$performance.pars[[1]]))
  instance = instance[, -labelIdx]

  # set meta data for IPC
  id = uuid::UUIDgenerate()
  count = 1
  status = "request"

  instanceJSON = rjson::toJSON(list("id" = c(id), "count" = c(count), "status" = c(status), "precision" = c(0), "instance" = length(instance)))
  con = socketConnection("localhost", 6666)
  writeLines(instanceJSON, con)

  responseRaw = character(0)
  response = character(0)

  cat("Explaining. This may take a while.", sep = "\n")
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
      anchors = response$anchors
      samplesToEvaluate = response$samplesToEvaluate

      # Create pertubations for rule
      newInstance = do.call(what = ctrl$perturbate, args = list(ctrl$perturbate.pars[[1]], dataset, datasetDisc, instanceIdx, anchors))
      #newInstance = perturbate(perturbationFun = perturbator, dataset = dataset, datasetDisc = datasetDisc, instanceIdx = instanceIdx, anchors = anchors)

      # Test pertubations with model
      pred = do.call(predict, list("object" = ctrl$predict.pars[[1]], "newdata" = newInstance))
      measure = do.call(ctrl$performance, args = list(pred, ctrl$performance.pars[[1]]))
      matchingLabels = length(measure[measure == instancePrediction])
      precision = matchingLabels / length(measure)

      # Send precision to anchors
      instanceJSON = rjson::toJSON(list("id" = c(id), "count" = c(count), "status" = c("eval_response"), "matchingLabels" = c(matchingLabels), "precision" = c(precision)))
      writeLines(instanceJSON, con)


    } else if (type == "response"){
      break;
    }

    #5) go to 1
    responseRaw = character(0)
  }

  if ("anchorResult" %in% names(response)){
    cat("\n"); cat("Explained!", sep = "\n")
    return (response$anchorResult[[1]])
  } else {
    stopf("R_IllegalFormatException: Could not find field \"anchorResult\" in Server response")
  }

}


