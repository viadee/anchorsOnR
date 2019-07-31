performance_model = function(pred, measures, task = NULL, model = NULL, feats = NULL) {
  if (!is.null(pred)) {
    checkmate::assertClass(pred, classes = "Prediction")
    if (!is.null(pred$task.desc)){
      checkmate::assertClass(pred$task.desc, classes = "TaskDesc")
      return(mlr::performance(pred, measures, task, model, feats))
    } else {
      acc = length(which(as.character(pred$data$truth) == as.character(pred$data$response))) / length(pred$data$truth)
      return(acc)
    }
  }
}
