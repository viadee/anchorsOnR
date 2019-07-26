performance_model = function(pred, measures, task = NULL, model = NULL, feats = NULL) {
  if (!is.null(pred)) {
    assertClass(pred, classes = "Prediction")
    if (!is.null(pred$task.desc)){
      assertClass(pred$task.desc, classes = "TaskDesc")
      return(mlr::performance(pred, measures, task, model, feats))
    } else {
      #FIXME That's so bad
      acc = length(ifelse(as.character(pred$data$truth) == as.character(pred$data$response), TRUE, FALSE)) / length(pred$data$truth)
      return(acc)
    }



  }


}
