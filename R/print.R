#' Prints a human readable IF-THEN sequence for all explanations
#'
#' This function prints a set of IF-THEN rules for each explanation.
#'
#' @param explainer A `` as returned by [anchors()].
#' @param explanation A `data.frame` as returned by [explain()].
#'
#' @return Prints a set of rules
#'
#' @export
printExplanations <- function(explainer, explanations){
  num_cases <- unique(suppressWarnings(as.numeric(explanations$case)))
  if (!anyNA(num_cases)) {
    explanations$case <- factor(explanations$case, levels = as.character(sort(num_cases)))
  }
  for(i in levels(explanations$case)){
    instance = explainer$x[i,]
    cat(paste("====Explained Instance ", i,"===="), sep = "\n")
    for(col in 1:(ncol(instance)-1)){
      cat(paste(colnames(instance)[col], "=", instance[col])); cat("\n");
    }
    cat(paste("WITH LABEL", names(explainer$feature_type)[explainer$target], "=", paste0("'",unique(instance[,explainer$target]),"'")), sep = "\n")
    case = explanations[explanations[, "case"] ==i,]
    #for (j in seq_along(rownames(case))){
    #  cat(paste(case[j,"feature"], "=", case[j, "feature_value"])); cat("\n");
    #}
    #cat(paste("WITH LABEL", names(explainer$feature_type)[explainer$target], "=", paste0("'",unique(case[,"label"]),"'")), sep = "\n")
    cat("====Result====", sep = "\n")

    # Empty rule removed
    actual_case <- case[case$feature != "base",]

    if (nrow(actual_case) == 0) {
      cat("IF [empty rule] \n")
    } else {

      for(j in seq_along(rownames(actual_case))){
        if (j == 1){
          if (length(seq_along(rownames(actual_case))) == j){
            cat(paste("IF", actual_case[j, "feature_desc"], "(ADDED PRECISION:", paste0(actual_case[j, "feature_weight"],", ADDED COVERAGE: ",actual_case[j, "added_coverage"],")")), sep = "\n")
          } else {
            cat(paste("IF", actual_case[j, "feature_desc"], "(ADDED PRECISION:", paste0(actual_case[j, "feature_weight"],", ADDED COVERAGE: ",actual_case[j, "added_coverage"],")"), "AND"), sep = "\n")
          }
        } else if (j > 1 && j < nrow(actual_case)){
          cat(paste(actual_case[j, "feature_desc"], "(ADDED PRECISION:", paste0(actual_case[j, "feature_weight"],", ADDED COVERAGE: ",actual_case[j, "added_coverage"],")"),  "AND"), sep = "\n")
        } else {
          cat(paste(actual_case[j, "feature_desc"], "(ADDED PRECISION:", paste0(actual_case[j, "feature_weight"],", ADDED COVERAGE: ",actual_case[j, "added_coverage"],")")),  sep = "\n");
        }
      }
    }
    predictOutput=paste("THEN PREDICT", paste0("'",unique(case$label),"'"))
    cat(predictOutput, sep = "\n")
    cat(paste("WITH PRECISION", unique(case[,"precision"]), "AND COVERAGE", unique(case[,"coverage"])), sep = "\n")
    cat("\n")
  }
}
