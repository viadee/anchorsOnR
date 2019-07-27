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
    for(j in seq_along(rownames(case))){
      if (j == 1){
        if (length(seq_along(rownames(case))) == j){
          cat(paste("IF", case[j, "feature_desc"], "(ADDED PRECISION:", paste0(case[j, "feature_weight"],", ADDED COVERAGE: ",case[j, "added_coverage"],")")), sep = "\n")
        } else {
          cat(paste("IF", case[j, "feature_desc"], "(ADDED PRECISION:", paste0(case[j, "feature_weight"],", ADDED COVERAGE: ",case[j, "added_coverage"],")"), "AND"), sep = "\n")
        }
      } else if (j > 1 && j < nrow(case)){
        cat(paste(case[j, "feature_desc"], "(ADDED PRECISION:", paste0(case[j, "feature_weight"],", ADDED COVERAGE: ",case[j, "added_coverage"],")"),  "AND"), sep = "\n")
      } else {
        cat(paste(case[j, "feature_desc"], "(ADDED PRECISION:", paste0(case[j, "feature_weight"],", ADDED COVERAGE: ",case[j, "added_coverage"],")")),  sep = "\n");
      }
    }
    cat(paste("THEN PREDICT", paste0("'",unique(case[,"prediction"]),"'")), sep = "\n")
    cat(paste("WITH PRECISION", unique(case[,"precision"]), "AND COVERAGE", unique(case[,"coverage"])), sep = "\n")
    cat("\n")

  }
}
