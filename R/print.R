#' #' Prints a human readable IF-THEN sequence for all explanations
#' #'
#' #' This function prints a set of IF-THEN rules for each explanation.
#' #'
#' #' @param explainer An `explainer` as returned by [anchors()].
#' #' @param explanation An `explanations` object as returned by [explain()].
#' #'
#' #' @return Prints a set of rules
#' #'
#' #' @export
printExplanations <- function(explainer, explanations){

  num_cases <- unique(suppressWarnings(as.numeric(explanations$case)))
  if (!anyNA(num_cases)) {
    explanations$case <- factor(explanations$case, levels = as.character(sort(num_cases)))
  }
  for(i in levels(explanations$case)){
    printInstance(i, explainer, explanations)
    printRule(i, explainer, explanations)
  }
}


printInstance <- function(i, explainer, explanations){
  instance = explainer$x[i,]
  cat(paste("====Explained Instance ", i,"===="), sep = "\n")
  for(col in 1:(ncol(instance)-1)){
    cat(paste(colnames(instance)[col], "=", unlist(instance[col]))); cat("\n");
  }
  cat(paste("WITH LABEL", explainer$target, "=", paste0("'",unique(instance[,explainer$target]),"'")), sep = "\n")
}

printRule <- function(i, explainer, explanations){

  explanations$feature_weight = explanations$feature_weight * 100
  explanations$added_coverage = explanations$added_coverage * 100
  explanations$precision = explanations$precision * 100
  explanations$coverage = explanations$coverage * 100

  if (explainer$verbose == FALSE){
    explanations$feature_weight = round(explanations$feature_weight, 2)
    explanations$added_coverage = round(explanations$added_coverage, 2)
    explanations$precision = round(explanations$precision, 2)
    explanations$coverage = round(explanations$coverage, 2)
  }

  case = explanations[explanations[, "case"] ==i,]
  case = case[order(case$feature_weight, decreasing = TRUE),]
  cat("====Result====", sep = "\n")

  # Empty rule removed
  actual_case <- case[case$feature != "base",]

  if (nrow(actual_case) == 0) {
    cat("IF [empty rule] \n")
  } else {

    for(j in seq_along(rownames(actual_case))){
      if (j == 1){
        if (length(seq_along(rownames(actual_case))) == j){
          cat(paste("IF", actual_case[j, "feature_desc"], "(ADDED PRECISION:", paste0(actual_case[j, "feature_weight"],"%, ADDED COVERAGE: ",actual_case[j, "added_coverage"],"%)")), sep = "\n")
        } else {
          cat(paste("IF", actual_case[j, "feature_desc"], "(ADDED PRECISION:", paste0(actual_case[j, "feature_weight"],"%, ADDED COVERAGE: ",actual_case[j, "added_coverage"],"%)"), "AND"), sep = "\n")
        }
      } else if (j > 1 && j < nrow(actual_case)){
        cat(paste(actual_case[j, "feature_desc"], "(ADDED PRECISION:", paste0(actual_case[j, "feature_weight"],"%, ADDED COVERAGE: ",actual_case[j, "added_coverage"],"%)"),  "AND"), sep = "\n")
      } else {
        cat(paste(actual_case[j, "feature_desc"], "(ADDED PRECISION:", paste0(actual_case[j, "feature_weight"],"%, ADDED COVERAGE: ",actual_case[j, "added_coverage"],"%)")),  sep = "\n");
      }
    }
  }
  predictOutput=paste("THEN PREDICT", paste0("'",unique(case$label),"'"))
  cat(predictOutput, sep = "\n")
  cat(paste0("WITH PRECISION ", unique(case[,"precision"]), "%, AND COVERAGE ", unique(case[,"coverage"]),"%"), sep = "\n")
  cat("\n")
}

