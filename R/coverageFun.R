calculate.coverage <- function(instance, features, coverage_perturbations) {

  featureVec <- as.data.frame(unclass(instance[,features]))
  colnames(featureVec) <- features
  reducedPerturbations <- as.data.frame(unclass(coverage_perturbations[,features]))
  colnames(reducedPerturbations) = features

  for(i in 1:ncol(reducedPerturbations)) {
    bin = provideBin.numeric(featureVec[i],
                             bins[[features[i]]]$cuts,
                             bins[[features[i]]]$right)
    featureVec[i] = bin
  }

  matchingRows = nrow(suppressMessages(plyr::match_df(reducedPerturbations, featureVec)))
  coverage = matchingRows / nrow(reducedPerturbations)

  return(coverage)
}
