calculate.coverage <- function(instance, features, coverage_perturbations) {
  featureVec <- as.data.frame(unclass(instance[,features]))
  colnames(featureVec) <- features
  reducedPerturbations <- as.data.frame(unclass(coverage_perturbations[,features]))
  colnames(reducedPerturbations) = features
  for(i in 1:ncol(reducedPerturbations)){

    lvls = levels(reducedPerturbations[,i])

    lvl = which(sapply(lvls, function(x){
      if(stringr::str_detect(x,"[(\\[]\\d+\\.?(\\d+)?,\\d+\\.?(\\d+)?[)\\]]")){
        return(isInIntervall(x, featureVec[i]))
      } else {
        return(x == featureVec[i])
      }
    }))

    # related to issue #8 | FIXME!
    if (length(lvl)>1){
      lvl = lvl[1]
    }

    featureVec[i] = names(lvl)
  }

  matchingRows = nrow(suppressMessages(plyr::match_df(reducedPerturbations, featureVec)))
  coverage = matchingRows / nrow(reducedPerturbations)

  coverage
}
