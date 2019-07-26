
getFeatureWeight <- function(candidates, feature, instance, dataset, datasetDisc){
  if (candidates$addedFeature == feature){
    return(candidates$addedPrecision)
  } else if ("parentCandidate" %in% names(candidates)){
    return (getFeatureWeight(candidates = candidates$parentCandidate, feature = feature, instance = instance, dataset = dataset, datasetDisc = datasetDisc))
  }
}

getFeatureText <- function(candidates, feature, instance, dataset, datasetDisc){
  if (candidates$addedFeature == feature){
    return(paste(colnames(dataset)[feature+1], "IN INLC RANGE", datasetDisc[as.numeric(rownames(instance)),feature+1]))
  } else if ("parentCandidate" %in% names(candidates)){
    return (getFeatureText(candidates = candidates$parentCandidate, feature = feature, instance = instance, dataset = dataset, datasetDisc = datasetDisc))
  }
}

getInstanceText <- function(instance){
  lapply(colnames(instance), function(name){
    return(paste(name, "=", paste0("'",instance[name],"'")))
  })
}
