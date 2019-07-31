calculate.coverage <-
  function(instance,
           features,
           bins,
           coverage_perturbations) {
    featureVec <- as.data.frame(unclass(instance[, features]))
    colnames(featureVec) <- features
    reducedPerturbations <-
      as.data.frame(unclass(coverage_perturbations[, features]))
    colnames(reducedPerturbations) = features

    for (i in 1:ncol(reducedPerturbations)) {
      bin <- bins[[features[i]]]
      if (!is.null(bin$doDiscretize) && !bin$doDiscretize) {
        providedBin = featureVec[i]
      } else {
        providedBin = provideBin.numeric(featureVec[i],
                                         bin$cuts,
                                         bin$right)
      }
      featureVec[i] = providedBin
    }

    matchingRows = nrow(suppressMessages(plyr::match_df(reducedPerturbations, featureVec)))
    coverage = matchingRows / nrow(reducedPerturbations)

    return(coverage)
  }
