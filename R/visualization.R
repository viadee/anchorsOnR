#' Visualizes the explanation result
#'
#' @param explanations the explanation result
#' @param featureNames the featureNames
#' @param colPal the color pallet
#'
#' @return the plot object
plotExplanations <- function(explanations, featureNames = NULL, colPal = NULL) {
    if (is.null(featureNames))
      featureNames = unique(explanations[, "feature"])

    precisionMatrix = matrix(rep(0, length(featureNames) * length(unique(explanations[, "case"]))), ncol =
                               length(featureNames))
    colnames(precisionMatrix) = featureNames
    rownames(precisionMatrix) = unique(explanations[, "case"])

    coverageMatrix = as.data.frame(matrix(rep(0, length(
      unique(explanations[, "case"])
    ) * 2), ncol = 2))
    colnames(coverageMatrix) = c("coverage", "label")
    rownames(coverageMatrix) = unique(explanations[, "case"])

    bins = as.data.frame(precisionMatrix)
    sapply(colnames(precisionMatrix), function(featureName) {
      cases = unique(explanations[, "case"])
      sapply(cases, function(case) {
        coverageMatrix[case, "coverage"] <<-
          unique(explanations[explanations[, "case"] == case, "coverage"])
        coverageMatrix[case, "label"] <<-
          unique(explanations[explanations[, "case"] == case, "label"])
      })
      sapply(cases, function(case, featureName) {
        if (featureName %in% explanations[explanations[, "case"] == case, "feature"]) {
          precisionMatrix[case, featureName] <<-
            explanations[explanations[, "case"] == case &
                           explanations[, "feature"] == featureName, "feature_weight"]
          bins[case, featureName] <<-
            explanations[explanations[, "case"] == case &
                           explanations[, "feature"] == featureName, "feature_desc_short"]
        }
      }, featureName)
    })


    prevPar = par(no.readonly = TRUE)
    #par(mfrow = c(length(unique(coverageMatrix[,"label"]))+1, 1), mar = c(3, 4, 3, 4))
    nf <-
      layout(matrix(1:(length(
        unique(explanations[, "label"])
      ) + 1), ncol = 1), heights = c(rep(2, length(
        unique(explanations[, "label"])
      )), 1), TRUE)
    if (is.null(colPal)) {
      colPal = grDevices::terrain.colors(length(featureNames))
    }

    maxYLim = 0
    sapply(1:length(unique(coverageMatrix[, "label"])), function(labelIndex) {
      currentLabel =  unique(coverageMatrix[, "label"])[labelIndex]

      cSum = sum(coverageMatrix[rownames(coverageMatrix)[which(coverageMatrix[, "label"] ==
                                                                 currentLabel)], "coverage"]) + length(rownames(coverageMatrix)[which(coverageMatrix[, "label"] ==
                                                                                                                                        currentLabel)]) * 0.2 * mean(coverageMatrix[rownames(coverageMatrix)[which(coverageMatrix[, "label"] ==
                                                                                                                                                                                                                     currentLabel)], "coverage"])
      if (cSum > maxYLim)
        maxYLim <<- cSum


    })
    for (i in 1:length(unique(coverageMatrix[, "label"]))) {
      currentLabel =  unique(coverageMatrix[, "label"])[i]
      toPlot = t(precisionMatrix[coverageMatrix[, "label"] == currentLabel, ])
      if (nrow(toPlot) == 1) {
        toPlot = matrix(toPlot)
        colnames(toPlot) = rownames(coverageMatrix)[which(coverageMatrix[, "label"] ==
                                                            currentLabel)]
      }

      if (i == length(unique(coverageMatrix[, "label"]))) {
        p = barplot(
          toPlot,
          width = coverageMatrix[colnames(toPlot), "coverage"],
          ylab = currentLabel,
          xlab = "Precision",
          col = colPal,
          horiz = TRUE,
          names.arg = colnames(toPlot),
          xlim = c(0, 1.1),
          ylim = c(0, maxYLim),
          xpd = F,
          axes = F
        )
        axis(
          side = 1,
          at = seq(0, 1.0, 0.2),
          labels = T,
          tick = T
        )
      } else{
        p = barplot(
          toPlot,
          width = coverageMatrix[colnames(toPlot), "coverage"],
          ylab = currentLabel,
          xlab = "",
          col = colPal,
          horiz = TRUE,
          names.arg = colnames(toPlot),
          xlim = c(0, 1.1),
          ylim = c(0, maxYLim),
          axes = F,
          xpd = F
        )
      }

      cumToPlot = toPlot
      a = sapply(1:nrow(toPlot), function(rowNo) {
        if (rowNo == 1) {
          cumToPlot[rowNo, ] <<- toPlot[1, ]
        } else if (ncol(toPlot) == 1) {
          cumToPlot[rowNo, ] <<- sum(toPlot[1:rowNo, 1])
        } else{
          cumToPlot[rowNo, ] <<- colSums(toPlot[1:rowNo, ])
        }

      })



      a = sapply(1:ncol(toPlot), function(ncol) {
        relevantBins = bins[rownames(coverageMatrix)[which(coverageMatrix[, "label"] ==
                                                             currentLabel)],]
        relevantBins = relevantBins[ncol, ]
        text(
          x = cumToPlot[, ncol],
          y = p[ncol],
          labels = ifelse(
            relevantBins == 0,
            "",
            substr(
              relevantBins,
              start = nchar(colnames(precisionMatrix)) + 1,
              stop = 100000
            )
          ),
          cex = 1,
          pos = 2
        )

        text(
          x = cumToPlot[nrow(cumToPlot), ncol],
          y = p[ncol],
          labels = paste("Cov:", coverageMatrix[colnames(cumToPlot)[ncol], "coverage"]),
          cex = 1,
          pos = 4
        )
      })



    }

    plot(NULL , xaxt = 'n', yaxt = 'n', bty = 'n', ylab = '', xlab = '', xlim = 0:1, ylim = 0:1)

    legend(
      "bottom",
      legend = featureNames,
      fill = colPal,
      xpd = TRUE,
      inset = c(0, 0),
      ncol = ifelse(
        length(featureNames) > 5,
        round(length(featureNames) / 5, 0),
        length(featureNames)
      )
    )

    par(prevPar)
  }
