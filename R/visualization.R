#' Visualizes the explanation result
#'
#' @param explanations the explanation result
#' @param featureNames the featureNames
#' @param colPal the color pallet
#' @param adjColorTransparent the transparency of the colors (alpha)
#' @param pdf whether output should be pdf
#'
#' @return the plot object
#'
#' @export
plotExplanations <- function(explanations,
                             featureNames = NULL,
                             colPal = NULL,
                             adjColorTransparent = 1.0,
                             pdf=NULL) {
    if (is.null(featureNames)) {
      featureNames = unique(explanations[, "feature"])
    }

    precisionMatrix = matrix(rep(0, length(featureNames) * length(unique(explanations[, "case"]))), ncol =
                               length(featureNames))
    colnames(precisionMatrix) = featureNames
    rownames(precisionMatrix) = unique(explanations[, "case"])

    coverageMatrix = as.data.frame(matrix(rep(0, length(
      unique(explanations[, "case"])
    ) * 3), ncol = 3))
    colnames(coverageMatrix) = c("coverage", "coverageBin" ,"label")
    rownames(coverageMatrix) = unique(explanations[, "case"])

    bins = as.data.frame(precisionMatrix)
    minCovWidth=1.5*par('mai')[1]/par('mar')[1]
    maxCovWidth=2*minCovWidth
    anyBelowZero=F
    minimumBase=1
    sapply(colnames(precisionMatrix), function(featureName) {
      cases = unique(explanations[, "case"])
      sapply(cases, function(case) {
        coverageMatrix[case, "coverage"] <<-
          unique(explanations[explanations[, "case"] == case, "coverage"])

            if(coverageMatrix[case, "coverage"]<0.2){
              coverageMatrix[case, "coverageBin"]<<-minCovWidth
            }else if(coverageMatrix[case, "coverage"]<0.4){
              coverageMatrix[case, "coverageBin"]<<-minCovWidth+(maxCovWidth-minCovWidth)/4
            }else if(coverageMatrix[case, "coverage"]<0.6){
              coverageMatrix[case, "coverageBin"]<<-minCovWidth+(maxCovWidth-minCovWidth)/4*2
            }else if(coverageMatrix[case, "coverage"]<0.8){
              coverageMatrix[case, "coverageBin"]<<-minCovWidth+(maxCovWidth-minCovWidth)/4*3
            }else{
              coverageMatrix[case, "coverageBin"]<<-maxCovWidth
            }

        coverageMatrix[case, "label"] <<-
          unique(explanations[explanations[, "case"] == case, "label"])
      })
      sapply(cases, function(case, featureName) {
        #if(featureName == "base"){
        #  precisionMatrix[case, featureName] <<-
        #    unique(explanations[explanations[, "case"] == case, "precision"])-
        #    sum(explanations[explanations[, "case"] == case, "feature_weight"])
        #  bins[case, featureName] <<- "base"
        #  if(minimumBase>precisionMatrix[case, featureName])
        #    minimumBase<<-precisionMatrix[case, featureName]
        #}else
        if (featureName %in% explanations[explanations[, "case"] == case, "feature"]) {
          precisionMatrix[case, featureName] <<-
            explanations[explanations[, "case"] == case &
                           explanations[, "feature"] == featureName, "feature_weight"]
          if(precisionMatrix[case, featureName]<0){
            precisionMatrix[case, featureName]<<-0.0001
            anyBelowZero<<-T
          }
          if(featureName == "base"){
            bins[case, featureName] <<- "base"
            if(minimumBase>precisionMatrix[case, featureName])
                minimumBase<<-precisionMatrix[case, featureName]
          } else {
            bins[case, featureName] <<-
              explanations[explanations[, "case"] == case &
                             explanations[, "feature"] == featureName, "feature_desc_short"]
          }
        }
      }, featureName)

    })



    if(anyBelowZero){
      message("Negative added precision is visualized as 0.")
    }
    prevPar = par(no.readonly = TRUE)

    if(!is.null(pdf)){
      pdf(file = pdf, paper="a4", width=8, height=11)
    }
    par(mar = c(4, 4, 0, 0))
    targetHeight=dev.size("in")[2]

    weightedCasesPerLabel=sapply(unique(explanations[, "label"]), function(label){
      heightForColumns= length(which(coverageMatrix[,"label"]==label))*par('mai')[1]/par('mar')[1]- par('mai')[1]/par('mar')[1]/2
      return(sum(coverageMatrix[which(coverageMatrix[,"label"]==label),"coverageBin"])+heightForColumns)
    })


    numberOfFeaturesInOneLegenRow = floor(dev.size("in")[1]/max(strwidth(featureNames, units="inches")))-1
    heightForLegend = 2*par('mai')[1]/par('mar')[1]+ceiling(length(featureNames)/numberOfFeaturesInOneLegenRow)*par('mai')[1]/par('mar')[1]
    heightPerCoveragePoint=(targetHeight-heightForLegend-par("mai")[1]*length(unique(explanations[, "label"]))*0.75-par("mai")[1]*0.25)/sum(weightedCasesPerLabel)


    heightWithPar = heightPerCoveragePoint*weightedCasesPerLabel+par("mai")[1]*0.75
    heightWithPar[length(heightWithPar)]=heightWithPar[length(heightWithPar)]+par("mai")[1]*0.25
    nf <-
      layout(matrix(1:(length(
        unique(explanations[, "label"])
      ) + 1), ncol = 1), heights = c(heightWithPar, heightForLegend), TRUE)

    if (is.null(colPal)) {
      colPal = grDevices::terrain.colors(length(featureNames))
    }


    names(weightedCasesPerLabel)=unique(explanations[, "label"])

    for (i in 1:length(unique(coverageMatrix[, "label"]))) {
      currentLabel =  unique(coverageMatrix[, "label"])[i]

      toPlot = t(precisionMatrix[coverageMatrix[, "label"] == currentLabel, ])
      # In some cases (especially when length = 1), colnames will not be set
      if (is.null(colnames(toPlot)))
        colnames(toPlot) = rownames(precisionMatrix)

      if (nrow(toPlot) == 1 && ncol(precisionMatrix)>1) {
        toPlot = matrix(toPlot)
        colnames(toPlot) = rownames(coverageMatrix)[which(coverageMatrix[, "label"] ==
                                                            currentLabel)]
      }


      maximumPrecisionConsideringNegative = max(apply(precisionMatrix, 1, sum))


      if (i == length(unique(coverageMatrix[, "label"]))) {
        par(mar = c(4, 4, 0, 0))
        spaces=c(par('mai')[1]/par('mar')[1]/mean(coverageMatrix[which(coverageMatrix[,"label"]==currentLabel),"coverageBin"])/2, rep(par('mai')[1]/par('mar')[1]/mean(coverageMatrix[which(coverageMatrix[,"label"]==currentLabel),"coverageBin"]), ncol(toPlot)-1))
        p = tryCatch({
          barplot(
            toPlot,
            width = coverageMatrix[colnames(toPlot), "coverageBin"],
            ylab =  paste("\'", currentLabel,"\'", "cases"),
            xlab = "Precision",
            space=spaces,
            col =  adjustcolor(colPal,  alpha.f=adjColorTransparent),
            horiz = TRUE,
            names.arg = rep("", length(colnames(toPlot))),
            xlim = c(ifelse(minimumBase>0.01, minimumBase-0.01, 0), min(maximumPrecisionConsideringNegative+0.1, maximumPrecisionConsideringNegative+((maximumPrecisionConsideringNegative-ifelse(minimumBase>0.01, minimumBase-0.01, 0))/5))),
            ylim = c(0,weightedCasesPerLabel[currentLabel]),
            xpd = F,
            axes = F
          )
        }, error = function(e) {

          print(e)
          message("Please check size of plotting region. It is too small. Alternatively: plot in pdf.")
          dev.off()
          return(NA)
        })

        suppressWarnings(if(is.na(p)) return(NULL))
        axis(
          side = 1,
          at = c(ifelse(minimumBase>0.01, minimumBase-0.01, 0),seq(0, 1.0, 0.1)),
          labels = T,
          tick = T
        )
        axis(
          side = 2,
          at = p,
          labels = colnames(toPlot),
          tick = F,
          las=1
        )
      } else{
        par(mar = c(3, 4, 0, 0))
        spaces=c(par('mai')[1]/par('mar')[1]/mean(coverageMatrix[which(coverageMatrix[,"label"]==currentLabel),"coverageBin"])/2, rep(par('mai')[1]/par('mar')[1]/mean(coverageMatrix[which(coverageMatrix[,"label"]==currentLabel),"coverageBin"]), ncol(toPlot)-1))
        p=tryCatch({
        barplot(
          toPlot,
          width = coverageMatrix[colnames(toPlot), "coverageBin"],
          ylab = paste("\'", currentLabel,"\'", "cases"),
          xlab = "",
          col = adjustcolor(colPal,  alpha.f=adjColorTransparent),
          space=spaces,
          horiz = TRUE,
          names.arg = rep("", length(colnames(toPlot))),
          xlim = c(ifelse(minimumBase>0.01, minimumBase-0.01, 0), min(maximumPrecisionConsideringNegative+0.1, maximumPrecisionConsideringNegative+((maximumPrecisionConsideringNegative-ifelse(minimumBase>0.01, minimumBase-0.01, 0))/5))),
          ylim = c(0,weightedCasesPerLabel[currentLabel]),
          axes = F,
          xpd = F
        )}, error = function(e) {

          print(e)
          message("Please check size of plotting region. It is too small. Alternatively: plot in pdf.")
          dev.off()
          return(NA)
        })

        axis(
          side = 2,
          at = p,
          labels = colnames(toPlot),
          tick = F,
          las=1
        )

        suppressWarnings(if(is.na(p)) return(NULL))
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

        #if only one attribute for all, a vector results
        if(is.null(nrow(relevantBins))){
          relevantBins = relevantBins[ncol]
        }else{
          relevantBins = relevantBins[ncol, ]
        }


        # Find textposition
        positions=rep(2, length(cumToPlot[, ncol]))

        widthOfLabels=strwidth(ifelse(
          relevantBins == 0,
          "",substr(
            relevantBins,
            start = nchar(colnames(precisionMatrix)) + 1,
            stop = 100000
          )))


        yPositions=rep(p[ncol], length(cumToPlot[, ncol]))
        i=1
        r=sapply(widthOfLabels, function(x){
          if(x>toPlot[i, ncol]){

            if(i>2){
              # Find last element with a label
              lastElement=i-1
              while(lastElement>0){
                if(widthOfLabels[lastElement]==0){
                  lastElement=lastElement-1
                }else{
                  break;
                }
              }

              if(yPositions[lastElement]<p[ncol]){
                yPositions[i]<<-p[ncol]+1/2*coverageMatrix[colnames(toPlot)[ncol], "coverageBin"]+max(strheight(relevantBins))
              }else{
                yPositions[i]<<-p[ncol]-1/2*coverageMatrix[colnames(toPlot)[ncol], "coverageBin"]-max(strheight(relevantBins))
              }
            }else if(i>1 && yPositions[i-1]<p[ncol]){
              yPositions[i]<<-p[ncol]+1/2*coverageMatrix[colnames(toPlot)[ncol], "coverageBin"]+max(strheight(relevantBins))

            }else{
              yPositions[i]<<-p[ncol]-1/2*coverageMatrix[colnames(toPlot)[ncol], "coverageBin"]-max(strheight(relevantBins))

            }
          }
          i<<-i+1
        })


        adaptedLabels= sapply(1:length(relevantBins), function(binNo){
          if(relevantBins[binNo]==0 || relevantBins[binNo] =="base") return("")
          if(yPositions[binNo]==p[ncol]) return(paste0(substr(
            relevantBins[binNo],
            start = nchar(colnames(precisionMatrix)[binNo]) + 1,
            stop = 100000
          ),"  "))

          if(yPositions[binNo]>p[ncol] && toPlot[, ncol][binNo]>0.001) return (paste(substr(
            relevantBins[binNo],
            start = nchar(colnames(precisionMatrix)[binNo]) + 1,
            stop = 100000
          ),"\U02C5 "))

          if(yPositions[binNo]>p[ncol]) return (paste(substr(
            relevantBins[binNo],
            start = nchar(colnames(precisionMatrix)[binNo]) + 1,
            stop = 100000
          ),"\U02C5"))

          if(toPlot[, ncol][binNo]>0.001) return (paste(substr(
            relevantBins[binNo],
            start = nchar(colnames(precisionMatrix)[binNo]) + 1,
            stop = 100000
          ),"\U02C4 "))

          return (paste(substr(
            relevantBins[binNo],
            start = nchar(colnames(precisionMatrix)[binNo]) + 1,
            stop = 100000
          ),"\U02C4"))
        })

        text(
          x = cumToPlot[, ncol],
          y = yPositions,
          labels = adaptedLabels,
          cex = 1,
          pos = positions,
          offset=-0.25,
          col=ifelse(yPositions==p[ncol], "black", colPal)
        )

        text(
          #x = cumToPlot[nrow(cumToPlot), ncol],
          x=maximumPrecisionConsideringNegative+0.0001,
          y = p[ncol],
          labels = paste("Cov:", coverageMatrix[colnames(cumToPlot)[ncol], "coverage"]),
          cex = 1,
          pos = 4
        )
      })



    }

  #Plot the legend

    par(mar = c(0,0,0,0))
    plot(NULL , xaxt = 'n', yaxt = 'n', bty = 'n', ylab = '', xlab = '', xlim = 0:1, ylim = 0:1)

    legend(
      "center",
      legend = featureNames,
      fill = adjustcolor(colPal,  alpha.f=adjColorTransparent),
      xpd = TRUE,
      inset = c(0, 0),
      ncol = min(length(featureNames), numberOfFeaturesInOneLegenRow),
      x.intersp = 0.3
    )

    if(!is.null(pdf)){ dev.off()}else{
    par(prevPar)
  }

  }

