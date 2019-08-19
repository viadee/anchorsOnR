#' Title
#'
#' @param df
#' @param bins
#'
#' @return
#' @export
#'
#' @examples
discretize.data.frame <- function(df, bins) {
  discretize.column <- function(x, bin) {
    return(sapply(x, function(v) provideBin(v, bin)))
  }

  for (i in 1:length(bins)) {
    bin <- bins[[i]]
    column <- df[, i]
    df.column <- discretize.column(column, bin)
    df[, i] <- df.column
  }

  return(df)
}

#' Creates an empty discretization which is also used as a default
#'
#' @param featureCount the number of features to create bins for
#'
#' @return an empty discretization which causes the
#' dataset to not get discretized
create.empty.discretization <- function(featureCount) {
  bins = lapply(seq(featureCount), function(feature) {
    bin <- list()
    bin$doDiscretize = F
    return(bin)
  })
  return(bins)
}

#' Helper function to build bins
#'
#' TODO what is categoriesPerBinList for?
#'
#' @param columnIndex the index to write the bin to
#' @param cuts the cuts to set
#' @param currentBins the currently used list of bins
#' @param categoriesPerBinList ???
#' @param right if upper border is to be included
#' @param disc true or null, if discretization is to be enabled for this column
#'
#' @return the new list of bins
buildBins <- function(columnIndex, cuts = NULL, currentBins = NULL,
                      categoriesPerBinList = NULL, right = F, disc = T) {
  if (is.null(currentBins)) {
    currentBins <- list()
  }
  currentBins[[columnIndex]] = list()
  currentBins[[columnIndex]]$doDiscretize = disc

  if (!disc)
    return(currentBins)

  if (!is.null(cuts)) {
    currentBins[[columnIndex]]$numeric = T
    currentBins[[columnIndex]]$cuts = cuts
    currentBins[[columnIndex]]$right = right
  } else {
    currentBins[[columnIndex]] = categoriesPerBinList
    currentBins[[columnIndex]]$numeric = F
  }

  return(currentBins)
}


#' Validates the passed discretization options and standardizes it
#'
#' @param bins the defined bins
#' @param length the number of features in the dataset exclusive of target
#'
#' @return the standardized bins
validate.bins <- function(bins, length) {
  checkmate::expect_list(bins)
  if (length(bins) != length)
    stop("There needs to be one bin defined for each element")
  for (i in 1:length(bins)) {
    bin <- bins[[i]]
    if (is.null(bin)) {
      stop("Indexes must range from 0 to length(features)")
    }


    if (is.numeric(bin)) {
      newBin <- list()
      if (length(bin) > 0) {
        newBin$numeric <- T
        newBin$cuts <- bin
      } else {
        newBin$doDiscretize = F
      }
      bin <- newBin
    } else if (is.list(bin) && is.null(names(bin)) && length(bin) >= 1) {
      newBin <- list()
      newBin$numeric <- F
      newBin$classes <- bin
      bin <- newBin
    }


    if (length(which(!(
      names(bin) %in% c("doDiscretize", "numeric", "classes", "cuts", "right")
    ))) > 0)
      stop("Invalid bin arguments")
    if (!is.null(bin$doDiscretize) && bin$doDiscretize == F) {
      bins[[i]] <- bin
      next
    }

    if (is.null(bin$numeric)) {
      if ((is.null(bin$cuts) && is.null(bin$classes)) ||
          (!is.null(bin$cuts) && !is.null(bin$classes))) {
        stop("Either classes or cuts have to be provided")
      }

      if (!is.null(bin$cuts))
        bin$numeric <- T
      if (!is.null(bin$classes))
        bin$numeric <- F
    }

    if (is.null(bin$right) && bin$numeric == T)
      bin$right <- F

    if (bin$numeric == T) {
      checkmate::assert_vector(bin$cuts)
      checkmate::assert_null(bin$classes)
    } else {
      checkmate::assert_list(bin$classes)
      checkmate::assert_null(bin$cuts)
      checkmate::assert_null(bin$right)
    }

    bins[[i]] <- bin
  }
  return(bins)
}


#' Discretizes an undiscretized value given a discretization option
#'
#' @param value the value to be discretized
#' @param bin the bin defining how the value is to be discretized
#'
#' @return the discretized value
provideBin <- function(value, bin) {
  # If discretization is disabled return value
  if (!is.null(bin$doDiscretize) && !bin$doDiscretize) {
    return(value)
  }

  if (is.data.frame(value)) {
    value <- value[[1]]
  }

  if (!bin$numeric) {
    i = 1
    while (!(value %in% bin$classes[[i]])) {
      if (is.null(bin$classes[[i]]))
        stop("All categorical classes need to be specified")
      i = i + 1
    }
    return(i)
  }
  else {
    i = 1
    while (i <= length(bin$cuts) &&
           ((bin$right && value > bin$cuts[i]) ||
            (!bin$right &&
             value >= bin$cuts[i]))) {
      i = i + 1
    }
    return(i)
  }
}

#' Checks if the specified undiscretized value is included in the bin
#'
#' TODO I don't really know what this function is there for and at first glance it looks buggy to me
#'
#' @param value the undiscretized value
#' @param bin the discretization options
#' @param binIndex the bin index in the list
#'
#' @return True, if value lies within specified discretization option
checkBin <- function(value, bin, binIndex) {
  if (!bin$numeric) {
    if (value %in% bin[[binIndex]]) {
      return(T)
    } else{
      return(F)
    }
  }
  else {
    if (binIndex == 1) {
      if ((bin$right &&
           value <= bin$cuts[binIndex]) ||
          (!bin$right && value < bin$cuts[binIndex])) {
        return(T)
      } else{
        return(F)
      }
    }

    if (binIndex > length(bin$cuts)) {
      if ((bin$right &&
           value > bin$cuts[binIndex - 1]) ||
          (!bin$right && value >= bin$cuts[binIndex - 1])) {
        return(T)
      } else{
        return(F)
      }
    }

    if ((bin$right &&
         value > bin$cuts[binIndex - 1] && value <= bin$cuts[binIndex]) ||
        (!bin$right &&
         value >= bin$cuts[binIndex - 1] &&
         value < bin$cuts[binIndex])) {
      return (T)
    } else{
      return(F)
    }
  }
}


#' Builds a printable representation of the discretization used for output formatting
#'
#' TODO remove cuts and right as they are included in bin. Also, include non-numeric discretization usungusing $classes
#'
#' @param bin the discretization option
#' @param cuts the cuts
#' @param right right
#' @param short short
#'
#' @return the readable description
buildDescription <- function(bin, cuts, right, short) {
  desc = ""
  if (bin == 1) {
    if (right) {
      desc = paste0("<=", ifelse(short, round(cuts[1], 2), cuts[1]))
    } else{
      desc = paste0("<", ifelse(short, round(cuts[1], 2), cuts[1]))
    }
  } else if (bin == length(cuts) + 1) {
    if (right) {
      desc = paste0(">", ifelse(short, round(cuts[length(cuts)], 2), cuts[length(cuts)]))
    } else{
      desc = paste0(">=", ifelse(short, round(cuts[length(cuts)], 2), cuts[length(cuts)]))
    }
  } else {
    if (right) {
      desc = paste0("(",
                    ifelse(short, round(cuts[bin - 1], 2), cuts[bin - 1]),
                    ",",
                    ifelse(short, round(cuts[bin], 2), cuts[bin]),
                    "]")
    } else{
      desc = paste0("[",
                    ifelse(short, round(cuts[bin - 1], 2), cuts[bin - 1]),
                    ",",
                    ifelse(short, round(cuts[bin], 2), cuts[bin]),
                    ")")
    }
  }

  return(desc)

}
