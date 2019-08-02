listPerturbTypes = function ()
{
  c("tabular", "text", "image", "audio", "video")
}

cleanupPackageNames = function(pkgs) {
  stri_replace_all(pkgs, "", regex = "^[!_]")
}

create.empty.discretization <- function(featureCount) {
  bins = list()
  for (feature in 1:featureCount) {
    bin <- list()
    bin$doDiscretize = F
    bins[[feature]] <- bin
  }
  return(bins)
}

validate.bins <- function(bins, length) {
  checkmate::expect_list(bins)
  if (length(bins) != length)
    stop("There needs to be one bin defined for each element")
  for (bin in bins) {
    if (length(which(!(
      names(bin) %in% c("doDiscretize", "numeric", "classes", "cuts", "right")
    ))) > 0)
      stop("Invalid bin arguments")
    if (!is.null(bin$doDiscretize) && bin$doDiscretize == F)
      next

    if (!is.null(bin$numeric) && bin$numeric == T) {
      checkmate::assert_vector(bin$cuts)
      checkmate::assert_null(bin$classes)
    } else {
      checkmate::assert_list(bin$classes)
      checkmate::assert_null(bin$cuts)
      checkmate::assert_null(bin$right)
    }
  }
}


provideBin.numeric <- function(value, bin) {
  # If discretization is disabled return value
  if (!is.null(bin$doDiscretize) && !bin$doDiscretize) {
    return(value)
  }

  if (!bin$numeric) {
    i = 1
    while (!(value %in% bin[[i]])) {
      i = i + 1
    }
    if (value %in% bin[[i]])
      return(i)
    stop("All categorical classes need to be specified")
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


buildDescription.numeric <- function(bin, cuts, right) {
  desc = ""
  if (bin == 1) {
    if (right) {
      desc = paste0("<=", cuts[1])
    } else{
      desc = paste0("<", cuts[1])
    }
  } else if (bin == length(cuts) + 1) {
    if (right) {
      desc = paste0(">", cuts[length(cuts)])
    } else{
      desc = paste0(">=", cuts[length(cuts)])
    }
  } else {
    if (right) {
      desc = paste0("(", cuts[bin - 1], ",", cuts[bin], "]")
    } else{
      desc = paste0("[", cuts[bin - 1], ",", cuts[bin], ")")
    }
  }

  return(desc)
}
