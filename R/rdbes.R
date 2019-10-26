#
# Functions for dealing with rdbes data format
#

#' Extracts measurements from a BV table
#' @description
#'  Reshapes BV table into a data.table with each row corresponding to one measured specimen,
#'  and the different BV types introduced as individual columns (e.g. age, length weight).
#'  @details
#'  Checks and halts for heterogenity in length units, weight untits, length measurements, or BVstratification
#' @param BVtable data.table() with column names following the RDBES (v 1.17) R-Name specification
#' @param BVtypes a character() vector identifying the BVtypes to introduce as columns (other record types are discarded)
#' @return data.table()
#' @examples
#' extractBV(NORportsampling2018$BV, c("Age", "Length", "Weight"))
#' @export
extractBV <- function(BVtable, BVtypes){
  if (!all(BVtypes %in% BVtable$BVtype)){
    stop(paste("Not all BVtypes found in data (",paste(BVtypes[!(BVtypes %in% BVtable$BVtype)], collapse = ",")), ")")
  }

  extraction <- unique(BVtable[,c("SAid", "FMid", "BVfishID")])

  for (t in BVtypes){
    BVt <- BVtable[BVtable$BVtype == t,]
    if (length(unique(BVt$BVunitVal)) > 1){
      stop(paste("Can not extract ", t, "as it is recorded with heterogenous BVunitVal"))
    }
    if (length(unique(BVt$BVunitRefList)) > 1){
      stop(paste("Can not extract ", t, "as it is recorded with heterogenous BVunitRefList"))
    }
    if (any(duplicated(paste(BVt$BVfishID, BVt$SAid, BVt$FMid)))){
      stop(paste("Can not extract ", t, "as it has duplicate registration for some BVfishID"))
    }
    if (length(unique((BVt$BVstratum))) > 1){
      stop(paste("Can not extract ", t, "as it has stratified selection for some BVfishID"))
    }
    BVt <- BVt[,c("SAid", "FMid", "BVfishID", "BVvalue")]
    names(BVt) <- c("SAid", "FMid", "BVfishID", t)

    extraction <- merge(extraction, BVt, all=T)

  }

  return(extraction)
}

#' Produce warnings for R-ECA
#' @description
#'  Checks for common concerns when configuring R-ECA
#' @details
#'  Checks for heterogenity in fish presentation
#'  Checks for stratification
#'  Checks for multiple sampling programs
#' @param flatRDBES data.table() with column names following the RDBES (v 1.17) R-Name specification.
#' @examples
#' warningsRecaApplicability(NORportsampling2018$SA)
#' @export
warningsRecaApplicability <- function(flatRDBES){

  if (!is.data.frame(flatRDBES)){ #true also for data.table
    stop("checks can only be applied to data frames")
  }

  if ("SApres" %in% names(flatRDBES)){
    if (length(unique(flatRDBES$SApres)) > 1){
      warning(paste("Sample contains several presentations, SApres:"), paste(unique(flatRDBES$SApres), collapse=","))
    }
  }

  stratificationColumns <- names(flatRDBES)[substr(names(flatRDBES),3,9) == "stratum"]
  for (s in stratificationColumns){
    if (length(unique(flatRDBES[[s]])) > 1){
      warning(paste("Sample contains stratified selections, ", s, ": ", paste(unique(flatRDBES[[s]]), collapse = ","), sep=""))
    }
  }

  clusteringColumns <- names(flatRDBES)[substr(names(flatRDBES),3,13) == "clusterName"]
  for (s in stratificationColumns){
    if (length(unique(flatRDBES[[s]])) > 1){
      warning(paste("Sample contains clustered selections, ", s, ": ", paste(unique(flatRDBES[[s]]), collapse = ","), sep=""))
    }
  }

  selectionColumns <- names(flattenRDBES)[substr(names(flatRDBES),3,12) == "selectMeth"]
  for (s in selectionColumns){
    if (s %in% c("UPSWR", "UPSWOR")){
      warning(paste("Sample contains unequal probability selection methods (", s, ")"))
    }
  }

  if ("DEid" %in% flatRDBES){
    if (length(unique(flatRDBES$DEid)) > 1){
      warning(paste("Sample contains several sampling scehmes:", paste(unique(flatRDBES$DEid), collapse = ",")))
    }
  }
}

#' Produce warnings for incomplete data
#' @description
#'  Produce warnings for columns with NA values, listing the number of NA values.
#' @param datatable data.table() to be checked for completeness
#' @param variables character() vector with variables (column names of datatable) to check for completeness
#' @export
warningsDataCompleteness <- function(datatable, variables){
  for (v in variables){
    nas <- sum(is.na(datatable[[v]]))
    if (nas > 0){
      warning(paste(nas, "NAs out of", nrow(datatable), "records, for variable", v))
    }
  }
}


#' Missing data histograms
#' @description Plots the number and fraction of missing data pr sample (SAid)
#' @param flatRDBES flatRDBES data.table() with column names following the RDBES (v 1.17) R-Name specification.
#' @param var variable to plot missing data for (column in flatRDBES)
#' @example
#'  ages <- extractBV(NORportsampling2018$BV, c("Age"))
#'  agesamples <- merge(ages, NORportsampling2018$SA, by="SAid")
#'  plotSAnas(agesamples, c("Age"))
plotSAnas <- function(flatRDBES, var){
  stopifnot("SAid" %in% names(flatRDBES))
  stopifnot(var %in% names(flatRDBES))

  missingPrSample <- aggregate(list(missing=flatRDBES[[var]]), by=list(SAid=flatRDBES$SAid), FUN=function(x){sum(is.na(x))})
  missingPrSample <- missingPrSample[missingPrSample$missing>0,]
  missing <- ggplot(missingPrSample, aes(x=missing)) +
    geom_histogram(binwidth = 1) +
    labs(title="Missing data",x=paste(var, "missing"), y = "# samples (SAid)")

  fractionPrSample <- aggregate(list(fraction=flatRDBES[[var]]), by=list(SAid=flatRDBES$SAid), FUN=function(x){100*sum(is.na(x))/length(x)})
  fractionPrSample <- fractionPrSample[fractionPrSample$fraction>0,]
  missingFrac <- ggplot(fractionPrSample, aes(x=fraction)) +
    geom_histogram(binwidth = 5) +
    labs(title="Missing data",x=paste(var, "missing (%)"), y = "# samples (SAid)")

  gridExtra::grid.arrange(missing, missingFrac, nrow=1)
}
