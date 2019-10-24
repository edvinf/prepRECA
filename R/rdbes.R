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
    if (length(unique((BVt$BVstratification))) > 1){
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
      warning(paste("Sample contains several presentations, SApres:"), paste(unique(flatRDBES$SApres)), collapse=",")
    }
  }

  stratificationColumns <- names(flatRDBES)[names(flatRDBES) == "stratification"]
  for (s in stratificationColumns){
    if (length(unique(flatRDBES[s])) > 1){
      warning(paste("Sample contains stratified selections, ", s, ": ", paste(unique(flatRDBES[s]), collapse = ","), sep=""))
    }
  }

  if ("DEid" %in% flatRDBES){
    if (length(unique(flatRDBES$DEid)) > 1){
      warning(paste("Sample contains several sampling scehmes:", paste(unique(flatRDBES$DEid), collapse = ",")))
    }
  }
}

