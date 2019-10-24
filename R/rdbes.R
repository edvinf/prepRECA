#
# Functions for dealing with rdbes data format
#

#' Extracts measurements from a BV table
#' @description
#'  Reshapes BV table into a data.table with each row corresponding to one measured specimen,
#'  and the different BV types introduced as individual columns (e.g. age, length weight).
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
    if (length(unique(BVt$BVunitVal))>1){
      stop(paste("Can not extract ", t, "as it is recorded with heterogenous BVunitVal"))
    }
    if (length(unique(BVt$BVunitRefList))>1){
      stop(paste("Can not extract ", t, "as it is recorded with heterogenous BVunitRefList"))
    }
    if (any(duplicated(paste(BVt$BVfishID, BVt$SAid, BVt$FMid)))){
      stop(paste("Can not extract ", t, "as it has duplicate registration for some BVfishID"))
    }
    BVt <- BVt[,c("SAid", "FMid", "BVfishID", "BVvalue")]
    names(BVt) <- c("SAid", "FMid", "BVfishID", t)

    extraction <- merge(extraction, BVt, all=T)

  }

  return(extraction)
}


#' Make SApres plot
#' @noRd
makeSApresPlot <- function(samples){
  pres <- table(samples$SA$SApres)
  pres <- data.table::data.table(pres)
  names(pres) <- c("SApres", "count")
  SApres <- ggplot(pres, aes(x="", y=count, fill=SApres))+
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y") +
    theme_minimal() + theme(axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            panel.border = element_blank())
  return(SApres)
}

makeLowHplot <- function(samples){
  lowh <- table(samples$SA$SAlowHierarchy)
  lowh <- data.table::data.table(lowh)
  names(lowh) <- c("SAlowHierarchy", "count")
  SAlowHierarchy <- ggplot(lowh, aes(x="", y=count, fill=SAlowHierarchy))+
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y") +
    theme_minimal() + theme(axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            panel.border = element_blank())
  return(SAlowHierarchy)
}

makeLengthUnitPlot <- function(sample){
  lengths <- sample$BV[sample$BV$BVtype=="Length"]
  units <- table(lengths$BVunitVal)
  units <- data.table::data.table(units)
  names(units) <- c("lengthUnit", "count")
  lengthunits <- ggplot(units, aes(x="", y=count, fill=lengthUnit))+
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y") +
    theme_minimal() + theme(axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            panel.border = element_blank())
  return(lengthunits)
}

makeWeightUnitPlot <- function(sample){
  lengths <- sample$BV[sample$BV$BVtype=="Weight"]
  units <- table(lengths$BVunitVal)
  units <- data.table::data.table(units)
  names(units) <- c("weightUnit", "count")
  weightunits <- ggplot(units, aes(x="", y=count, fill=weightUnit))+
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y") +
    theme_minimal() + theme(axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            panel.border = element_blank())
  return(weightunits)
}

#' Plots a summary of samples
#' @description
#'  Plots a paneled plot to reveal heterogeneity i key sampling parameters, important for R-ECA.
#' @param samples named list() of data.table() members, each named with the RDBES record type, and with column names following the RDBES (v 1.17) R-Name specification
#' @examples
#' sampleSummaryPlot(NORportsampling2018)
#' @export
sampleSummaryPlot <- function(samples){
  requireNamespace("ggplot2")
  if (!("SA") %in% names(samples)){
    stop("No SA table found")
  }

  SApres <- makeSApresPlot(samples)
  SAlowH <- makeLowHplot(samples)
  BVlengthUnit <- makeLengthUnitPlot(samples)
  BVweightUnit <- makeWeightUnitPlot(samples)

  gridExtra::grid.arrange(SApres, SAlowH, BVlengthUnit, BVweightUnit, ncol=2)
}
#PLots
#sample composition etc

