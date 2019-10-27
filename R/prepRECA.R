
# convert codes, coherent in landings and samples

# return ECA object, and mapping between codes.

#' Check that all fixed effect combinations are sampled
#' @noRd
checkAllSampled <- function(landings, samples, fixedeffects){
  if (is.null(fixedeffects) | length(fixedeffects) == 0){
    return(T)
  }

  landingsfixed <- landings[[fixedeffects[1]]]
  samplesfixed <- samples[[fixedeffects[1]]]

  if (length(fixedeffects) > 1){
    for (f in fixedeffects[2:length(fixedeffects)]){
      landingsfixed <- paste(landingsfixed, landings[f], sep="/")
      samplesfixed <- paste(samplesfixed, samples[f], sep="/")
    }
  }

  return(all(samplesfixed %in% landingsfixed))
}

#' Check that all fixed effects are sampled in combination with careffect or neighbout
#' @noRd
checkAllSampledCar <- function(landings, samples, fixedeffects, careffect, neighbours){

  for (l in landings[[careffect]]){
    landcar <- landings[landings[[careffect]] == l,]
    sampcar <- samples[samples[[careffect]] %in% c(l, neighbours[l]),]
    sampcar[[careffect]] <- l
    if (!checkAllSampled(landcar, sampcar, c(fixedeffects, careffect))){
      return(F)
    }
  }

  return(T)
}

#' @noRd
getCovariateMap <- function(covariate, samples, landings){

  if (!(covariate %in% names(samples))){
    stop(paste("Covariate", covariate, "not in samples"))
  }

  values <- unique(samples[[covariate]])

  if (covariate %in% names(landings)){
    values <- unique(c(values, landings[[covariate]]))
  }

  map <- list()
  i <- 1
  for (v in values){
    map[i] <- v
    i <- i + 1
  }

  return(map)
}

#' Prepare data for R-ECA
#' @description
#'  Checks and reformats data so that it can be fed to R-ECA.
#' @details
#'  The cell definition is specified by 'landings'.
#'  The type of covariates are specified in fixedeffects, randomeffects and careffect.
#'  All fixed effects, as well as any car-effect, must be included in the cell definition.
#'  All covariates must occur in samples.
#'
#'  midseason is used for calculating a fractional age of fish at harvest.
#'  Whenever a categorical temporal covariate is included it is typically set to a time in the middle of each value of the covariate.
#'  Consider a model configuration where month is included as a covariate.
#'  For each cell containing January, midseason would naturally be defined as 15.5/365 for a non-leap year.
#'
#'  neighbours must be symetric, so that b %in% neighbours[a], implies a %in% neighbours[b]
#'
#' @param samples data.table() with samples, each row corresponding to one sampled fish. Contains columns:
#'  \describe{
#'   \item{catchId}{Column identifying the catch that the sample was taken from. Typically a haul or a landing.}
#'   \item{sampleId}{Column identifying the sample. If only one sample is taken for each catch. This can be set equal to catchId}
#'   \item{Age}{Age of fish. Must be complete (no NAs)}
#'   \item{Length}{Length of fish. Must be complete (no NAs)}
#'   \item{Weight}{Weight of fish. Fish with missing values will not be included in Weight-given-length model.}
#'   \item{...}{Additional columns to be used as covariates. Must at least be all covariates in 'landings'. Type of covariate must be sepcified in 'fixedeffects', 'randomeffects' or 'careffect'}
#'  }
#' @param landings data.table() with total landings, each row corresponding to one cell. Contains columns:
#' \describe{
#'  \item{LiveWeightKG}{numeric(). Total landings (Live/Round weight in Kg) for the cell}
#'  \item{midseason}{numeric(). The temporal location of the cell, expressed as fraction of a year.}
#'  \item{...}{Additional columns to be used as covariates. These define each cell. Type of covariate must be sepcified in 'fixedeffects', 'randomeffects' or 'careffect'}
#' }
#' @param fixedeffects character() vector specifying fixed effects. Corresponding columns must exists in samples and landings.
#' @param randomeffects character() vector specifying random effects. Corresponding columns must exists samples (may also exist in landings).
#' @param careffect character() specifying a random effect with conditional autoregressive coefficient. Corresponding columns must exists samples (may also exist in landings).
#' @param neighbours list() specifying the neighbourhood-structure for the careffect. neighbours[a] should provide a vector of neighbours to a. May be NULL of no careffect is used.
#' @return list() with elements:
#' \describe{
#'  \item{AgeLength}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}}
#'  \item{WeightLength}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}}
#'  \item{Landings}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}}
#'  \item{CodeMap}{Mapping of values for each covariate in samples and landings to integer value used in R-ECA.}
#' }
#' @export
prepRECA <- function(samples, landings, fixedeffects, randomeffects, careffect, neighbours=NULL){
  # check mandatory columns
  if (!(all(c("LiveWeightKG", "midseason") %in% names(landings)))){
    stop("Columns LiveWeightKG and midseason are mandatory in landings")
  }
  if (!(all(c("catchId", "sampleId", "Age", "Length", "Weight") %in% names(samples)))){
    stop("Columns, catchId, sampleId, Age, Length, and Weight are mandatory in samples")
  }

  # check for NAs
  if (!all(!is.na(samples[,!"Weight"]))){
    stop("NAs are only allowed for weight in samples")
  }
  if(!all(!is.na(landings))){
    stop("NAs in landings")
  }

  #check different effect types
  if (!is.null(fixedeffects) & length(fixedeffects) > 0){
    if (!all(fixedeffects %in% names(samples))){
      stop(paste("Data missing for fixed effects (samples):", paste(fixedeffects[!(fixedeffects %in% names(samples))], collapse=",")))
    }
    if (!all(fixedeffects %in% names(landings))){
      stop(paste("Data missing for fixed effects (landings):", paste(fixedeffects[!(fixedeffects %in% names(landings))], collapse=",")))
    }
    if (!checkAllSampled(landings, samples, fixedeffects)){
      stop("Not all combinations of fixed effects are sampled")
    }
  }
  if (!is.null(careffect)){
    if (!(careffect %in% names(samples))){
      stop(paste("Data missing for CAR effect (samples):", careffect))
    }
    if (!(careffect %in% names(landings))){
      stop(paste("Data missing for CAR effect (landings):", careffect))
    }
    if (is.null(neighbours)){
      stop("CAR effect specified, but no neighbours provided.")
    }

    for (l in names(neighbours)){
      for (n in neighbours[l]){
        if (!(n %in% names(neighbours) | l %in% neighbours[n]))
          stop(paste("neighbours not symmetric wrp",l,n))
      }
    }

    if (!checkAllSampledCar(landings, samples, fixedeffects, careffect, neighbours)){
      stop("Not all combinations of fixed effects are sampled together with CAR effect or neighbours")
    }
  }
  if (!is.null(randomeffects) & length(randomeffects) > 0){
    if (!all(randomeffects %in% names(samples))){
      stop(paste("Data missing for random effects (samples):", paste(randomeffects[!(randomeffects %in% names(samples))], collapse=",")))
    }
  }

  #check that all covariates are specified
  if(!(all(names(samples) %in% c(fixedeffects, randomeffects, careffect, c("catchId", "sampleId", "Age", "Length", "Weight"))))){
    stop(paste("Effect not specified for covariates:", paste(names(samples)[!(names(samples) %in% c(fixedeffects, randomeffects, careffect, c("catchId", "Age", "Length", "Weight")))], collapse=",")))
  }
  if(!(all(names(landings) %in% c(fixedeffects, randomeffects, careffect, c("LiveWeightKG", "midseason"))))){
    stop(paste("Effect not specified for covariates:", paste(names(landings)[!(names(landings) %in% c(fixedeffects, randomeffects, careffect, c("LiveWeightKG", "midseason")))], collapse=",")))
  }

  covariateMaps <- list()
  for (f in c(fixedeffects, randomeffects, careffect)){
    covariateMaps[[f]] <- getCovariateMap(f, samples, landings)
  }

  # figure out how to deal with delprøve
  # - Add sample ID to input.
  # - Add optional argument for partcount

  # build eca objects

  # info matrix: interaction and in.landings based on landings

}

#' Data report for R-ECA preparation
#' @description
#'  Generates overview of samples to inform on sample availability in potential cell definitions.
#'  Informs on which covariates may be fixed effects, and how grouping of covariates is best done for random effects.
#'
#'  The columns in landings define the cells.
#'  For each cell the total landed weight is reported, along with the number of unique occurances of covariates not in landings (including Catchsample).
#'  Lastly the number of fish measurements for Age, Weight and Length is reported.
#'
#' @param samples data.table() with samples, each row corresponding to one sampled fish. Contains columns:
#'  \describe{
#'   \item{catchId}{Column identifying the catch that the sample was taken from. Typically a haul or a landing.}
#'   \item{sampleId}{Column identifying the sample. If only one sample is taken for each catch. This can be set equal to catchId}
#'   \item{Age}{Age of fish. Must be complete (no NAs)}
#'   \item{Length}{Length of fish. Must be complete (no NAs)}
#'   \item{Weight}{Weight of fish. Fish with missing values will not be included in Weight-given-length model.}
#'   \item{...}{Additional columns to be used as covariates. Must at least be all covariates in 'landings'.}
#'  }
#'
#' @param landings data.table() with total landings, each row corresponding to one cell. Contains columns:
#' \describe{
#'  \item{LiveWeightKG}{numeric(). Total landings (Live/Round weight in Kg) for the cell}
#'  \item{<midseason>}{optional for this function. numeric() The temporal location of the cell, expressed as fraction of a year.}
#'  \item{...}{Additional columns to be used as covariates. These define each cell.}
#' }
#' @return data.table() with columns
#' \describe{
#'  \item{<Covariates in landings>}{one column for each. Defines the cells.}
#'  \item{LiveWeightKG}{The total weight (kg) in the cell.}
#'  \item{LiveWeightCumFraction}{The fraction of landings in this cell AND all the cells with higher total weight than this cell.}
#'  \item{<Count of covariates not in landings>}{Count of unique values for covariate. one column for each. Column name is covariate name (from samples) prefixed with n}
#'  \item{nCatchsample}{The number of unique catches sampled in the cell.}
#'  \item{nSample}{The number of unique catch-samples in the cell.}
#'  \item{nAge}{The number of age readings in the cell.}
#'  \item{nWeight}{The number of fish weight measurements in the cell.}
#'  \item{nLength}{The number of fish length measurements in the cell.}
#' }
#' @export
rEcaDataReport <- function(samples, landings){
  # check mandatory columns
  if (!(all(c("LiveWeightKG") %in% names(landings)))){
    stop("Columns LiveWeightKG and midseason are mandatory in landings")
  }
  if (!(all(c("catchId", "sampleId", "Age", "Length", "Weight") %in% names(samples)))){
    stop(paste("Columns, catchId, sampleId, Age, Length, and Weight are mandatory in samples. Missing:", paste(c("catchId", "sampleId", "Age", "Length", "Weight")[(!c("catchId", "sampleId", "Age", "Length", "Weight") %in% names(samples))])))
  }

  # check for NAs
  if (!all(!is.na(samples[,!"Weight"]))){
    stop("NAs are only allowed for weight in samples")
  }
  if(!all(!is.na(landings))){
    stop("NAs in landings")
  }

  inlandings <- names(landings)
  inlandings <- inlandings[!(inlandings %in% c("LiveWeightKG", "midseason"))]

  insamples <- names(samples)
  insamples <- insamples[!(insamples %in% c("catchId", "sampleId", "Age", "Length", "Weight"))]

  if (length(inlandings) == 0){
    stop("No covariates in landings. Cannot produce report.")
  }

  if (!all(insamples %in% inlandings)){
    stop(paste("All covariates in landings must also be in samples. Missing", paste(inlandings[!(inlandings %in% insamples)])))
  }

  agglist <- list()
  for (l in inlandings){
    agglist[[l]] <- samples[[l]]
  }

  agglistLand <- list()
  for (l in inlandings){
    agglistLand[[l]] <- landings[[l]]
  }

  # sampled units (count unique)
  nCs <- aggregate(list(nCatch=samples$catchId), by=agglist, FUN=function(x){length(unique(x))})
  # samples units (count unique)
  nSs <- aggregate(list(nSample=samples$sampleId), by=agglist, FUN=function(x){length(unique(x))})

  # fish parameters, count rows
  nAges <- aggregate(list(nAge=samples$Age), by=agglist, FUN=length)
  nWeight <- aggregate(list(nWeight=samples$Weight), by=agglist, FUN=length)
  nLength <- aggregate(list(nLength=samples$Length), by=agglist, FUN=length)

  # total weights, sum
  kgLanded <- aggregate(list(LiveWeightKG=landings$LiveWeightKG), by=agglistLand, FUN=sum)
  kgLanded <- kgLanded[order(kgLanded$LiveWeightKG, decreasing=T),]
  kgLanded$LiveWeightCumFraction <- cumsum(kgLanded$LiveWeightKG) / sum(kgLanded$LiveWeightKG)

  out <- merge(kgLanded, nCs, all.x=T)
  out <- merge(out, nSs, all.x=T)

  # covariates (count unique)
  for (s in insamples[!(insamples %in% inlandings)]){
    l <- list(t=samples[[s]])
    names(l) <- paste("n",s)
    agg <- aggregate(l, agglist, FUN=function(x){length(unique(x))})
    out <- merge(out, agg, all.x=T)
  }

  out <- merge(out, nAges, all.x=T)
  out <- merge(out, nWeight, all.x=T)
  out <- merge(out, nLength, all.x=T)

  stopifnot(all(!is.na(out$LiveWeightKG)))
  # NAs come from cells not present in samples
  out[is.na(out)]<-0
  out <- out[order(out$LiveWeightKG, decreasing = T),]
  out <- as.data.table(out)
  return(out)
}
