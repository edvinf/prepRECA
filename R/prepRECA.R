
# convert codes, coherent in landings and samples

# return ECA object, and mapping between codes.

#' Check that all fixed effect combinations are sampled
#' @noRd
checkAllSampled <- function(landings, samples, fixedEffects){
  if (is.null(fixedEffects) | length(fixedEffects) == 0){
    return(T)
  }

  landingsfixed <- landings[[fixedEffects[1]]]
  samplesfixed <- samples[[fixedEffects[1]]]

  if (length(fixedEffects) > 1){
    for (f in fixedEffects[2:length(fixedEffects)]){
      landingsfixed <- paste(landingsfixed, landings[f], sep="/")
      samplesfixed <- paste(samplesfixed, samples[f], sep="/")
    }
  }

  return(all(samplesfixed %in% landingsfixed))
}

#' Check that all fixed effects are sampled in combination with carEffect or neighbout
#' @noRd
checkAllSampledCar <- function(landings, samples, fixedEffects, carEffect, neighbours){

  for (l in landings[[carEffect]]){
    landcar <- landings[landings[[carEffect]] == l,]
    sampcar <- samples[samples[[carEffect]] %in% c(l, neighbours[l]),]
    sampcar[[carEffect]] <- l
    if (!checkAllSampled(landcar, sampcar, c(fixedEffects, carEffect))){
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

#' @noRd
getInfoMatrix <- function(samples, landings, fixedEffects, randomEffects, carEffect){
  info <- matrix(ncol=7, nrow=length(c(fixedEffects, randomEffects, carEffect))+1)
  colnames(info) <- c("random", "CAR", "continous", "in.landings", "nlev", "interaction", "in.slopeModel")
  rownames(info) <- c("constant", c(fixedEffects, randomEffects, carEffect))
  info[1,] <- c(0,0,0,0,1,0,0)
  i <- 2
  if (!is.null(fixedEffects) & length(fixedEffects) > 0){
    for (e in fixedEffects){
      inl <- 0
      if (e %in% names(landings)){
        inl <- 1
      }
      info[i,] <- c(0,0,0,inl,length(unique(c(samples[[e]], landings[[e]]))), inl, 0)
      i <- i+1
    }
  }
  if (!is.null(randomEffects) & length(randomEffects) > 0){
    for (e in randomEffects){
      inl <- 0
      if (e %in% names(landings)){
        inl <- 1
      }
      info[i,] <- c(1,0,0,inl,length(unique(c(samples[[e]], landings[[e]]))), inl, 0)
      i <- i+1
    }
  }
  if (!is.null(carEffect)){
    inl <- 0
    if (carEffect %in% names(landings)){
      inl <- 1
    }
    info[i,] <- c(1,1,0,inl,length(unique(c(samples[[e]], landings[[e]]))), inl, 0)
    i <- i+1
  }
  return(info)
}

#' Run before renaming columns
#' @noRd
addPartCount <- function(DataMatrix, nFish){

  # renumber sampleID to delprøve convention (needed ?)
  DataMatrix$partcount <- NA
  partsamples <- aggregate(list(nSampleId=DataMatrix$sampleId), by=list(catchId=DataMatrix$catchId), FUN=function(x){length(unique(x))})
  partsamples <- partsamples[partsamples$nSampleId > 1,]
  partsamples <- merge(partsamples, unique(DataMatrix[,c("sampleId", "catchId")]))

  if (nrow(partsamples) > 0){
    if (is.null(nFish)){
      stop(paste("Some cacthes are sampled several times, but argument 'nFish' not given."))
    }
    if (!all(partsamples$sampleId %in% nFish$sampleId)){
      stop(paste("Some catches are sampled several times, but corresponding catchId not in nFish:", paste(partsamples$sampleId[!(partsamples$sampleId %in% nFish$catchId)], collapse=",")))
    }
    nFish$partcount <- nFish$count
    nFish$count <- NULL
    DataMatrix <- merge(DataMatrix, nFish, by="sampleId")
  }
  return(DataMatrix)
}

#' @noRd
addSamplingId <- function(DataMatrix){
  mapping <- data.table(catchId=unique(DataMatrix$catchId), samplingID=seq(1,length(unique(DataMatrix$catchId))))
  return(data.table::as.data.table(merge(DataMatrix, mapping)))
}

#' @noRd
formatCatchIdMap <- function(catchidmap){
  catchidmap <- catchidmap[order(catchidmap$samplingID),]
  map <- list()
  for (i in 1:nrow(catchidmap)){
    stopifnot(catchidmap[i,"samplingID"] == i)
    map[[i]] <- catchidmap[i,"catchId"]
  }
  return(map)
}

#' Preps data matix, sorts by catchID
#' then renames and recodes (preserving order)
#' @noRd
getDataMatrixAgeLength <- function(samples, nFish=NULL){

  DataMatrix <- samples[,c("catchId", "sampleId", "date", "Age", "Length", "Weight")]
  DataMatrix$date <- (as.numeric(strftime(DataMatrix$date, "%j"))+1)/366
  DataMatrix <- DataMatrix[,c("Age", "date", "Length", "catchId", "sampleId")]
  DataMatrix <- addPartCount(DataMatrix, nFish)
  names(DataMatrix) <- c("age", "part.year", "lengthCM", "catchId", "partnumber", "partcount")
  DataMatrix <- addSamplingId(DataMatrix)
  DataMatrix <- DataMatrix[order(DataMatrix$catchId),]

  catchidMap <- unique(DataMatrix[,c("catchId", "samplingID")])

  DataMatrix <- DataMatrix [,c("age", "part.year", "lengthCM", "samplingID", "partnumber", "partcount")]

  ret <- list()
  ret$DataMatrix <- DataMatrix
  ret$catchIdMap <- formatCatchIdMap(catchidMap)


  return(ret)
}

#' Preps data matix, sorts by catchID
#' then renames and recodes (preserving order)
#' @noRd
getDataMatrixWeightLength <- function(samples, nFish=NULL){

  DataMatrix <- samples[,c("catchId", "sampleId", "Age", "Length", "Weight")]
  DataMatrix <- DataMatrix[,c("Weight", "Length", "catchId", "sampleId")]
  DataMatrix <- addPartCount(DataMatrix, nFish)
  names(DataMatrix) <- c("weightKG",  "lengthCM", "catchId", "partnumber", "partcount")
  DataMatrix <- addSamplingId(DataMatrix)
  DataMatrix <- DataMatrix[order(DataMatrix$catchId),]

  catchidMap <- unique(DataMatrix[,c("catchId", "samplingID")])

  DataMatrix <- DataMatrix [,c("weightKG",  "lengthCM", "samplingID", "partnumber", "partcount")]

  ret <- list()
  ret$DataMatrix <- DataMatrix
  ret$catchIdMap <- formatCatchIdMap(catchidMap)

  return(ret)
}

#' Order by catchId before removing column
#' @noRd
getCovariateMatrix <- function(samples, covariates, covariatesMap){

  samples$constant <- 1
  cols <- c("catchId", "constant", covariates)
  samples <- samples[!duplicated(samples$catchId),]
  samples <- samples[,..cols]
  stopifnot(length(samples$catchId) == length(unique(samples$catchId)))
  for (cov in covariates){
    samples[[cov]] <- match(samples[[cov]], covariatesMap[[cov]])
  }
  samples <- samples[order(samples$catchId),]
  samples$catchId <- NULL
  return(samples)
}

#' @noRd
getNeighbours <- function(neighbours){
  if (is.null(neighbours)){
    return(NULL)
  }
  warning("getNeighbours not implemented")
}

#' Formats landings for R-ECA.
#' @details
#'  The parameters 'date', 'month', and 'quarter' are used to set the temporal resolution for catch at age prediction.
#'  Provide exactly one of these, and set the other ones to NULL.
#'
#' @param landings data.table() with total landings (as in \code{\link[prepRECA]{prepRECA}}, and \code{\link[prepRECA]{rEcaDataReport}}), each row corresponding to one cell. Contains columns:
#' \describe{
#'  \item{LiveWeightKG}{numeric(). Total landings (Live/Round weight in Kg) for the cell}
#'  \item{...}{Additional columns to be used as covariates. These define each cell. Type of covariate must be sepcified in 'fixedEffects', 'randomEffects' or 'carEffect'}
#' }
#' @param covariates character() vector of covariates used in model
#' @param covariateMaps list() mapping covariate values from integers used in RECA to values used in 'landings'. For a covarate 'cov', the integer i is used for value a, when covariateMaps[[cov]][[i]] == a
#' @param date POSIXct() vector, matching the number of rows in 'landings', date of catch, see details.
#' @param month integer() vector, matching the number of rows in 'landings', month of catch (1 for January, etc.), see details.
#' @param quarter integer() vector, vector, matching the number of rows in 'landings', quarter of catch (1 for Q1, etc.), see details.
#' @return Landings object as required by \code{\link[Reca]{eca.predict}}
getLandings <- function(landings, covariates, covariateMaps, date=NULL, month=NULL, quarter=NULL){

  if (is.null(date) & is.null(month) & is.null(quarter)){
    stop("date, month, and quarter can not all be NULL")
  }
  if (sum(c(!is.null(date), !is.null(month), !is.null(quarter))) > 1){
    stop("Several arguments for temporal resolution is provided. Provide either: date, month or quarter.")
  }

  inlandings <- names(landings)[names(landings) %in% covariates]

  for (cov in inlandings){
    landings[[cov]] <- match(landings[[cov]], covariateMaps[[cov]])
  }

  if (!is.null(date)){
    landings$midseason <- (as.numeric(strftime(date, "%j"))+1)/366
  }
  else if (!is.null(month)){
    landings$midseason <- (month/12.0)-(1/24.0)
  }
  else if (!is.null(quarter)){
    landings$midseason <- (quarter/4.0)-(1/8.0)
  }
  else{
    stop()
  }

  inlandings <- c(inlandings, "midseason")
  #aggregate ?

  recaLandings <- list()
  recaLandings$AgeLengthCov <- landings[,..inlandings]
  recaLandings$WeightLengthCov <- landings[,..inlandings]
  recaLandings$LiveWeightKG <- landings[["LiveWeightKG"]]

  return(recaLandings)
}

#' Prepare data for R-ECA
#' @description
#'  Checks and reformats data so that it can be fed to R-ECA.
#' @details
#'  The cell definition is specified by 'landings'.
#'  The type of covariates are specified in fixedEffects, randomEffects and carEffect.
#'  All fixed effects, as well as any car-effect, must be included in the cell definition.
#'  All covariates must occur in samples.
#'
#'  The parameters 'date', 'month', and 'quarter' are used to set the temporal resolution for catch at age prediction.
#'  Provide exactly one of these, and set the other ones to NULL.
#'
#'  neighbours must be symetric, so that b \%in\% neighbours[a], implies a \%in\% neighbours[b]
#'
#'  nfish is only needed when several samples may be taken from the same catch.
#'  If these are stratified in any way (e.g. pre-sorting by size or sex), an estimate of strata sizes must be given (count).
#'  If these are replicate samples from the same selection frame, an estimate of the total catch may be given.
#'
#'  output GlobalParameters: While outputs AgeLength, WeightLength and Landings are complete and ready for R-ECA runs.
#'  This function populates the list of GlobalParameters only partially. Run parameters have to be specified elsewhere.
#'
#' @param samples data.table() with samples, each row corresponding to one sampled fish. Contains columns:
#'  \describe{
#'   \item{catchId}{Column identifying the catch that the sample was taken from. Typically a haul or a landing.}
#'   \item{sampleId}{Column identifying the sample. If only one sample is taken for each catch. This can be set equal to catchId}
#'   \item{date}{POSIXct() Date of catch}
#'   \item{Age}{integer() Age of fish}
#'   \item{Length}{numeric() Length of fish in cm. Must be complete (no NAs)}
#'   \item{Weight}{numeric() Weight of fish in kg. Fish with missing values will not be included in Weight-given-length model.}
#'   \item{...}{Additional columns to be used as covariates. Must at least be all covariates in 'landings'. Type of covariate must be sepcified in 'fixedEffects', 'randomEffects' or 'carEffect'}
#'  }
#' @param landings data.table() with total landings, each row corresponding to one cell. Contains columns:
#' \describe{
#'  \item{LiveWeightKG}{numeric(). Total landings (Live/Round weight in Kg) for the cell}
#'  \item{...}{Additional columns to be used as covariates. These define each cell. Type of covariate must be sepcified in 'fixedEffects', 'randomEffects' or 'carEffect'}
#' }
#' @param fixedEffects character() vector specifying fixed effects. Corresponding columns must exists in samples and landings.
#' @param randomEffects character() vector specifying random effects. Corresponding columns must exists samples (may also exist in landings).
#' @param carEffect character() specifying a random effect with conditional autoregressive coefficient. Corresponding columns must exists samples (may also exist in landings).
#' @param neighbours list() specifying the neighbourhood-structure for the carEffect. neighbours[a] should provide a vector of neighbours to a. May be NULL of no carEffect is used.
#' @param nFish data.table() specifying the number of fish in the part of the catch that each sample was taken from. Not alwaus needed. See details. Columns:
#' @param ageError matrix() specifying the probability of read age (rows), given true age (columns). Row and column names specify the ages. If NULL, a unit matrix is assumed (No error in age reading).
#' @param minAge lowest age to include in model. If NULL, minimal age in samples is used.
#' @param maxAge highest age to include in model. If NULL, maximal age in samples is used.
#' @param maxLength longest length to include in model.  If NULL, maximal length in samples is used.
#' @param lengthResolution desired resolution for length groups. If NULL minimal difference in first testMax records are used.
#' @param testMax The largest number of record to inspect for deriving lengthResolution.
#' @param date POSIXct() vector, matching the number of rows in 'landings', date of catch, see details.
#' @param month integer() vector, matching the number of rows in 'landings', month of catch (1 for January, etc.), see details.
#' @param quarter integer() vector, vector, matching the number of rows in 'landings', quarter of catch (1 for Q1, etc.), see details.
#' \describe{
#'  \item{sampleID}{Column idenitfying the sample, defined as for 'samples'}
#'  \item{count}{Estimated number of fish in the part of the catch the sample was taken from}
#' }
#' @return list() with elements:
#' \describe{
#'  \item{AgeLength}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}}
#'  \item{WeightLength}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}}
#'  \item{Landings}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}}
#'  \item{GlobalParameters}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}. see details}
#'  \item{CovariateMaps}{Mapping of values for each covariate in landings and samples (including catchId) to integer value used in R-ECA.}
#' }
#' @export
prepRECA <- function(samples, landings, fixedEffects, randomEffects, carEffect=NULL, neighbours=NULL, nFish=NULL, ageError=NULL, minAge=NULL, maxAge=NULL, maxLength=NULL, lengthResolution=NULL, testMax=1000, date=NULL, month=NULL, quarter=NULL){
  # check mandatory columns
  if (!(all(c("LiveWeightKG") %in% names(landings)))){
    stop("Column LiveWeightKG is mandatory in landings")
  }
  if (!(all(c("catchId", "sampleId", "date", "Age", "Length", "Weight") %in% names(samples)))){
    stop("Columns, catchId, sampleId, Age, Length, and Weight are mandatory in samples")
  }

  # check for NAs
  if (!all(!is.na(samples[,!c("Weight", "Age")]))){
    stop("NAs are only allowed for weight and age in samples")
  }
  if(!all(!is.na(landings))){
    stop("NAs in landings")
  }

  #check different effect types
  if (!is.null(fixedEffects) & length(fixedEffects) > 0){
    if (!all(fixedEffects %in% names(samples))){
      stop(paste("Data missing for fixed effects (samples):", paste(fixedEffects[!(fixedEffects %in% names(samples))], collapse=",")))
    }
    if (!all(fixedEffects %in% names(landings))){
      stop(paste("Data missing for fixed effects (landings):", paste(fixedEffects[!(fixedEffects %in% names(landings))], collapse=",")))
    }
    if (!checkAllSampled(landings, samples[!is.na(samples$Age)], fixedEffects)){
      stop("Not all combinations of fixed effects are sampled for Age")
    }
    if (!checkAllSampled(landings, samples[!is.na(samples$Weight)], fixedEffects)){
      stop("Not all combinations of fixed effects are sampled for Weight")
    }
  }
  if (!is.null(carEffect)){
    if (!(carEffect %in% names(samples))){
      stop(paste("Data missing for CAR effect (samples):", carEffect))
    }
    if (!(carEffect %in% names(landings))){
      stop(paste("Data missing for CAR effect (landings):", carEffect))
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

    if (!checkAllSampledCar(landings, samples[!is.na(samples$Age)], fixedEffects, carEffect, neighbours)){
      stop("Not all combinations of fixed effects are sampled together with CAR effect or neighbours for Age")
    }
    if (!checkAllSampledCar(landings, samples[!is.na(samples$Weight)], fixedEffects, carEffect, neighbours)){
      stop("Not all combinations of fixed effects are sampled together with CAR effect or neighbours for Weight")
    }

  }
  if (!is.null(randomEffects) & length(randomEffects) > 0){
    if (!all(randomEffects %in% names(samples))){
      stop(paste("Data missing for random effects (samples):", paste(randomEffects[!(randomEffects %in% names(samples))], collapse=",")))
    }
  }

  #check that all covariates are specified
  if(!(all(names(samples) %in% c(fixedEffects, randomEffects, carEffect, c("catchId", "sampleId", "date", "Age", "Length", "Weight"))))){
    stop(paste("Effect not specified for covariates:", paste(names(samples)[!(names(samples) %in% c(fixedEffects, randomEffects, carEffect, c("catchId", "Age", "Length", "Weight")))], collapse=",")))
  }
  if(!(all(names(landings) %in% c(fixedEffects, randomEffects, carEffect, c("LiveWeightKG"))))){
    stop(paste("Effect not specified for covariates:", paste(names(landings)[!(names(landings) %in% c(fixedEffects, randomEffects, carEffect, c("LiveWeightKG")))], collapse=",")))
  }

  covariateMaps <- list()
  for (f in c(fixedEffects, randomEffects, carEffect)){
    covariateMaps[[f]] <- getCovariateMap(f, samples, landings)
  }

  if (is.null(lengthResolution)){
    lengthDiffs<-abs(outer(samples$Length[1:testMax], samples$Length[1:testMax], "-"))
    lengthResolution <- min(lengthDiffs[lengthDiffs != 0])
  }

  if (is.null(maxLength)){
    maxLength <- max(samples$Length)
  }
  if (is.null(minAge)){
    minAge <- min(samples$Age, na.rm=T)
  }
  if (is.null(maxAge)){
    maxAge <- max(samples$Age, na.rm=T)
  }
  if (any(!is.na(samples$Age) & samples$Age < minAge)){
    stop("Samples contains ages smaller than minAge")
  }
  if (any(!is.na(samples$Age) & samples$Age > maxAge)){
    stop("Samples contains ages larger than maxAge")
  }
  if (any(samples$Length > maxLength)){
    stop("Samples contains lengths longer than maxLength")
  }

  ageRange <- seq(minAge, maxAge)

  if (!is.null(ageError)){
    if (!is.matrix(ageError)){
      stop("ageError must be a matrix")
    }
    if (is.null(rownames(ageError)) | is.null(colnames(ageError))){
      stop("rownames and colnames must be set for ageError matrix")
    }
    if (!(all(ageRange %in% rownames(ageError) & ageRange %in% colnames(ageError)))){
      stop("Age error matrix must have entries for the entire age range estimated.")
    }
    ageError <- ageError[ageRange, ageRange]
  }

  # build eca objects
  info <- getInfoMatrix(samples, landings, fixedEffects, randomEffects, carEffect)

  AgeLength <- list()
  ret <- getDataMatrixAgeLength(samples, nFish)
  AgeLength$DataMatrix <- ret$DataMatrix
  AgeLength$CovariateMatrix <- getCovariateMatrix(samples, c(fixedEffects, randomEffects, carEffect), covariateMaps)
  AgeLength$info <- info
  AgeLength$CARNeighbours <- getNeighbours(neighbours)
  AgeLength$AgeErrorMatrix <- ageError
  AgeLength$CCerrorList <- NULL # CC (stock splitting) not supported
  covariateMaps$AgeLengthCatchId <- ret$catchIdMap

  WeightLength <- list()
  ret <- getDataMatrixWeightLength(samples[!is.na(samples$Weight),])
  WeightLength$DataMatrix <- ret$DataMatrix
  WeightLength$CovariateMatrix <- getCovariateMatrix(samples[!is.na(samples$Weight),], c(fixedEffects, randomEffects, carEffect), covariateMaps)
  WeightLength$info <- info
  # CARNeighbours are taken from AgeLength
  covariateMaps$WeightLengthCatchId <- ret$catchIdMap

  Landings <- getLandings(landings, c(fixedEffects, randomEffects, carEffect), covariateMaps, date, month, quarter)

  GlobalParameters <- list()
  GlobalParameters$lengthresCM <- lengthResolution
  GlobalParameters$maxlength <- maxLength
  GlobalParameters$minage <- minAge
  GlobalParameters$maxage <- maxAge
  GlobalParameters$age.error <- !is.null(ageError)
  GlobalParameters$CC <- F  # CC (stock splitting) not supported
  GlobalParameters$CCerror <- F # CC (stock splitting) not supported


  ret <- list()
  ret$AgeLength <- AgeLength
  ret$WeightLength <- WeightLength
  ret$Landings <- Landings
  ret$GlobalParameters <- GlobalParameters
  ret$CovariateMaps <- covariateMaps

  return(ret)
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
#' @param samples data.table() with samples (as in \code{\link[prepRECA]{prepRECA}}), each row corresponding to one sampled fish. Contains columns:
#'  \describe{
#'   \item{catchId}{Column identifying the catch that the sample was taken from. Typically a haul or a landing.}
#'   \item{sampleId}{Column identifying the sample. If only one sample is taken for each catch. This can be set equal to catchId}
#'   \item{Age}{integer() Age of fish.}
#'   \item{Length}{numeric() Length of fish. Must be complete (no NAs)}
#'   \item{Weight}{numeric() Weight of fish.}
#'   \item{...}{Additional columns to be used as covariates. Must at least be all covariates in 'landings'.}
#'  }
#'
#' @param landings data.table() with total landings (as in \code{\link[prepRECA]{prepRECA}}), each row corresponding to one cell. Contains columns:
#' \describe{
#'  \item{LiveWeightKG}{numeric(). Total landings (Live/Round weight in Kg) for the cell}
#'  \item{...}{Additional columns to be used as covariates. These define each cell.}
#' }
#' @return data.table() with columns
#' \describe{
#'  \item{<Covariates in landings>}{one column for each. Defines the cells.}
#'  \item{LiveWeightKG}{The total weight (kg) in the cell.}
#'  \item{LiveWeightCumFraction}{The fraction of landings in this cell AND all the cells with higher total weight than this cell.}
#'  \item{<Count of covariates not in landings>}{Count of unique values for covariate. one column for each. Column name is covariate name (from samples) prefixed with N}
#'  \item{Ncatch}{The number of unique catches sampled in the cell.}
#'  \item{Nsample}{The number of unique catch-samples in the cell.}
#'  \item{Nage}{The number of age readings in the cell.}
#'  \item{Nweight}{The number of fish weight measurements in the cell.}
#'  \item{Nlength}{The number of fish length measurements in the cell.}
#' }
#' @export
rEcaDataReport <- function(samples, landings){
  # check mandatory columns
  if (!(all(c("LiveWeightKG") %in% names(landings)))){
    stop("Column LiveWeightKG is mandatory in landings")
  }
  if (!(all(c("catchId", "sampleId", "date", "Age", "Length", "Weight") %in% names(samples)))){
    stop(paste("Columns, catchId, sampleId, Age, Length, and Weight are mandatory in samples. Missing:", paste(c("catchId", "sampleId", "Age", "Length", "Weight")[(!c("catchId", "sampleId", "Age", "Length", "Weight") %in% names(samples))])))
  }

  if (length(unique(samples$sampleId)) != nrow(unique(samples[, c("catchId", "sampleId")]))){
    stop("sampleID must be unique identifier, irrespective of catchId")
  }

  # check for NAs
  if (!all(!is.na(samples[,!c("Weight", "Age")]))){
    stop("NAs are only allowed for weight in samples")
  }
  if(!all(!is.na(landings))){
    stop("NAs in landings")
  }

  inlandings <- names(landings)
  inlandings <- inlandings[!(inlandings %in% c("LiveWeightKG"))]

  insamples <- names(samples)
  insamples <- insamples[!(insamples %in% c("catchId", "sampleId", "date", "Age", "Length", "Weight"))]

  if (length(inlandings) == 0){
    stop("No covariates in landings. Cannot produce report.")
  }

  if (!all(inlandings %in% insamples)){
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

  #sampled dates
  nDs <- aggregate(list(Ndate=samples$date), by=agglist, FUN=function(x){length(unique(x))})
  # sampled units (count unique)
  nCs <- aggregate(list(Ncatch=samples$catchId), by=agglist, FUN=function(x){length(unique(x))})
  # samples units (count unique)
  nSs <- aggregate(list(Nsample=samples$sampleId), by=agglist, FUN=function(x){length(unique(x))})

  # fish parameters, count non-NA rows
  nAges <- aggregate(list(Nage=samples$Age), by=agglist, FUN=function(x){sum(!is.na(x))})
  nWeight <- aggregate(list(Nweight=samples$Weight), by=agglist, FUN=function(x){sum(!is.na(x))})
  nLength <- aggregate(list(Nlength=samples$Length), by=agglist, FUN=function(x){sum(!is.na(x))})

  # total weights, sum
  kgLanded <- aggregate(list(LiveWeightKG=landings$LiveWeightKG), by=agglistLand, FUN=sum)
  kgLanded <- kgLanded[order(kgLanded$LiveWeightKG, decreasing=T),]
  kgLanded$LiveWeightCumFraction <- cumsum(kgLanded$LiveWeightKG) / sum(kgLanded$LiveWeightKG)

  out <- kgLanded

  # covariates (count unique)
  for (s in insamples[!(insamples %in% inlandings)]){
    l <- list(t=samples[[s]])
    names(l) <- paste("N",s, sep="")
    agg <- aggregate(l, agglist, FUN=function(x){length(unique(x))})
    out <- merge(out, agg, all.x=T)
  }

  out <- merge(out, nDs, all.x=T)
  out <- merge(out, nCs, all.x=T)
  out <- merge(out, nSs, all.x=T)

  out <- merge(out, nAges, all.x=T)
  out <- merge(out, nWeight, all.x=T)
  out <- merge(out, nLength, all.x=T)

  stopifnot(all(!is.na(out$LiveWeightKG)))

  # NAs come from cells not present in samples
  out[is.na(out)]<-0
  out <- out[order(out$LiveWeightKG, decreasing = T),]
  out <- data.table::as.data.table(out)
  return(out)
}
