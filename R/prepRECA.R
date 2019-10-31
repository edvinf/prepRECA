
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
      landingsfixed <- paste(landingsfixed, landings[[f]], sep="/")
      samplesfixed <- paste(samplesfixed, samples[[f]], sep="/")
    }
  }

  return(all(landingsfixed %in% samplesfixed))
}

#' @noRd
checkSamplesInFrame <- function(samples, landings, covariates){

  inlandings <- covariates[covariates %in% names(landings)]

  if (length(inlandings) == 0){
    return(T)
  }

  landingscells <- landings[[inlandings[1]]]
  samplescells <- samples[[inlandings[1]]]

  if (length(inlandings) > 1){
    for (f in inlandings[2:length(inlandings)]){
      landingscells <- paste(landingscells, landings[[f]], sep="/")
      samplescells <- paste(samplescells, samples[[f]], sep="/")
    }
  }

  return(all(samplescells %in% landingscells))
}

#' Check that all fixed effects are sampled in combination with carEffect or neighbout
#' @noRd
checkAllSampledCar <- function(landings, samples, fixedEffects, carEffect, neighbours){

  for (l in unique(landings[[carEffect]])){
    landcar <- landings[landings[[carEffect]] == l,]
    sampcar <- samples[samples[[carEffect]] %in% c(l, neighbours[l]),]
    sampcar[[carEffect]] <- l
    if (!checkAllSampled(landcar, sampcar, c(fixedEffects, carEffect))){
      return(F)
    }
  }

  return(T)
}

#' Make Covariate Map
#' @description Constructs a covariate map for R-ECA
#' @details
#'  RECA require all covariate to be formatted as integers.
#'  In order to maintain connection to other formats for the covariates, they should be converted back and forth with a pre-defined covariate map
#' @param covariate character() name of covariate to make map for
#' @param samples data.table() the samples the covariate mapping should be defined for (must contain column specified by 'covariate')
#' @param landings data.table() the landings the covariate mapping should be defined for (may contain column specified by 'covariate')
#' @return list() where the nth member represents the RECA covariate value 'n', so that for a covarate 'cov', the integer i is used for value a, when covariateMaps[[cov]][[i]] == a
#' @examples
#'  samples <- prepRECA::NORportsampling2018$LE
#'  samples$Metier5 <- samples$LEmetier5
#'  landings <- CLCodHadNOR
#'  landings$Metier5 <- landings$FishingActivityCategoryEuropeanLvl5
#'  getCovariateMap("Metier5", samples, landings)
#' @export
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
    map[[i]] <- v
    i <- i + 1
  }

  return(map)
}

#' order columns by constant, inlandings, notinlandings (inlandings and notinalndings sorted alphabetically)
#' @noRd
getInfoMatrix <- function(samples, landings, fixedEffects, randomEffects, carEffect){
  info <- matrix(ncol=7, nrow=length(c(fixedEffects, randomEffects, carEffect))+1)
  colnames(info) <- c("random", "CAR", "continuous", "in.landings", "nlev", "interaction", "in.slopeModel")
  rownames(info) <- c("constant", c(fixedEffects, randomEffects, carEffect))
  info[1,] <- c(0,0,0,1,1,0,1)
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
        info[i,] <- c(1,0,0,inl,length(unique(c(samples[[e]], landings[[e]]))), inl, 0)
      }
      else{
        info[i,] <- c(1,0,0,inl,length(unique(samples[[e]])), inl, 0)
      }

      i <- i+1
    }
  }
  if (!is.null(carEffect)){
    inl <- 0
    if (carEffect %in% names(landings)){
      inl <- 1
    }
    info[i,] <- c(1,1,0,inl,length(unique(c(samples[[carEffect]], landings[[carEffect]]))), inl, 0)
    i <- i+1
  }

  #order columns by constant, inlandings, notinlandings (inlandings and notinalndings sorted alphabetically)
  inlandings <- rownames(info)[info[,"in.landings"] == 1]
  inlandings <- sort(inlandings[inlandings != "constant"])
  notinlandings <- sort(rownames(info)[info[,"in.landings"] == 0])

  ord <- c("constant", inlandings, notinlandings)
  info <- info[match(ord, rownames(info)),]
  return(info)
}

#' Run before renaming columns
#' @noRd
addPartCount <- function(DataMatrix, nFish){

  # renumber sampleID to delprøve convention (needed ?)
  partsamples <- aggregate(list(nSampleId=DataMatrix$sampleId), by=list(catchId=DataMatrix$catchId), FUN=function(x){length(unique(x))})
  partsamples <- merge(partsamples, unique(DataMatrix[,c("sampleId", "catchId")]))

  if (nrow(partsamples) == 0){
    DataMatrix$partcount <- NA
    return(DataMatrix)
  }
  else if (nrow(partsamples) > 0){
    if (is.null(nFish) & !all(partsamples$nSampleId == 1)){
      stop(paste("Some catches are sampled several times, but argument 'nFish' not given."))
    }
    else if (is.null(nFish) & all(partsamples$nSampleId == 1)){
      DataMatrix$partcount <- NA
      return(DataMatrix)
    }
    if (!all(partsamples[partsamples$nSampleId > 1,]$sampleId %in% nFish$sampleId)){
      stop(paste("Some catches are sampled several times, but corresponding sampleId not in nFish:", paste(partsamples$sampleId[!(partsamples$sampleId %in% nFish$catchId)], collapse=",")))
    }
    nFish <- nFish[nFish$sampleId %in% DataMatrix$sampleId,]
    nFish$partcount <- as.integer(round(nFish$count))
    nFish$count <- NULL
    DataMatrix <- merge(DataMatrix, nFish, by="sampleId", all.x=T)
    return(DataMatrix)
  }

}

#' @noRd
addSamplingId <- function(DataMatrix){
  mapping <- data.table::data.table(catchId=unique(DataMatrix$catchId), samplingID=seq(1,length(unique(DataMatrix$catchId))))
  return(data.table::as.data.table(merge(DataMatrix, mapping, by="catchId", all.x=T)))
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
#'
#' @noRd
getDataMatrixAgeLength <- function(samples, nFish=NULL, hatchday=1){
  DataMatrix <- samples[,c("catchId", "sampleId", "date", "Age", "Length", "Weight")]
  DataMatrix$day <- (as.integer(strftime(DataMatrix$date, "%j")) + 2 - hatchday) #srtftime returns 0-based day of the year
  sel <- DataMatrix$day <= 0

  DataMatrix[sel,"Age"] <- DataMatrix[sel,"Age"] - 1L
  DataMatrix[sel,"day"] <- 366 + DataMatrix[sel,"day"] #day is negative in this case, hence the + operator
  DataMatrix$date <- DataMatrix$day / 366

  DataMatrix <- DataMatrix[,c("Age", "date", "Length", "catchId", "sampleId")]
  DataMatrix <- addPartCount(DataMatrix, nFish)
  DataMatrix <- DataMatrix[,c("Age", "date", "Length", "catchId", "sampleId", "partcount")]
  names(DataMatrix) <- c("age", "part.year", "lengthCM", "catchId", "partnumber", "partcount")
  DataMatrix <- addSamplingId(DataMatrix)
  DataMatrix <- DataMatrix[order(DataMatrix$catchId),]

  catchidMap <- unique(DataMatrix[,c("catchId", "samplingID")])

  DataMatrix$otolithtype <- NA
  DataMatrix$otolithtype <- as.integer(DataMatrix$otolithtype)
  DataMatrix <- DataMatrix [,c("age",  "part.year", "lengthCM", "samplingID", "partnumber", "otolithtype", "partcount")]

  if (any(DataMatrix$age) < 0){
    stop("Negative age for fish. Consider parameter hatchDay.")
  }

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
  DataMatrix <- DataMatrix[,c("Weight", "Length", "catchId", "sampleId", "partcount")]
  names(DataMatrix) <- c("weightKG",  "lengthCM", "catchId", "partnumber", "partcount")
  DataMatrix <- addSamplingId(DataMatrix)
  DataMatrix <- DataMatrix[order(DataMatrix$catchId),]

  catchidMap <- unique(DataMatrix[,c("catchId", "samplingID")])

  DataMatrix$otolithtype <- NA
  DataMatrix$otolithtype <- as.integer(DataMatrix$otolithtype)
  DataMatrix <- DataMatrix [,c("weightKG", "lengthCM", "samplingID", "partnumber", "otolithtype", "partcount")]

  ret <- list()
  ret$DataMatrix <- DataMatrix
  ret$catchIdMap <- formatCatchIdMap(catchidMap)

  return(ret)
}

#' Order by catchId before removing column
#' order columns by constant, inlandings, notinlandings (inlandings and notinalndings sorted alphabetically)
#' @noRd
getCovariateMatrix <- function(samples, covariates, covariatesMapInLandings, covariatesMapRandom){

  samples$constant <- 1
  cols <- c("catchId", "constant", covariates)
  samples <- samples[!duplicated(samples$catchId),]
  samples <- samples[,..cols]
  stopifnot(length(samples$catchId) == length(unique(samples$catchId)))
  for (cov in covariates){
    if (length(covariatesMapInLandings) > 0 & cov %in% names(covariatesMapInLandings)){
      samples[[cov]] <- match(samples[[cov]], covariatesMapInLandings[[cov]])
    }
    else if (length(covariatesMapRandom) > 0 & cov %in% names(covariatesMapRandom)){
      samples[[cov]] <- match(samples[[cov]], covariatesMapRandom[[cov]])
    }
    else{
      stop()
    }
  }
  samples <- samples[order(samples$catchId),]
  samples$catchId <- NULL

  inlandings <- c()
  if (!is.null(covariatesMapInLandings)){
    inlandings <- covariates[covariates %in% names(covariatesMapInLandings)]
    inlandings <- sort(inlandings)
  }

  notinlandings <- c()
  if (!is.null(covariatesMapRandom)){
    notinlandings <- covariates[covariates %in% names(covariatesMapRandom)]
    notinlandings <- sort(notinlandings)
  }

  cols <- c("constant", inlandings, notinlandings)
  return(samples[,..cols])
}

#' @param neighbours list mapping values of covariate with CAR effect to list of neighbours (symmetric)
#' @param covariateMap covariate Map for CAR effect
#' @noRd
getNeighbours <- function(neighbours, covariateMap){
  if (is.null(neighbours)){
    return(NULL)
  }

  if (length(neighbours) != length(covariateMap)){
    stop("length of neighbours does not match length of covariateMap")
  }
  CARNeigbours <- list()
  CARNeigbours$numNeighbours <- c()
  CARNeigbours$idNeighbours <- c()
  for (i in 1:length(covariateMap)){
    CARNeigbours$numNeighbours <- c(CARNeigbours$numNeighbours, length(neighbours[[covariateMap[[i]]]]))
    CARNeigbours$idNeighbours <- c(CARNeigbours$idNeighbours, match(neighbours[[covariateMap[[i]]]], covariateMap))
  }
  return(CARNeigbours)
}

#' Formats landings for R-ECA.
#' @description
#'  Prepares a Landings object as required by \code{\link[Reca]{eca.predict}}.
#' @details
#'  The parameters 'date', 'month', and 'quarter' are used to set the temporal resolution for catch at age prediction.
#'  Provide exactly one of these, and set the other ones to NULL.
#'  Temporal resolution need not match any temporal covariate used.
#'  One can for example run with month, even if Quarter is a covariate in the model.
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
#' @examples
#'  samples <- prepRECA::NORportsampling2018$LE
#'  samples$Metier5 <- samples$LEmetier5
#'  landings <- CLCodHadNOR
#'  landings$LiveWeightKG <- landings$OfficialLandingsWeight
#'  landings$Metier5 <- landings$FishingActivityCategoryEuropeanLvl5
#'  covMap <- getCovariateMap("Metier5", samples, landings)
#'  getLandings(landings, c("Metier5"), covMap, month=landings$Month)
#' @export
getLandings <- function(landings, covariates, covariateMaps, date=NULL, month=NULL, quarter=NULL){

  #consider redesigning, using info matrix for ordering

  if (is.null(date) & is.null(month) & is.null(quarter)){
    stop("date, month, and quarter can not all be NULL")
  }
  if (sum(c(!is.null(date), !is.null(month), !is.null(quarter))) > 1){
    stop("Several arguments for temporal resolution is provided. Provide either: date, month or quarter.")
  }

  inlandings <- names(landings)[names(landings) %in% covariates]
  inlandings <- sort(inlandings)

  aggList <- list()
  for (cov in inlandings){
    aggList[[cov]] <- landings[[cov]]
  }
  if (!is.null(date)){
    aggList$tempres <- date
  }
  if (!is.null(month)){
    aggList$tempres <- month
  }
  if (!is.null(quarter)){
    aggList$tempres <- quarter
  }


  landings <- aggregate(list(LiveWeightKG=landings$LiveWeightKG), by=aggList, FUN=sum)


  for (cov in inlandings){
    landings[[cov]] <- match(landings[[cov]], covariateMaps[[cov]])
  }

  if (!is.null(date)){
    landings$midseason <- (as.numeric(strftime(landings$tempres, "%j"))+1)/366
  }
  else if (!is.null(month)){
    landings$midseason <- (landings$tempres/12.0)-(1/24.0)
  }
  else if (!is.null(quarter)){
    landings$midseason <- (landings$tempres/4.0)-(1/8.0)
  }
  else{
    stop()
  }
  landings$tempres <- NULL

  landings <- data.table::as.data.table(landings)

  landings$constant <- 1
  inlandings <- c("constant", inlandings, "midseason")

  recaLandings <- list()
  recaLandings$AgeLengthCov <- landings[,..inlandings]
  recaLandings$WeightLengthCov <- landings[,..inlandings]
  recaLandings$LiveWeightKG <- landings[["LiveWeightKG"]]

  return(recaLandings)
}

#' Prepare data for R-ECA
#' @description
#'  Checks and reformats data as required by \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}
#' @details
#'  The cell definition is specified by 'landings'.
#'  The type of covariates are specified in fixedEffects, randomEffects and carEffect.
#'  All fixed effects, as well as any car-effect, must be included in the cell definition.
#'  All covariates must occur in samples.
#'
#'  The parameters 'date', 'month', and 'quarter' are used to set the temporal resolution for catch at age prediction.
#'  Provide exactly one of these, and set the other ones to NULL.
#'  Temporal resolution need not match any temporal covariate used.
#'  One can for example run with month, even if Quarter is a covariate in the model.
#'  Note that resolution is sensitive to data volume. If you get errors in prediction with E_p(a) = nan,
#'  consider trying with quarter.
#'
#'  neighbours must be symetric, so that b \%in\% neighbours[a], implies a \%in\% neighbours[b]
#'
#'  nfish is only needed when several samples may be taken from the same catch.
#'  If these are stratified in any way (e.g. pre-sorting by size or sex), an estimate of strata sizes must be given (count).
#'  If these are replicate samples from the same selection frame, an estimate of the total catch may be given.
#'
#'  output GlobalParameters: While outputs AgeLength, WeightLength and Landings are complete and ready for R-ECA runs.
#'  This function populates the list of GlobalParameters only partially. Run parameters have to be added afterwards.
#'
#' @param samples data.table() with samples, each row corresponding to one sampled fish. Contains columns:
#'  \describe{
#'   \item{catchId}{Column identifying the catch that the sample was taken from. Typically a haul or a landing.}
#'   \item{sampleId}{Column identifying the sample. If only one sample is taken for each catch. This can be set equal to catchId}
#'   \item{date}{POSIXct() Date of catch}
#'   \item{Age}{integer() Age of fish}
#'   \item{Length}{numeric() Length of fish in cm. Must be complete (no NAs)}
#'   \item{Weight}{numeric() Weight of fish in kg. Fish with missing values will not be included in Weight-given-length model.}
#'   \item{...}{Additional columns which may be used as covariates as covariates. Type of covariate must be sepcified in 'fixedEffects', 'randomEffects' or 'carEffect'}
#'  }
#' @param landings data.table() with total landings, each row corresponding to one cell. Contains columns:
#' \describe{
#'  \item{LiveWeightKG}{numeric(). Total landings (Live/Round weight in Kg) for the cell}
#'  \item{...}{Additional columns which may be used as covariates. Covariates in landings define each cell. Type of covariate must be sepcified in 'fixedEffects', 'randomEffects' or 'carEffect'}
#' }
#' @param fixedEffects character() vector specifying fixed effects. Corresponding columns must exists in samples and landings.
#' @param randomEffects character() vector specifying random effects. Corresponding columns must exists samples (may also exist in landings).
#' @param carEffect character() specifying a random effect with conditional autoregressive coefficient. Corresponding columns must exists samples (may also exist in landings).
#' @param neighbours list() specifying the neighbourhood-structure for the carEffect. neighbours[a] should provide a vector of neighbours to a. May be NULL of no carEffect is used.
#' @param nFish data.table() specifying the number of fish in the part of the catch that each sample was taken from. Not alwaus needed. See details. Columns:
#' @param ageError matrix() specifying the probability of read age (rows), given true age (columns). Row and column names specify the ages. If NULL, a unit matrix is assumed (No error in age reading).
#' @param minAge lowest age to include in model. If NULL, minimal age in samples is used. Age range must match any age error matrix provided (ageError)
#' @param maxAge highest age to include in model. If NULL, maximal age in samples is used. Age range must match any age error matrix provided (ageError)
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
#' @examples
#'  samples <- extractBV(prepRECA::NORportsampling2018$BV, c("Age", "Length", "Weight"), c("integer", "numeric", "numeric"))
#'  samples <- merge(samples, prepRECA::NORportsampling2018$SA, by="SAid")
#'  samples <- merge(samples, prepRECA::NORportsampling2018$SS, by="SSid")
#'  samples <- merge(samples, prepRECA::NORportsampling2018$LE, by="LEid", suffixes = c("", ".LE"))
#'  samples <- merge(samples, prepRECA::NORportsampling2018$VD, by="VDid")
#'  samples$catchId <- samples$LEid
#'  samples$sampleId <- samples$SAid
#'  samples$date <- samples$LEdate
#'  samples$Metier5 <- samples$LEmetier5
#'  landings <- prepRECA::CLCodHadNOR
#'  landings$LiveWeightKG <- landings$OfficialLandingsWeight
#'  landings$Metier5 <- landings$FishingActivityCategoryEuropeanLvl5
#'  prepRECA(samples, landings, NULL, c("Metier5", "VDencrCode"))
#' @export
prepRECA <- function(samples, landings, fixedEffects, randomEffects, carEffect=NULL, neighbours=NULL, nFish=NULL, ageError=NULL, minAge=NULL, maxAge=NULL, maxLength=NULL, lengthResolution=NULL, testMax=1000, date=NULL, month=NULL, quarter=NULL){
  hatchDay=1
  # check mandatory columns
  if (!(all(c("LiveWeightKG") %in% names(landings)))){
    stop("Column LiveWeightKG is mandatory in landings")
  }
  if (!(all(c("catchId", "sampleId", "date", "Age", "Length", "Weight") %in% names(samples)))){
    stop("Columns, catchId, sampleId, Age, Length, and Weight are mandatory in samples")
  }

  # check for NAs
  ins <- c(fixedEffects, randomEffects, carEffect, "Length")
  if (!all(!is.na(samples[,..ins]))){
    stop("NAs are only allowed for weight and age in samples")
  }
  inl <- c(fixedEffects, randomEffects, carEffect, "LiveWeightKG")[c(fixedEffects, randomEffects, carEffect, "LiveWeightKG") %in% names(landings)]
  if(!all(!is.na(landings[,..inl]))){
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
    if (!checkAllSampled(landings, samples, fixedEffects)){
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
      for (n in neighbours[[l]]){
        if (!(n %in% names(neighbours) | l %in% c(neighbours[[n]])))
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

  if (!is.null(nFish)){
    if (!(all(c("sampleId", "count") %in% names(nFish)))){
      stop("Columns 'sampleId' and 'count' are mandatory for parameter nFish.")
    }
    if (any(is.na(nFish))){
      stop("nFish contains NAs.") #Note that nFish need only be provided for samples (sampleId) where there is more than one sample for a catch (catchId)
    }
  }

  if (!checkSamplesInFrame(samples, landings, c(fixedEffects, randomEffects, carEffect))){
    stop("Some samples are taken from cells not in landings.")
  }

  if (!is.null(hatchDay)){
    if (hatchDay != 1){
      # need to consult with NR about documentation. Get weird crashes for some values.
      stop("HatchDay not properly supported yet.")
    }
    if (hatchDay<1 | hatchDay>366){
      stop("Invalid hatchDay.")
    }
  }

  covariateMaps <- list()

  #covariateMaps common between models (effects in landings)
  covariateMaps$inLandings <- list()
  for (f in c(fixedEffects, randomEffects, carEffect)[c(fixedEffects, randomEffects, carEffect) %in% names(landings)]){
    covariateMaps$inLandings[[f]] <- getCovariateMap(f, samples, landings)
  }

  #covariateMaps specific to each model (random effects not in landings)
  covariateMaps$randomEffects <- list()
  covariateMaps$randomEffects$AgeLength <- list()
  covariateMaps$randomEffects$WeightLength <- list()
  for (f in randomEffects[!(c(randomEffects) %in% names(landings))]){
    covariateMaps$randomEffects$AgeLength[[f]] <- getCovariateMap(f, samples, landings)
    covariateMaps$randomEffects$WeightLength[[f]] <- getCovariateMap(f, samples[!is.na(samples$Weight)], landings)
  }

  if (is.null(lengthResolution)){
    lengthDiffs <- abs(outer(samples$Length[1:min(testMax, nrow(samples))], samples$Length[1:min(testMax, nrow(samples))], "-"))
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
    if (nrow(ageError) != length(ageRange)){
      stop("age error matrix does not match the provided age range")
    }
    if (!all(colSums(ageError) == 1)){
      stop("Age error columns does not sum to 1.")
    }
  }


  # build eca objects

  AgeLength <- list()
  ret <- getDataMatrixAgeLength(samples, nFish, hatchDay)
  AgeLength$DataMatrix <- ret$DataMatrix
  AgeLength$CovariateMatrix <- getCovariateMatrix(samples, c(fixedEffects, randomEffects, carEffect), covariateMaps$inLandings, covariateMaps$randomEffects$AgeLength)
  AgeLength$info <- getInfoMatrix(samples, landings, fixedEffects, randomEffects, carEffect)
  AgeLength$CARNeighbours <- getNeighbours(neighbours, covariateMaps$inLandings[[carEffect]])
  AgeLength$AgeErrorMatrix <- ageError
  AgeLength$CCerrorList <- NULL # CC (stock splitting) not supported
  covariateMaps$randomEffects$AgeLength$CatchId <- ret$catchIdMap

  WeightLength <- list()
  ret <- getDataMatrixWeightLength(samples[!is.na(samples$Weight),], nFish)
  WeightLength$DataMatrix <- ret$DataMatrix
  WeightLength$CovariateMatrix <- getCovariateMatrix(samples[!is.na(samples$Weight),], c(fixedEffects, randomEffects, carEffect), covariateMaps$inLandings, covariateMaps$randomEffects$WeightLength)
  WeightLength$info <- getInfoMatrix(samples[!is.na(samples$Weight),], landings, fixedEffects, randomEffects, carEffect)
  WeightLength$CARNeighbours <- getNeighbours(neighbours, covariateMaps$inLandings[[carEffect]])
  covariateMaps$randomEffects$WeightLength$CatchId <- ret$catchIdMap

  Landings <- getLandings(landings, c(fixedEffects, randomEffects, carEffect), covariateMaps$inLandings, date, month, quarter)

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

#' Run data checks and converts data.table to data.frame
#' @noRd
checkEcaObj <- function(RECAobj){
  obj <- RECAobj
  obj$AgeLength$DataMatrix <- as.data.frame(obj$AgeLength$DataMatrix)
  obj$AgeLength$CovariateMatrix <- as.data.frame(obj$AgeLength$CovariateMatrix)
  obj$WeightLength$DataMatrix <- as.data.frame(obj$WeightLength$DataMatrix)
  obj$WeightLength$CovariateMatrix <- as.data.frame(obj$WeightLength$CovariateMatrix)
  obj$Landings$AgeLengthCov <- as.data.frame(obj$Landings$AgeLengthCov)
  obj$Landings$WeightLengthCov <- as.data.frame(obj$Landings$WeightLengthCov)

  checkWeightLength(obj$WeightLength)
  checkAgeLength(obj$AgeLength)
  checkCovariateConsistency(obj$AgeLength, obj$Landings$AgeLengthCov)
  checkCovariateConsistency(obj$WeightLength, obj$Landings$WeightLengthCov)
  checkLandings(obj$Landings)

  checkGlobalParameters(obj$GlobalParameters, obj$AgeLength, obj$WeightLength)

  return(obj)
}

#' @noRd
fixCov <- function(cov, covariateMaps, model){
  for (n in names(cov)[names(cov) != "cell"]){ #deal with cell later
    if (n %in% names(covariateMaps$inLandings)){
      map <- covariateMaps$inLandings[[n]]
    }
    else if (n %in% names(covariateMaps$randomEffects[[model]])){
      map <- covariateMaps$randomEffects[[model]][[n]]
    }

    dimm <- dim(cov[[n]])
    stopifnot(length(dimm) == 3)

    if (n == "constant"){
      dimnames(cov[[n]]) <- list(1:dimm[[1]], 1:dimm[[2]], 1:dimm[[3]])
    }
    else{
      dimnames(cov[[n]]) <- list(1:dimm[[1]], unlist(map[1:dimm[[2]]]), 1:dimm[[3]]) #first dimension is one, expect for Prop at age, when it is the age groups
    }

  }
  return(cov)
}

#' @noRd
fixCar <- function(car, careffect){
  if (is.null(car)){
    return(NULL)
  }
  if (names(car) == c("spatial")){
    names(car) = c(careffect)
  }
  return(car)
}

#' To be called from runRECA, update covariateMap wih an entry for cell in covariateMap$inlandings, before renameRecaOutput is run
#' @noRd
addCellCovariateMap <- function(covariateMap, infoMatrix){
  cellMemb <- rownames(infoMatrix[infoMatrix[,"interaction"] == 1,])
  stop("Not implemented. Need doc update.")
}

#' Rename R-ECA output
#' @description Renames output returned from \code{\link[Reca]{eca.estimate}},
#' so that covariate names and levels correspond to those used in data fed to \code{\link[prepRECA]{prepRECA}}
#' @param fit as returned from \code{\link[Reca]{eca.estimate}},
#' @param covariateMaps as returned from \code{\link{prepRECA}{prepRECA}}
#' @param careffect name of careffect
#' @noRd
renameRecaOutput <- function(ecafit, covariateMaps, careffect){
  stop("Need doc update for cell effects before this can be finalized. Add tests.")
  for (model in names(ecafit)){
    if ("Intercept" %in% names(ecafit[[model]])){
      for (p in names(ecafit[[model]][["Intercept"]])){
        if ("catchSample" %in% names(ecafit[[model]][["Intercept"]][[p]])){
          ecafit[[model]][["Intercept"]][[p]][["catchId"]] <- ecafit[[model]][["Intercept"]][["catchSample"]]
          ecafit[[model]][["Intercept"]][[p]][["catchSample"]] <- NULL
        }
      }
    }
    if ("Slope" %in% names(ecafit[[model]])){
      for (p in names(ecafit[[model]][["Slope"]])){
        if ("catchSample" %in% names(ecafit[[model]][["Slope"]][[p]])){
          ecafit[[model]][["Slope"]][["cov"]][[p]][["catchId"]] <- ecafit[[model]][["Slope"]][["catchSample"]]
          ecafit[[model]][["Slope"]][["cov"]][[p]][["catchSample"]] <- NULL
        }
      }
    }

    for (reg in c("Intercept", "Slope")){
      if ("cov" %in% names(ecafit[[model]][[reg]])){
        ecafit[[model]][[reg]][["cov"]] <- fixCov(ecafit[[model]][[reg]][["cov"]], covariateMaps, model)
      }
      if ("CAR" %in% names(ecafit[[model]][[reg]])){
        ecafit[[model]][[reg]][["CAR"]] <- fixCar(ecafit[[model]][[reg]][["CAR"]], careffect)
      }
    }
  }
  return(ecafit)
}

#' Run R-ECA
#' @description Runs \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}.
#' @details
#'  \code{\link[Reca]{eca.estimate}} performs Markov-chain Monte Carlo (MCMC) simulations to determine maximum likelihood of parameters for the given samples.
#'
#'  \code{\link[Reca]{eca.predict}} samples the posterior distributions of parameters estimated in \code{\link[Reca]{eca.estimate}},
#'  in order to obtain proportinos of catches and fish parameters.
#'  Using these parameters and the given total landings, predictions of distribution of catch-parameter distributions will be calculated.
#'
#'  If resultdir is NULL,  atemporary directory will be created for its purpose.
#'  This will be attempted removed after execution.
#'  If removal is not successful a warning will be issued which includes the path to the temporary directory.
#'
#' @param RecaObj as returned from \code{\link[prepRECA]{prepRECA}}
#' @param nSamples number of MCMC samples that will be made available for \code{\link[Reca]{eca.predict}}. See documentation for \code{\link[Reca]{eca.estimate}},
#' @param burnin number of MCMC samples run and discarded by \code{\link[Reca]{eca.estimate}} before any samples are saved. See documentation for \code{\link[Reca]{eca.estimate}}.
#' @param lgamodel The length age relationship to use for length-age fits (options: "log-linear", "non-linear": Schnute-Richards model). See documentation for \code{\link[Reca]{eca.estimate}}.
#' @param fitfile name of output files in resultdir. See documentation for \code{\link[Reca]{eca.estimate}}.
#' @param predictfile name of output files in resultdir. See documentation for \code{\link[Reca]{eca.predict}}.
#' @param resultdir a directory where Reca may store temp-files \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}. . If NULL, a temporary directory will be created. See documentation for \code{\link[Reca]{eca.estimate}}.
#' @param thin controls how many iterations are run between each samples saved. This may be set to account for autocorrelation introduced by Metropolis-Hastings simulation. see documentation for \code{\link[Reca]{eca.estimate}}
#' @param delta.age see documentation for \code{\link[Reca]{eca.estimate}}
#' @param seed see documentation for \code{\link[Reca]{eca.estimate}}
#' @param caa.burnin see documentation for \code{\link[Reca]{eca.predict}}
#' @return list() with elements:
#' \describe{
#'  \item{fit}{as returned by \code{\link[Reca]{eca.estimate}}}
#'  \item{prediction}{as returned by \code{\link[Reca]{eca.predict}}}
#'  \item{covariateMaps}{list() mapping from Reca covariate encoding to values fed to \code{\link[prepRECA]{prepRECA}}. As on parameter 'RecaObj'}
#' }
#' @examples
#'  \dontrun{runRECA(prepRECA::recaPrepExample, 500, 500)}
#' @export
runRECA <- function(RecaObj, nSamples, burnin, lgamodel="log-linear", fitfile="fit", predictfile="pred", resultdir=NULL, thin=10, delta.age=0.001, seed=NULL, caa.burnin=0){

  if (is.null(resultdir)){
    fpath <- file.path(tempdir(), "Recadir")
    if (dir.exists(fpath)){
      unlink(fpath, recursive = T)
    }
    dir.create(Recadir <- fpath)
    write("Tempfiles created at:", stderr())
    write(Recadir, stderr())
    resultdir <- Recadir
  }
  if (grepl(" ", resultdir)) {
    stop(paste(
      "Please make ecadir",
      "(current:",
      resultdir,
      ") contain no spaces."
    ))
  }

  tryCatch(
    {
      GlobalParameters <- RecaObj$GlobalParameters
      GlobalParameters$nSamples <- nSamples
      GlobalParameters$burnin <- burnin
      GlobalParameters$lgamodel <- lgamodel
      GlobalParameters$fitfile <- fitfile
      GlobalParameters$predictfile <- predictfile
      GlobalParameters$resultdir <- resultdir
      GlobalParameters$thin <- thin
      GlobalParameters$delta.age <- delta.age
      GlobalParameters$seed <- seed
      GlobalParameters$caa.burnin <- caa.burnin

      RecaObj$GlobalParameters <- GlobalParameters
      RecaObj <- checkEcaObj(RecaObj)

      fit <- Reca::eca.estimate(RecaObj$AgeLength, RecaObj$WeightLength, RecaObj$Landings, RecaObj$GlobalParameters)
      pred <- Reca::eca.predict(RecaObj$AgeLength, RecaObj$WeightLength, RecaObj$Landings, RecaObj$GlobalParameters)

      out <- list()
      out$fit <- fit
      out$prediction <- pred
      out$covariateMaps <- RecaObj$CovariateMaps
    },
    finally={
      if (is.null(resultdir)){
        unlink(Recadir, recursive = T)
        write("Removing tempdir:", stderr())
        write(Recadir, stderr())
        if (dir.exists(Recadir)){
          warning(paste("Could not remove tempdir: ", Recadir))
        }
      }
    }
  )

  #put col and rownames on Intercept$Cov, Slope$Cov
  #change name of catchsample in output

  return(out)
}

#' Data report for R-ECA preparation
#' @description
#'  Generates overview of samples to inform on sample availability in potential cell definitions.
#'  Informs on which columns maybe be used as fixed effect covariates, and how grouping of covariates is best done for random effects.
#'
#'  The covariates in landings define the cells.
#'  For each cell the covariates defining the cell is reported, before the total landed weight, along with the number of unique occurances of covariates not in landings (including catchId and sampleId).
#'  Lastly the number of fish measurements for Age, Weight and Length is reported.
#'
#' @param samples data.table() with samples (as in \code{\link[prepRECA]{prepRECA}}), each row corresponding to one sampled fish. Contains columns:
#'  \describe{
#'   \item{catchId}{Column identifying the catch that the sample was taken from. Typically a haul or a landing.}
#'   \item{sampleId}{Column identifying the sample. If only one sample is taken for each catch. This can be set equal to catchId}
#'   \item{Age}{integer() Age of fish.}
#'   \item{Length}{numeric() Length of fish. Must be complete (no NAs)}
#'   \item{Weight}{numeric() Weight of fish.}
#'   \item{...}{Additional columns which may be used as covariates.}
#'  }
#'
#' @param landings data.table() with total landings (as in \code{\link[prepRECA]{prepRECA}}), each row corresponding to one cell. Contains columns:
#' \describe{
#'  \item{LiveWeightKG}{numeric(). Total landings (Live/Round weight in Kg) for the cell}
#'  \item{...}{Additional columns which may be used as covariates. These will define each cell.}
#' }
#' @param covariates character() vector of columns to consider for covariates.
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
#' @examples
#'  samples <- extractBV(prepRECA::NORportsampling2018$BV, c("Age", "Length", "Weight"), c("integer", "numeric", "numeric"))
#'  samples <- merge(samples, prepRECA::NORportsampling2018$SA, by="SAid")
#'  samples <- merge(samples, prepRECA::NORportsampling2018$SS, by="SSid")
#'  samples <- merge(samples, prepRECA::NORportsampling2018$LE, by="LEid", suffixes = c("", ".LE"))
#'  samples <- merge(samples, prepRECA::NORportsampling2018$VD, by="VDid")
#'  samples$catchId <- samples$LEid
#'  samples$sampleId <- samples$SAid
#'  samples$date <- samples$LEdate
#'  samples$Metier5 <- samples$LEmetier5
#'  landings <- prepRECA::CLCodHadNOR
#'  landings$LiveWeightKG <- landings$OfficialLandingsWeight
#'  landings$Metier5 <- landings$FishingActivityCategoryEuropeanLvl5
#'  rEcaDataReport(samples, landings, c("Metier5", "VDencrCode"))
#' @export
rEcaDataReport <- function(samples, landings, covariates){
  # check mandatory columns
  if (!(all(c("LiveWeightKG") %in% names(landings)))){
    stop("Column LiveWeightKG is mandatory in landings")
  }
  if (!(all(c("catchId", "sampleId", "date", "Age", "Length", "Weight") %in% names(samples)))){
    stop(paste("Columns, catchId, sampleId, date, Age, Length, and Weight are mandatory in samples. Missing:", paste(c("catchId", "sampleId", "date", "Age", "Length", "Weight")[(!c("catchId", "sampleId", "date", "Age", "Length", "Weight") %in% names(samples))], collapse=",")))
  }

  if (length(unique(samples$sampleId)) != nrow(unique(samples[, c("catchId", "sampleId")]))){
    stop("sampleID must be unique identifier, irrespective of catchId")
  }

  if (!all(covariates %in% names(samples))){
    stop("Only columns that exist in samples may be used as covariates")
  }

  insamples <- covariates

  inlandings <- covariates[covariates %in% names(landings)]

  if (length(inlandings) == 0){
    stop("No covariates in landings. Cannot produce report.")
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
    out <- merge(out, agg, all=T)
  }

  out <- merge(out, nDs, all=T)
  out <- merge(out, nCs, all=T)
  out <- merge(out, nSs, all=T)

  out <- merge(out, nAges, all=T)
  out <- merge(out, nWeight, all=T)
  out <- merge(out, nLength, all=T)

  if (any(is.na(out$LiveWeightKG))){
    warning("Some cells are sampled, but does not occur in landings.")
  }

  # NAs come from cells not present in samples
  out[is.na(out)]<-0
  out <- out[order(out$LiveWeightKG, decreasing = T),]
  out <- data.table::as.data.table(out)
  return(out)
}
