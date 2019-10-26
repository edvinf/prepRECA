
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
#'   \item{CatchSampleId}{Column identifying each catch sample. Typically a haul or a landing.}
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
#' @export
prepRECA <- function(samples, landings, fixedeffects, randomeffects, careffect, neighbours=NULL){
  # check mandatory columns
  if (!(all(c("LiveWeightKG", "midseason") %in% names(landings)))){
    stop("Columns LiveWeightKG and midseason are mandatory in landings")
  }
  if (!(all(c("CatchSampleId", "Age", "Length", "Weight") %in% names(samples)))){
    stop("Columns, CatchSampleId, Age, Length, and Weight are mandatory in landings")
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
  if(!(all(names(samples) %in% c(fixedeffects, randomeffects, careffect, c("CatchSampleId", "Age", "Length", "Weight"))))){
    stop(paste("Effect not specified for covariates:", paste(names(samples)[!(names(samples) %in% c(fixedeffects, randomeffects, careffect, c("CatchSampleId", "Age", "Length", "Weight")))], collapse=",")))
  }
  if(!(all(names(landings) %in% c(fixedeffects, randomeffects, careffect, c("LiveWeightKG", "midseason"))))){
    stop(paste("Effect not specified for covariates:", paste(names(landings)[!(names(landings) %in% c(fixedeffects, randomeffects, careffect, c("LiveWeightKG", "midseason")))], collapse=",")))
  }

  # Map covariate levels

  # build eca objects

  # info matrix: interaction and in.landings based on landings

  #
  # Test error checks, including empty lists
  #

}
