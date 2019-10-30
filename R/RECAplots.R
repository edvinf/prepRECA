#' Catch at Age plot
#' @description
#'  Plots estimated total catch at age for each age group.
#'  Constructs equal tailed credibility intervals (The probability mass above and below the interval is approximatelty the same).
#' @details
#'  parameter 'unit' supports:
#'  \describe{
#'   \item{number}{Catch at age as number of fish}
#'   \item{thousands}{Catch at age as number of fish in thousands}
#'   \item{millions}{Catch at age as number of fish in millions}
#'   \item{kg}{Catch at age as mass in kilogrammes}
#'   \item{T}{Catch at age as mass in tons}
#'   \item{kT}{Catch at age as mass in kilotonnes}
#'  }
#' @param prediction as returned by \code{\link[Reca]{eca.predict}} or \code{\link[prepRECA]{runRECA}}.
#' @param unit unit of reported estimates. See details.
#' @param plusGroup Fish this age or older will be grouped in plot
#' @param credibility The desired credibility for credibility intervals.
#' @param title Title for plot
#' @export
plotCatchAtAge <- function(prediction, unit="millions", plusGroup=NULL, credibility=.95, title=NULL){

  if (is.null(title)){
    title <- paste("Catch At Age, ", credibility*100, "% CI", sep="")
  }

  resTab <- makeResultTableRECA(prediction, unit, plusGroup, alpha=(1-credibility))
  resTab$order <- 1:nrow(resTab)
  ggplot2::ggplot(resTab, ggplot2::aes(x=reorder(age, order), y=total)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=lowerQuantile, ymax=upperQuantile), width=.1) +
    ggplot2::geom_point() +
    ggplot2::xlab("Age") +
    ggplot2::ylab(paste("Total catch (", unit, ")", sep="")) +
    ggplot2::ggtitle(title) +
    ggplot2::theme_minimal()
}
