#' @noRd
getPlusGr <- function(prediction, unit, plusGroup){

  TotalCount <- prediction$TotalCount
  MeanWeight <- prediction$MeanWeight
  ages <- as.character(prediction$AgeCategories)
  sortcol <- prediction$AgeCategories

  #aggregate on plusgroup
  if (!is.null(plusGroup)){

    if (plusGroup >= max(prediction$AgeCategories)){
      stop("plusGroup can not be larger than or equal to highest age in prediction.")
    }
    if (plusGroup <= min(prediction$AgeCategories)){
      stop("plusGroup can not be smaller than or equal to lowest age in prediction.")
    }

    plusGroupIndex <- match(plusGroup, prediction$AgeCategories)

    # weighted means for mean individual weight
    # Stats are calcuated for each iteration
    # TotWeightPrAgeGroupInPlus is Total Weight Pr Age Group In Plus for all iterations
    TotWeightPrAgeGroupInPlus <- apply(TotalCount[,plusGroupIndex:length(ages),], c(2, 3), sum) * MeanWeight[plusGroupIndex:length(ages),]
    stopifnot(dim(TotWeightPrAgeGroupInPlus)[[1]] == c(length(ages) - plusGroupIndex + 1))
    stopifnot(dim(TotWeightPrAgeGroupInPlus)[[2]] == dim(TotalCount)[[3]])
    TotWeightPlusGroup <- apply(TotWeightPrAgeGroupInPlus, c(2), sum)
    stopifnot(length(TotWeightPlusGroup) == dim(TotalCount)[[3]])
    TotCountPlusGroup <- apply(TotalCount[,plusGroupIndex:length(ages),], c(3), sum)

    MeanWeight[plusGroupIndex,] <- TotWeightPlusGroup / TotCountPlusGroup
    MeanWeight <- MeanWeight[1:plusGroupIndex,]

    #sum for totals
    TotalCount[,plusGroupIndex,] <- apply(TotalCount[,plusGroupIndex:length(ages),], c(1, 3), sum)
    TotalCount <- TotalCount[,1:plusGroupIndex,]

    #correct names and sorting column
    ages <- ages[1:plusGroupIndex]
    ages[plusGroupIndex] <- paste(ages[plusGroupIndex], "+", sep="")
    sortcol <- sortcol[1:plusGroupIndex]
  }

  if (unit == "number"){
    caa <- apply(TotalCount, c(2, 3), sum)
  }
  else if (unit == "thousands"){
    caa <- apply(TotalCount, c(2, 3), sum) / 1e3
  }
  else if (unit == "millions"){
    caa <- apply(TotalCount, c(2, 3), sum) / 1e6
  }
  else if (unit == "kg"){
    caa <- apply(TotalCount, c(2, 3), sum) * MeanWeight
  }
  else if (unit == "T"){
    caa <- (apply(TotalCount, c(2, 3), sum) * MeanWeight) / 1e3
  }
  else if (unit == "kT"){
    caa <- (apply(TotalCount, c(2, 3), sum) * MeanWeight) / 1e6
  }
  else{
    stop(paste("Unit", unit, "not supported."))
  }

  pl <- list()
  pl$caa <- caa
  pl$TotalCount <- TotalCount
  pl$MeanWeight <- MeanWeight
  pl$ages <- ages
  pl$sortcol <- sortcol

  return(pl)
}

#' Catch at Age result table
#' @description
#'  Produces common reported statistics for Catch-At-Age estimation.
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
#' @param plusGroup Fish this age or older will be grouped in report
#' @param alpha value for percentiles.
#' @return data.table() with columns:
#' \describe{
#'  \item{age}{Age statistics are reported for}
#'  \item{total}{Estimated total catch of age group}
#'  \item{unit}{Unit for total catch and standard deviation for estimated total}
#'  \item{sd}{Standard deviation for estimated total}
#'  \item{cv}{Coefficient of variation for estimated total}
#'  \item{lowerQuantile}{Lower quantile (100*alpha/2 percentile) for estimated total}
#'  \item{UpperQuantile}{Upper quantile: (100*(1-alpha/2) percentile) for estimated total}
#'  \item{alpha}{alpha for quantiles.}
#' }
#' @examples
#'  makeResultTableRECA(prepRECA::recaRunExample$prediction)
#' @export
makeResultTableRECA <- function(prediction, unit="millions", plusGroup=NULL, alpha=.05){

  pl <- getPlusGr(prediction, unit, plusGroup)

  TotalCount <- pl$TotalCount
  MeanWeight <- pl$MeanWeight
  ages <- pl$ages
  sortcol <- pl$sortcol
  caa <- pl$caa

  meanList <- list(age = as.character(ages), total = rowMeans(caa), unit = rep(unit, length(ages)), sortcol = sortcol)
  means <-
    data.table::as.data.table(meanList)
  cv <-
    data.table::as.data.table(list(
      age = ages,
      sd = apply(caa, FUN = sd, MARGIN = 1)
    ))
  cv$cv <- cv$sd / means[[names(meanList)[[2]]]]

  tab <- merge(means, cv)

  quantiles <-
    data.table::as.data.table(list(
      age = ages,
      upperQuantile = apply(caa, FUN = quantile, MARGIN = 1,  1-(alpha/2.0)),
      lowerQuantile = apply(caa, FUN = quantile, MARGIN = 1, (alpha/2.0))
    ))

  tab <- merge(means, quantiles)
  tab$alpha <- alpha


  tab <- data.table::as.data.table(tab)
  tab <- tab[order(tab$sortcol),]
  tab$sortcol <- NULL
  return(tab)
}

#' Catch at age traces
#' @description
#'  Compiles traces for age groups. That is the value for total catch at age in each iteration of \code{\link[Reca]{eca.predict}}
#' @details
#'  The number of iterations of \code{\link[Reca]{eca.predict}}
#'  is determined by the parameter 'nSamples' to \code{\link[Reca]{eca.estimate}} or \code{\link[prepRECA]{runRECA}}
#'  and the parameter 'caa.burnin' (different from parameter 'burnin') to \code{\link[Reca]{eca.predict}} or \code{\link[prepRECA]{runRECA}}
#'
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
#' @param unit unit of traced estimates. See details.
#' @param plusGroup Fish this age or older will be grouped in one trace.
#' @return data.table() with one column for each age (or plus group), and one row for each iteration of \code{\link[Reca]{eca.predict}}.
#' @examples
#'  makeAgeTracesRECA(prepRECA::recaRunExample$prediction)
#' @export
makeAgeTracesRECA <- function(prediction, unit="millions", plusGroup=NULL){
  pl <- getPlusGr(prediction, unit, plusGroup)
  ages <- pl$ages

  caa <- pl$caa
  caa <- t(caa)
  colnames(caa) <- ages

  return(data.table::as.data.table(caa))
}

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
#' @examples
#' plotCatchAtAge(prepRECA::recaRunExample$prediction)
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

#' Traceplot
#' @description
#'  Plots a paneled plot with traces for age groups. That is the value for total catch at age in each iteration of \code{\link[Reca]{eca.predict}}
#' @details
#'  The number of iterations of \code{\link[Reca]{eca.predict}}
#'  is determined by the parameter 'nSamples' to \code{\link[Reca]{eca.estimate}} or \code{\link[prepRECA]{runRECA}}
#'  and the parameter 'caa.burnin' (different from parameter 'burnin') to \code{\link[Reca]{eca.predict}} or \code{\link[prepRECA]{runRECA}}
#'
#'  parameter 'unit' supports:
#'  \describe{
#'   \item{number}{Catch at age as number of fish}
#'   \item{thousands}{Catch at age as number of fish in thousands}
#'   \item{millions}{Catch at age as number of fish in millions}
#'   \item{kg}{Catch at age as mass in kilogrammes}
#'   \item{T}{Catch at age as mass in tons}
#'   \item{kT}{Catch at age as mass in kilotonnes}
#'  }
#'
#'  In order to trade off readability of plots with the number of panels needed. the traces are clustered with a simple clustering algorithm
#'  and grouped in the same plots accordingly. Adjust clustering parameters, to get fewer or more plots.
#'
#' @param prediction as returned by \code{\link[Reca]{eca.predict}} or \code{\link[prepRECA]{runRECA}}.
#' @param unit unit of traced estimates. See details.
#' @param plusGroup Fish this age or older will be grouped in one trace.
#' @param nclust the number of plots to distribute the ages and plus group on
#' @param iter.max maximal number of iterations for k-means clustering deciding which ages are ploted in same plot.
#' @param nstart the number of random sets chosen for the k-means clustering
#' @param agecolors named vector matching ages to colors, if null a default color scheme is used
#' @param lowerquant lower quantile in each age group to plot as points
#' @param upperquant upper quantile in each age group to plot as points
#' @param catlimit the upper limit for number of ages in a plot using categorical coloring. Plots with more than this number of ages will use a gradient coloring scheme
#' @param title main title for plot
#' @examples
#'  plotAgeTraces(prepRECA::recaRunExample$prediction)
#' @export
plotAgeTraces <- function(prediction, unit="millions", plusGroup=NULL, nclust=4, iter.max=20, nstart=10, agecolors=NULL, lowerquant=.05, upperquant=.95, catlimit=8, title=""){

  traces <- makeAgeTracesRECA(prediction, unit, plusGroup)
  ages <- names(traces)
  sortcol <- 1:length(ages)
  traces <- t(traces)

  if (nclust > length(ages)){
    stop("Choose nclust to be lower than the number of age group traces to plot.")
  }

  if (is.null(agecolors)){
    agecolors <- c(RColorBrewer::brewer.pal(8, "Accent"), RColorBrewer::brewer.pal(9, "Set1"), RColorBrewer::brewer.pal(8, "Dark2"), RColorBrewer::brewer.pal(8, "Set3"))
    agecolors <- agecolors[sortcol]
    agecolors <- setNames(agecolors, ages)
  }

  means <- apply(traces, FUN=mean, MARGIN=1)
  lq<-apply(traces, MARGIN=1, FUN=function(x){quantile(x, lowerquant)})
  names(lq) <- ages
  uq<-apply(traces, MARGIN=1, FUN=function(x){quantile(x, upperquant)})
  names(uq) <- ages


  #clustering ages in plots. kemans on log(means) seems to work well, but sometimes failes due to 0 means, which is avoided by adding lowest non-zero mean
  llo <- min(means[means>0])
  clust <- kmeans(log(means+llo), nclust, iter.max = iter.max, nstart = nstart)
  m <- reshape2::melt(traces, c("age", "iteration"), value.name=unit)
  m <- merge(m, data.frame(age=names(lq), lq=lq))
  m <- merge(m, data.frame(age=names(uq), uq=uq))

  plots <- list()
  plotnr <- 1
  for (i in seq(1,nclust)[order(clust$centers, decreasing = T)]){
    mcp <- m[m$age %in% ages[clust$cluster==i],]
    maxy <- max(mcp[unit]) + max(mcp[unit])*.1
    if (sum(clust$cluster==i)<=catlimit){
      mcp$age <- as.factor(mcp$age)
      plots[[plotnr]]<-ggplot2::ggplot(data=mcp, ggplot2::aes_string(x="iteration", y=unit, group="age"))+ggplot2::geom_line(data=mcp, ggplot2::aes(color=age)) + ggplot2::geom_point(data=mcp[mcp[unit] > mcp$uq | mcp[unit] < mcp$lq,], ggplot2::aes(color=age)) + ggplot2::scale_color_manual(values = agecolors) + ggplot2::ylim(0,maxy)
    }
    else{
      plots[[plotnr]]<-ggplot2::ggplot(data=mcp, ggplot2::aes_string(x="iteration", y=unit, group="age"))+ggplot2::geom_line(data=mcp, ggplot2::aes(color=age)) + ggplot2::geom_point(data=mcp[mcp[unit] > mcp$uq | mcp[unit] < mcp$lq,], ggplot2::aes(color=age)) + ggplot2::ylim(0,maxy)
    }

    plotnr <- plotnr+1
  }
  gridExtra::grid.arrange(grobs=plots, top=grid::textGrob(title,gp=grid::gpar(fontsize=20,font=1)), ncol=2)

}
