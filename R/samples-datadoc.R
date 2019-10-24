#' Data from Norwegian port sampling program.
#'
#' Norwegian port sampling for landings in 2018 of fresh whitefish north of Lat 64, covering Jig, Gillnet, Longline, and Demershal Seining.
#' Sampling is implemented by a research vessel systematically sampling ports accepting fresh fish along the northern Norwegian coast.
#' Landings are almost exclusively day-catches located to small areas.
#' Vessels are intercepted during landing, and samples of up to 30 fish are taken stratified by gear and species.
#' Length, weight and age measured for all fish. Sex and maturity when presentation allows for it.
#' Weight of individual fish is reported as measured (presentation encoded in SApres), while total weight of landing and sample is converted to Whole-weight
#' using constant factors standardized by the Norwegian Directorate of Fisheries (See table 2.1 in \href{https://www.fiskeridir.no/Yrkesfiske/Tall-og-analyse/Omregningsfaktorer}{Conversion factors}. Document is mostly in Norwegian, but the relevant table is in English as well).
#'
#'
#' @docType data
#'
#' @usage data(NORportsampling2018)
#'
#' @format RDBES data model version 1.17 Hierarchy 5 (\href{https://github.com/ices-tools-dev/RDBES/tree/1a17e09d34d3e9b6bb6173f2b33e10f0830d4cf3}{RDBES data model v 1.17})
#'
#' @keywords datasets
#'
#' @examples
#' data(NORportsampling2018)
#' table(NORportsampling2018$SA$SApres)
"NORportsampling2018"

