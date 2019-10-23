#
# Move out of package to avoid non-ASCII characters (from LSS format) ?
#

#' @noRd
#' @keywords internal
getMonth <- function(date){
  return(as.integer(substr(date, 4,5)))
}

#' @noRd
#' @keywords internal
getQuarter <- function(month){
  return(c(1,1,1,2,2,2,3,3,3,4,4,4)[month])
}

#' @noRd
#' @keywords internal
#' @return LSS with area annotation
getAreas <- function(lss){
  if (!all(lss$`Hovedområde (kode)` %in% conversionTables$areaCodes$Homr)){
    stop("Area mapping not provided for all areas")
  }
  if (!all(lss$`Lokasjon (kode)` %in% conversionTables$areaCodes$lokasjon)){
    stop("Area mapping not provided for all locations")
  }
  lss <- merge(lss, conversionTables$areaCodes, by.x=c("Hovedområde (kode)", "Lokasjon (kode)"), by.y=c("Homr", "lokasjon"))
  return(lss)
}

#' @noRd
#' @keywords internal
getSubPolygon <- function(omr, loc){
  return(paste(omr,loc,sep="-"))
}

#' @noRd
#' @keywords internal
getLandingCategory <- function(anvendelse){
  if (!all(anvendelse[!is.na(anvendelse)] %in% conversionTables$landingCategoryCodes$anvhgr)){
    stop("Some landing category not supported")
  }
  return(unlist(conversionTables$landingCategoryCodes[match(anvendelse, conversionTables$landingCategoryCodes$anvhgr),"landingCategory"]))
}

#' @param lss lss with aphia annotated
#' @return lss with column FishingActivityCategoryEuropeanLvl5 added
#' @noRd
#' @keywords internal
getMetier5 <- function(lss){

  if (!("aphia" %in% names(lss))){
    stop("Need aphia annotation")
  }
  if (!(all(lss$`Redskap (kode)` %in% conversionTables$gearCodes$FDIRgear))){
    stop("Mappng not defined for all gears")
  }

  lss <- merge(lss, conversionTables$gearCodes, by.x="Redskap (kode)", by.y="FDIRgear", all.x=T)


  if (any(lss$aphia %in% conversionTables$speciesGearAssemblage$aphia)){
    lsswGdep <- lss[,lss$aphia %in% conversionTables$speciesGearAssemblage$aphia]
    if (!all(lsswGdep[["Redksap (kode)"]] %in% conversionTables$speciesGearAssemblage$FDIRgear)){
      stop("Not all gear specified for species with gear dependent assemblage")
    }
    lss <- merge(lss, conversionTables$speciesGearAssemblage, by.x=c("aphia", "Redskap (kode)", by.y=c("aphia", "FDIRgear")), all.x=T)
    #override any gear dependent assemblage
    lss[!is.na(lss$gearDepAssemblage), "assemblage"] <- lss[!is.na(lss$gearDepAssemblage), "gearDepAssemblage"]
  }

  lss <- merge(lss, conversionTables$metierlvl5Codes, by.x=c("FAO1980gear", "assemblage"), by.y=c("FAO1980gear", "assemblage"), all.x=T)

  if (any(is.na(lss$FishingActivityCategoryEuropeanLvl5))){
    stop("Metier lvl 5 could not be annotated for all landings")
  }

  return(lss)
}

#' @noRd
#' @keywords internal
getMetier6 <- function(lss){
  return(NA)
}

#' @noRd
#' @keywords internal
getVesselLengthCategory <- function(storste_lengde){

  if (is.na(storste_lengde)){
    return(NA)
  }

  if (storste_lengde < 12){
    return("VL0012")
  }
  else if (storste_lengde < 24){
    return("VL1224")
  }
  else if (storste_lengde < 40){
    return("VL2440")
  }
  else{
    return("VL40XX")
  }

}
getVesselLengthCategory <- Vectorize(getVesselLengthCategory)

#' Convert landings data to data.table with columns matching RDB CL
#' @param landingLss landings to export formatted according to the LSS format provided by the Norwegian Directorate of Fisheries.
#' @return data.table columns matching RDB CL
#' @noRd
#' @keywords internal
convertCL <- function(landingsLss){

  landingsLss <- merge(landingsLss, conversionTables$speciesCodes[,c("aphia", "FDIR", "assemblage")], by.x="Art - FDIR (kode)", by.y="FDIR", all.x=T)
  if (any(is.na(landingsLss$aphia))){
    stop("Incomplete aphia mapping")
  }

  landingsLss$Month <- getMonth(landingsLss$`Siste fangstdato`)
  landingsLss$Quarter <- getQuarter(landingsLss$Month)
  landingsLss <- getAreas(landingsLss)
  landingsLss$Subpolygon <- getSubPolygon(landingsLss$`Hovedområde (kode)`, landingsLss$`Lokasjon (kode)`)
  landingsLss$LandingCategory <- getLandingCategory(landingsLss$`Anvendelse hovedgruppe (kode)`)
  landingsLss <- getMetier5(landingsLss)
  warning("getMetier6 not implemented")
  landingsLss$FishingActivityCategoryEuropeanLvl6 <- getMetier6(landingsLss)
  landingsLss$VesselLengthCategory <- getVesselLengthCategory(landingsLss$`Største lengde`)

  #add inn constant columns
  landingsLss$CommercialSizeCategoryScale <- NA
  landingsLss$CommercialSizeCategory <- NA
  landingsLss$FishingActivityCategoryNational <- NA
  landingsLss$Harbour <- NA
  landingsLss$AreaMisreportedCatchWeight <- 0
  landingsLss$UnallocatedCatchWeight <- 0
  landingsLss$LandingsMultiplier <- 1
  landingsLss$OfficialLandingsValue <- NA

  lssNames <- c("Landingsnasjon (kode)", "Fartøynasjonalitet (kode)","Fangstår", "Quarter", "Month","ICESArea","StatRect", "Subpolygon", "aphia", "LandingCategory", "CommercialSizeCategoryScale","CommercialSizeCategory","FishingActivityCategoryNational", "FishingActivityCategoryEuropeanLvl5", "FishingActivityCategoryEuropeanLvl6", "Harbour", "VesselLengthCategory", "UnallocatedCatchWeight", "AreaMisreportedCatchWeight", "Rundvekt", "LandingsMultiplier", "OfficialLandingsValue")
  rdbNames <- c("LandingCountry", "VesselFlagCountry", "Year", "Quarter", "Month","Area", "StatisticalRectange", "Subpolygon", "Species", "LandingCategory", "CommercialSizeCategoryScale","CommercialSizeCategory","FishingActivityCategoryNational","FishingActivityCategoryEuropeanLvl5", "FishingActivityCategoryEuropeanLvl6", "Harbour", "VesselLengthCategory", "UnallocatedCatchWeight", "AreaMisreportedCatchWeight", "OfficialLandingsWeight", "LandingsMultiplier", "OfficialLandingsValue")
  landingsLss <- landingsLss[,lssNames]
  names(landingsLss) <- rdbNames
  return(landingsLss)
}

#' Standard aggregation of CL data
#' @param CLdata CL data as returned by convertCL
#' @return aggregated CLdata CL data formatted as CLdata
#' @noRd
#' @keywords internal
aggregateCL <- function(CLdata){
  mustbecomplete <- c("LandingCountry", "VesselFlagCountry", "Year", "Quarter", "Month", "Species")
  if (any(is.na(CLdata[,mustbecomplete]))){
    stop(paste("Does not accept NAs in:", paste(mustbecomplete, collapse = ",")))
  }
  aggcolumnnames <- c("LandingCountry", "VesselFlagCountry", "Year", "Quarter", "Month", "Area", "Subpolygon", "Species", "LandingCategory", "CommercialSizeCategoryScale", "CommercialSizeCategory", "FishingActivityCategoryNational", "FishingActivityCategoryEuropeanLvl5", "FishingActivityCategoryEuropeanLvl6", "Harbour", "VesselLengthCategory")
  for (a in aggcolumnnames){
    CLdata[is.na(CLdata[[a]]),a] <- "notNA"
  }

  aggcolumns <- list(LandingCountry=CLdata$LandingCountry, VesselFlagCountry=CLdata$VesselFlagCountry, Year=CLdata$Year, Quarter=CLdata$Quarter, Month=CLdata$Month, Area=CLdata$Area, Subpolygon=CLdata$Subpolygon, Species=CLdata$Species, LandingCategory=CLdata$LandingCategory, CommercialSizeCategoryScale=CLdata$CommercialSizeCategoryScale, CommercialSizeCategory=CLdata$CommercialSizeCategory, FishingActivityCategoryNational=CLdata$FishingActivityCategoryNational, FishingActivityCategoryEuropeanLvl5=CLdata$FishingActivityCategoryEuropeanLvl5, FishingActivityCategoryEuropeanLvl6=CLdata$FishingActivityCategoryEuropeanLvl6, Harbour=CLdata$Harbour, VesselLengthCategory=CLdata$VesselLengthCategory)
  if (length(unique(CLdata$LandingsMultiplier))!=1){
    stop("Cannot aggregate cells with heterogenous landings multiplier")
  }

  landmult <- aggregate(list(LandingsMultiplier=CLdata$LandingsMultiplier), by=aggcolumns, FUN=median)
  unnacc <- aggregate(list(UnallocatedCatchWeight=CLdata$UnallocatedCatchWeight), by=aggcolumns, FUN=sum)
  misrep <- aggregate(list(AreaMisreportedCatchWeight=CLdata$AreaMisreportedCatchWeight), by=aggcolumns, FUN=sum)
  offw <- aggregate(list(OfficialLandingsWeight=CLdata$OfficialLandingsWeight), by=aggcolumns, FUN=sum)
  offv <- aggregate(list(OfficialLandingsValue=CLdata$OfficialLandingsValue), by=aggcolumns, FUN=sum)

  tab <- merge(landmult, unnacc)
  tab <- merge(tab, misrep)
  tab <- merge(tab, offw)
  tab <- merge(tab, offv)

  for (a in aggcolumnnames){
    tab[tab[,a]=="notNA",a] <- NA
  }

  return(as.data.table(tab))
}
