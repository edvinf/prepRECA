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
getArea <- function(omr, loc){
  return(NA)
}

#' @noRd
#' @keywords internal
getStatRect <- function(omr, loc){
  return(NA)
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

#' @noRd
#' @keywords internal
getMetier5 <- function(lss){
  return(NA)
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
  landingsLss <- merge(landingsLss, conversionTables$speciesCodes[,c("aphia", "FDIR")], by.x="Art - FDIR (kode)", by.y="FDIR", all.x=T)
  if (any(is.na(landingsLss$aphia))){
    stop("Incomplete aphia mapping")
  }

  landingsLss$Month <- getMonth(landingsLss$`Siste fangstdato`)
  landingsLss$Quarter <- getQuarter(landingsLss$Month)
  warning("getArea not implemented")
  landingsLss$Area <- getArea(landingsLss$`Hovedområde (kode)`, landingsLss$`Lokasjon (kode)`)
  warning("getStatRect not implemented")
  landingsLss$StatisticalRectangle <- getStatRect(landingsLss$`Hovedområde (kode)`, landingsLss$`Lokasjon (kode)`)
  landingsLss$Subpolygon <- getSubPolygon(landingsLss$`Hovedområde (kode)`, landingsLss$`Lokasjon (kode)`)
  warning("getLandingCategory not implemented")
  landingsLss$LandingCategory <- getLandingCategory(landingsLss$`Anvendelse hovedgruppe (kode)`)
  warning("getMetier5 not implemented")
  landingsLss$FishingActivityCategoryEuropeanLvl5 <- getMetier5(landingsLss)
  warning("getMetier6 not implemented")
  landingsLss$FishingActivityCategoryEuropeanLvl6 <- getMetier6(landingsLss)
  warning("getVesselLengthCategory not implemented")
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

  lssNames <- c("Landingsnasjon (kode)", "Fartøynasjonalitet (kode)","Fangstår", "Quarter", "Month","Area","Subpolygon", "aphia", "LandingCategory", "CommercialSizeCategoryScale","CommercialSizeCategory","FishingActivityCategoryNational", "FishingActivityCategoryEuropeanLvl5", "FishingActivityCategoryEuropeanLvl6", "Harbour", "VesselLengthCategory", "UnallocatedCatchWeight", "AreaMisreportedCatchWeight", "Rundvekt", "LandingsMultiplier", "OfficialLandingsValue")
  rdbNames <- c("LandingCountry", "VesselFlagCountry", "Year", "Quarter", "Month","Area","Subpolygon", "Species", "LandingCategory", "CommercialSizeCategoryScale","CommercialSizeCategory","FishingActivityCategoryNational","FishingActivityCategoryEuropeanLvl5", "FishingActivityCategoryEuropeanLvl6", "Harbour", "VesselLengthCategory", "UnallocatedCatchWeight", "AreaMisreportedCatchWeight", "OfficialLandingsWeight", "LandingsMultiplier", "OfficialLandingsValue")
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

#' Create conversion tables for LSS -> RDB conversion
#' @noRd
#' @keywords internal
create_conversion_tables <- function(){
  conversionTables <- list()
  speciesCodes <- data.table(aphia=character(), FDIR=character(), FAO=character(), norwegianCommonName=character())
  speciesCodes <- rbind(speciesCodes, data.table(aphia=as.character("126439"), FDIR=as.character("1038"), FAO=as.character("WHB"), norwegianCommonName=as.character("Kolmule")))
  speciesCodes <- rbind(speciesCodes, data.table(aphia=as.character("126417"), FDIR=as.character("061101"), FAO=as.character("HER"), norwegianCommonName=as.character("Norsk vårgytende sild")))
  speciesCodes <- rbind(speciesCodes, data.table(aphia=as.character("126735"), FDIR=as.character("075101"), FAO=as.character("CAP"), norwegianCommonName=as.character("Barentshavslodde")))
  conversionTables$speciesCodes <- speciesCodes

  landingCategoryCodes <- data.table(anvhgr=integer(), landingCategory=character(), norwegianLandingCategryName=character())
  landingCategoryCodes <- rbind(landingCategoryCodes, data.table(anvhgr=as.integer(1), landingCategory=as.character("HUC"), norwegianLandingCategryName=as.character("Konsum")))
  landingCategoryCodes <- rbind(landingCategoryCodes, data.table(anvhgr=as.integer(2), landingCategory=as.character("IND"), norwegianLandingCategryName=as.character("Mel og olje")))
  landingCategoryCodes <- rbind(landingCategoryCodes, data.table(anvhgr=as.integer(3), landingCategory=as.character("IND"), norwegianLandingCategryName=as.character("Dyrefor/fiskefor, agn og annet")))
  conversionTables$landingCategoryCodes <- landingCategoryCodes

  return(conversionTables)
}

