library(data.table)
library(rgdal)
library(prepRECA)


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
  if (!all(lss$`Hovedområde (kode)` %in% prepRECA:::conversionTables$areaCodes$Homr)){
    stop("Area mapping not provided for all areas")
  }
  if (!all(lss$`Lokasjon (kode)` %in% prepRECA:::conversionTables$areaCodes$lokasjon)){
    stop("Area mapping not provided for all locations")
  }
  lss <- merge(lss, prepRECA:::conversionTables$areaCodes, by.x=c("Hovedområde (kode)", "Lokasjon (kode)"), by.y=c("Homr", "lokasjon"))
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
  if (!all(anvendelse[!is.na(anvendelse)] %in% prepRECA:::conversionTables$landingCategoryCodes$anvhgr)){
    stop("Some landing category not supported")
  }
  return(unlist(prepRECA:::conversionTables$landingCategoryCodes[match(anvendelse, prepRECA:::conversionTables$landingCategoryCodes$anvhgr),"landingCategory"]))
}

#' @param lss lss with aphia annotated
#' @return lss with column FishingActivityCategoryEuropeanLvl5 added
#' @noRd
#' @keywords internal
getMetier5 <- function(lss){

  if (!("aphia" %in% names(lss))){
    stop("Need aphia annotation")
  }
  if (!(all(lss$`Redskap (kode)` %in% prepRECA:::conversionTables$gearCodes$FDIRgear))){
    stop("Mappng not defined for all gears")
  }

  lss <- merge(lss, prepRECA:::conversionTables$gearCodes, by.x="Redskap (kode)", by.y="FDIRgear", all.x=T)


  if (any(lss$aphia %in% prepRECA:::conversionTables$speciesGearAssemblage$aphia)){
    lsswGdep <- lss[,lss$aphia %in% prepRECA:::conversionTables$speciesGearAssemblage$aphia]
    if (!all(lsswGdep[["Redksap (kode)"]] %in% prepRECA:::conversionTables$speciesGearAssemblage$FDIRgear)){
      stop("Not all gear specified for species with gear dependent assemblage")
    }
    lss <- merge(lss, prepRECA:::conversionTables$speciesGearAssemblage, by.x=c("aphia", "Redskap (kode)", by.y=c("aphia", "FDIRgear")), all.x=T)
    #override any gear dependent assemblage
    lss[!is.na(lss$gearDepAssemblage), "assemblage"] <- lss[!is.na(lss$gearDepAssemblage), "gearDepAssemblage"]
  }

  lss <- merge(lss, prepRECA:::conversionTables$metierlvl5Codes, by.x=c("FAO1980gear", "assemblage"), by.y=c("FAO1980gear", "assemblage"), all.x=T)

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

  landingsLss <- merge(landingsLss, prepRECA:::conversionTables$speciesCodes[,c("aphia", "FDIR", "assemblage")], by.x="Art - FDIR (kode)", by.y="FDIR", all.x=T)
  if (any(is.na(landingsLss$aphia))){
    stop("Incomplete aphia mapping")
  }

  landingsLss$Fangstår <- as.integer(landingsLss$Fangstår)
  landingsLss$Month <- as.integer(getMonth(landingsLss$`Siste fangstdato`))
  landingsLss$Quarter <- as.integer(getQuarter(landingsLss$Month))
  landingsLss <- getAreas(landingsLss)
  landingsLss$Subpolygon <- getSubPolygon(landingsLss$`Hovedområde (kode)`, landingsLss$`Lokasjon (kode)`)
  landingsLss$LandingCategory <- getLandingCategory(landingsLss$`Anvendelse hovedgruppe (kode)`)
  warning("target assemblage not properly implemented")
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
  landingsLss$Rundvekt <- as.integer(round(landingsLss$Rundvekt))

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

  tab$Month <- as.integer(tab$Month)
  tab$Year <- as.integer(tab$Year)
  tab$Quarter <- as.integer(tab$Quarter)

  return(as.data.table(tab))
}


#' Create conversion tables for LSS -> RDB conversion
#' Relies on resources external to the package. Used for updating internal tables (R/sysdata.R):
#'  conversionTables <- create_conversion_tables()
#'  usethis::use_data(conversionTables, internal = T, overwrite = T)
#' @noRd
#' @keywords internal
create_conversion_tables <- function(){
  conversionTables <- list()
  speciesCodes <- data.table(aphia=character(), FDIR=character(), FAO=character(), assemblage=character(), norwegianCommonName=character())
  speciesCodes <- rbind(speciesCodes, data.table(aphia=as.character("126439"), FDIR=as.character("1038"), assemblage=as.character("SPF"), FAO=as.character("WHB"), norwegianCommonName=as.character("Kolmule")))
  speciesCodes <- rbind(speciesCodes, data.table(aphia=as.character("126417"), FDIR=as.character("061101"), assemblage=as.character("SPF"), FAO=as.character("HER"), norwegianCommonName=as.character("Norsk vårgytende sild")))
  speciesCodes <- rbind(speciesCodes, data.table(aphia=as.character("126735"), FDIR=as.character("075101"), assemblage=as.character("SPF"), FAO=as.character("CAP"), norwegianCommonName=as.character("Barentshavslodde")))
  speciesCodes <- rbind(speciesCodes, data.table(aphia=as.character("126436"), FDIR=as.character("1022"), assemblage=as.character("DEF"), FAO=as.character("COD"), norwegianCommonName=as.character("Torsk")))
  speciesCodes <- rbind(speciesCodes, data.table(aphia=as.character("126437"), FDIR=as.character("1027"), assemblage=as.character("DEF"), FAO=as.character("HAD"), norwegianCommonName=as.character("Hyse")))
  speciesCodes <- rbind(speciesCodes, data.table(aphia=as.character("126436"), FDIR=as.character("102202"), assemblage=as.character("DEF"), FAO=as.character("COD"), norwegianCommonName=as.character("Nordøstarktisk torsk")))
  speciesCodes <- rbind(speciesCodes, data.table(aphia=as.character("126437"), FDIR=as.character("102701"), assemblage=as.character("DEF"), FAO=as.character("HAD"), norwegianCommonName=as.character("Nordøstarktisk hyse")))
  conversionTables$speciesCodes <- speciesCodes

  landingCategoryCodes <- data.table(anvhgr=integer(), landingCategory=character(), norwegianLandingCategryName=character())
  landingCategoryCodes <- rbind(landingCategoryCodes, data.table(anvhgr=as.integer(1), landingCategory=as.character("HUC"), norwegianLandingCategryName=as.character("Konsum")))
  landingCategoryCodes <- rbind(landingCategoryCodes, data.table(anvhgr=as.integer(2), landingCategory=as.character("IND"), norwegianLandingCategryName=as.character("Mel og olje")))
  landingCategoryCodes <- rbind(landingCategoryCodes, data.table(anvhgr=as.integer(3), landingCategory=as.character("IND"), norwegianLandingCategryName=as.character("Dyrefor/fiskefor, agn og annet")))
  conversionTables$landingCategoryCodes <- landingCategoryCodes

  gearCodes <- data.table(FDIRgear=integer(), FAO1980gear=character(), norwegianGearName=character())
  gearCodes <- rbind(gearCodes, data.table(FDIRgear=as.integer(51), FAO1980gear=as.character("TB"), norwegianGearName=as.character("Bunntrål")))
  gearCodes <- rbind(gearCodes, data.table(FDIRgear=as.integer(55), FAO1980gear=as.character("TBS"), norwegianGearName=as.character("Reketrål (herunder sputniktrål)")))
  gearCodes <- rbind(gearCodes, data.table(FDIRgear=as.integer(57), FAO1980gear=as.character("TBN"), norwegianGearName=as.character("Krepsetrål")))
  gearCodes <- rbind(gearCodes, data.table(FDIRgear=as.integer(52), FAO1980gear=as.character("PTB"), norwegianGearName=as.character("Bunntrål par")))
  gearCodes <- rbind(gearCodes, data.table(FDIRgear=as.integer(53), FAO1980gear=as.character("TM"), norwegianGearName=as.character("Flytetrål")))
  gearCodes <- rbind(gearCodes, data.table(FDIRgear=as.integer(58), FAO1980gear=as.character("OTT"), norwegianGearName=as.character("Dobbeltrål")))

  gearCodes <- rbind(gearCodes, data.table(FDIRgear=as.integer(11), FAO1980gear=as.character("PS"), norwegianGearName=as.character("Snurpenot/ringnot")))
  gearCodes <- rbind(gearCodes, data.table(FDIRgear=as.integer(12), FAO1980gear=as.character("SB"), norwegianGearName=as.character("Landnot")))

  gearCodes <- rbind(gearCodes, data.table(FDIRgear=as.integer(61), FAO1980gear=as.character("SV"), norwegianGearName=as.character("Snurrevad")))

  gearCodes <- rbind(gearCodes, data.table(FDIRgear=as.integer(35), FAO1980gear=as.character("LLS"), norwegianGearName=as.character("Autoline")))
  gearCodes <- rbind(gearCodes, data.table(FDIRgear=as.integer(32), FAO1980gear=as.character("LL"), norwegianGearName=as.character("Andre liner")))
  gearCodes <- rbind(gearCodes, data.table(FDIRgear=as.integer(34), FAO1980gear=as.character("LX"), norwegianGearName=as.character("Dorg/harp/snik")))
  gearCodes <- rbind(gearCodes, data.table(FDIRgear=as.integer(30), FAO1980gear=as.character("LX"), norwegianGearName=as.character("Udefinert krokredskap")))
  gearCodes <- rbind(gearCodes, data.table(FDIRgear=as.integer(33), FAO1980gear=as.character("LHM"), norwegianGearName=as.character("Juksa/pilk")))
  gearCodes <- rbind(gearCodes, data.table(FDIRgear=as.integer(31), FAO1980gear=as.character("LLD"), norwegianGearName=as.character("Flyteline")))

  gearCodes <- rbind(gearCodes, data.table(FDIRgear=as.integer(20), FAO1980gear=as.character("GEN"), norwegianGearName=as.character("Udefinert garn")))
  gearCodes <- rbind(gearCodes, data.table(FDIRgear=as.integer(22), FAO1980gear=as.character("GNS"), norwegianGearName=as.character("Settegarn")))
  gearCodes <- rbind(gearCodes, data.table(FDIRgear=as.integer(21), FAO1980gear=as.character("GND"), norwegianGearName=as.character("Drivgarn")))

  gearCodes <- rbind(gearCodes, data.table(FDIRgear=as.integer(42), FAO1980gear=as.character("FPO"), norwegianGearName=as.character("Teiner")))
  gearCodes <- rbind(gearCodes, data.table(FDIRgear=as.integer(44), FAO1980gear=as.character("FPO"), norwegianGearName=as.character("Havteiner")))
  gearCodes <- rbind(gearCodes, data.table(FDIRgear=as.integer(41), FAO1980gear=as.character("FIX"), norwegianGearName=as.character("Ruser")))

  gearCodes <- rbind(gearCodes, data.table(FDIRgear=as.integer(80), FAO1980gear=as.character("MIS"), norwegianGearName=as.character("Annet")))

  gearCodes <- rbind(gearCodes, data.table(FDIRgear=as.integer(84), FAO1980gear=as.character("MIS"), norwegianGearName=as.character("Tangkutter")))
  gearCodes <- rbind(gearCodes, data.table(FDIRgear=as.integer(81), FAO1980gear=as.character("DRB"), norwegianGearName=as.character("Skjellskrape")))
  conversionTables$gearCodes <- gearCodes

  # add species with assemblage dependence on FAO gear, assume all others are defined by assembalge in species codes
  speciesGearAssemblage <- data.table(aphia=character(), FDIRgear=integer(), gearDepAssemblage=character())
  conversionTables$speciesGearAssemblage <- speciesGearAssemblage

  metierlvl5Codes <- data.table(assemblage=character(), FAO1980gear=character(), FishingActivityCategoryEuropeanLvl5=character())
  metierlvl5Codes <- rbind(metierlvl5Codes, data.table(assemblage=as.character("SPF"), FAO1980gear=as.character("TM"), FishingActivityCategoryEuropeanLvl5=as.character("OTM_SPF")))
  metierlvl5Codes <- rbind(metierlvl5Codes, data.table(assemblage=as.character("SPF"), FAO1980gear=as.character("PS"), FishingActivityCategoryEuropeanLvl5=as.character("PS_SPF")))
  metierlvl5Codes <- rbind(metierlvl5Codes, data.table(assemblage=as.character("DEF"), FAO1980gear=as.character("TB"), FishingActivityCategoryEuropeanLvl5=as.character("OTB_DEF")))
  metierlvl5Codes <- rbind(metierlvl5Codes, data.table(assemblage=as.character("DEF"), FAO1980gear=as.character("TBS"), FishingActivityCategoryEuropeanLvl5=as.character("OTB_DEF")))
  metierlvl5Codes <- rbind(metierlvl5Codes, data.table(assemblage=as.character("DEF"), FAO1980gear=as.character("TBN"), FishingActivityCategoryEuropeanLvl5=as.character("OTB_DEF")))
  metierlvl5Codes <- rbind(metierlvl5Codes, data.table(assemblage=as.character("DEF"), FAO1980gear=as.character("OTB"), FishingActivityCategoryEuropeanLvl5=as.character("OTB_DEF")))
  metierlvl5Codes <- rbind(metierlvl5Codes, data.table(assemblage=as.character("DEF"), FAO1980gear=as.character("PTB"), FishingActivityCategoryEuropeanLvl5=as.character("PTB_DEF")))
  metierlvl5Codes <- rbind(metierlvl5Codes, data.table(assemblage=as.character("DEF"), FAO1980gear=as.character("TM"), FishingActivityCategoryEuropeanLvl5=as.character("OTM_DEF")))
  metierlvl5Codes <- rbind(metierlvl5Codes, data.table(assemblage=as.character("DEF"), FAO1980gear=as.character("OTT"), FishingActivityCategoryEuropeanLvl5=as.character("OTT_DEF")))
  metierlvl5Codes <- rbind(metierlvl5Codes, data.table(assemblage=as.character("DEF"), FAO1980gear=as.character("PS"), FishingActivityCategoryEuropeanLvl5=as.character("PS_DEF")))
  metierlvl5Codes <- rbind(metierlvl5Codes, data.table(assemblage=as.character("DEF"), FAO1980gear=as.character("SB"), FishingActivityCategoryEuropeanLvl5=as.character("SB_DEF")))
  metierlvl5Codes <- rbind(metierlvl5Codes, data.table(assemblage=as.character("DEF"), FAO1980gear=as.character("SV"), FishingActivityCategoryEuropeanLvl5=as.character("SSC_DEF")))
  metierlvl5Codes <- rbind(metierlvl5Codes, data.table(assemblage=as.character("DEF"), FAO1980gear=as.character("SSC"), FishingActivityCategoryEuropeanLvl5=as.character("SSC_DEF")))
  metierlvl5Codes <- rbind(metierlvl5Codes, data.table(assemblage=as.character("DEF"), FAO1980gear=as.character("LLS"), FishingActivityCategoryEuropeanLvl5=as.character("LLS_DEF")))
  metierlvl5Codes <- rbind(metierlvl5Codes, data.table(assemblage=as.character("DEF"), FAO1980gear=as.character("LLD"), FishingActivityCategoryEuropeanLvl5=as.character("LLD_DEF")))
  metierlvl5Codes <- rbind(metierlvl5Codes, data.table(assemblage=as.character("DEF"), FAO1980gear=as.character("LL"), FishingActivityCategoryEuropeanLvl5=as.character("LX_DEF")))
  metierlvl5Codes <- rbind(metierlvl5Codes, data.table(assemblage=as.character("DEF"), FAO1980gear=as.character("LX"), FishingActivityCategoryEuropeanLvl5=as.character("LX_DEF")))
  metierlvl5Codes <- rbind(metierlvl5Codes, data.table(assemblage=as.character("DEF"), FAO1980gear=as.character("LHM"), FishingActivityCategoryEuropeanLvl5=as.character("LHM_DEF")))
  metierlvl5Codes <- rbind(metierlvl5Codes, data.table(assemblage=as.character("DEF"), FAO1980gear=as.character("LHP"), FishingActivityCategoryEuropeanLvl5=as.character("LHM_DEF")))
  metierlvl5Codes <- rbind(metierlvl5Codes, data.table(assemblage=as.character("DEF"), FAO1980gear=as.character("GEN"), FishingActivityCategoryEuropeanLvl5=as.character("GNS_DEF")))
  metierlvl5Codes <- rbind(metierlvl5Codes, data.table(assemblage=as.character("DEF"), FAO1980gear=as.character("GNS"), FishingActivityCategoryEuropeanLvl5=as.character("GNS_DEF")))
  metierlvl5Codes <- rbind(metierlvl5Codes, data.table(assemblage=as.character("DEF"), FAO1980gear=as.character("GND"), FishingActivityCategoryEuropeanLvl5=as.character("GND_DEF")))
  metierlvl5Codes <- rbind(metierlvl5Codes, data.table(assemblage=as.character("DEF"), FAO1980gear=as.character("FPO"), FishingActivityCategoryEuropeanLvl5=as.character("FPO_DEF")))
  metierlvl5Codes <- rbind(metierlvl5Codes, data.table(assemblage=as.character("DEF"), FAO1980gear=as.character("FIX"), FishingActivityCategoryEuropeanLvl5=as.character("FPO_DEF")))
  metierlvl5Codes <- rbind(metierlvl5Codes, data.table(assemblage=as.character("DEF"), FAO1980gear=as.character("MIS"), FishingActivityCategoryEuropeanLvl5=as.character("MIS_DEF")))
  metierlvl5Codes <- rbind(metierlvl5Codes, data.table(assemblage=as.character("DEF"), FAO1980gear=as.character("DRB"), FishingActivityCategoryEuropeanLvl5=as.character("MIS_DEF")))

  conversionTables$metierlvl5Codes <- metierlvl5Codes

  # load shapefiles
  lokshapes <- readOGR("~/shapefiles/fdir/fdir_annotated/Lokasjoner_fom_2018/", "Lok_2018")
  ll <- coordinates(lokshapes)
  lokcoordinates <- data.table(longitude=ll[,1], latitude=ll[,2])
  lokcoordinates$lokid <- as.character(lokshapes$lok)
  lokcoordinates$lokasjon <- as.character(lokshapes$Lokasjon)
  lokcoordinates$Homr <- as.character(lokshapes$HAVOMR)

  llsp <- SpatialPoints(ll, proj4string = CRS(proj4string(lokshapes)))
  ICESarea <- readOGR("~/shapefiles/ICES_StatRec_mapto_ICES_Areas", "StatRec_map_Areas_Full_20170124")
  overl <- over(llsp, ICESarea, returnList=F)
  lokcoordinates$ICESArea <- paste("27", overl$Area_27, sep=".")
  lokcoordinates$StatRect <- as.character(overl$ICESNAME)
  conversionTables$areaCodes <- lokcoordinates
  # impute 0-loc based on area
  conversionTables$areaCodes <- rbind(conversionTables$areaCodes, data.table(longitude=as.numeric(NA), latitude=as.numeric(NA), lokid=as.character(NA), lokasjon=as.character("00"), Homr=as.character("07"), ICESArea=as.character("27.2.a.2"), StatRect=as.character(NA)))
  conversionTables$areaCodes <- rbind(conversionTables$areaCodes, data.table(longitude=as.numeric(NA), latitude=as.numeric(NA), lokid=as.character(NA), lokasjon=as.character("00"), Homr=as.character("05"), ICESArea=as.character("27.2.a.2"), StatRect=as.character(NA)))
  conversionTables$areaCodes <- rbind(conversionTables$areaCodes, data.table(longitude=as.numeric(NA), latitude=as.numeric(NA), lokid=as.character(NA), lokasjon=as.character("00"), Homr=as.character("28"), ICESArea=as.character("27.4.a"), StatRect=as.character(NA)))
  conversionTables$areaCodes <- rbind(conversionTables$areaCodes, data.table(longitude=as.numeric(NA), latitude=as.numeric(NA), lokid=as.character(NA), lokasjon=as.character("00"), Homr=as.character("41"), ICESArea=as.character("27.4.b"), StatRect=as.character(NA)))
  conversionTables$areaCodes <- rbind(conversionTables$areaCodes, data.table(longitude=as.numeric(NA), latitude=as.numeric(NA), lokid=as.character(NA), lokasjon=as.character("00"), Homr=as.character("20"), ICESArea=as.character("27.2.b.2"), StatRect=as.character(NA)))
  conversionTables$areaCodes <- rbind(conversionTables$areaCodes, data.table(longitude=as.numeric(NA), latitude=as.numeric(NA), lokid=as.character(NA), lokasjon=as.character("00"), Homr=as.character("03"), ICESArea=as.character("27.1.b"), StatRect=as.character(NA)))
  conversionTables$areaCodes <- rbind(conversionTables$areaCodes, data.table(longitude=as.numeric(NA), latitude=as.numeric(NA), lokid=as.character(NA), lokasjon=as.character("00"), Homr=as.character("12"), ICESArea=as.character("27.2.b.2"), StatRect=as.character(NA)))
  conversionTables$areaCodes <- rbind(conversionTables$areaCodes, data.table(longitude=as.numeric(NA), latitude=as.numeric(NA), lokid=as.character(NA), lokasjon=as.character("00"), Homr=as.character("00"), ICESArea=as.character("27.2.a.2"), StatRect=as.character(NA)))
  conversionTables$areaCodes <- rbind(conversionTables$areaCodes, data.table(longitude=as.numeric(NA), latitude=as.numeric(NA), lokid=as.character(NA), lokasjon=as.character("00"), Homr=as.character("61"), ICESArea=as.character("27.14.b.2"), StatRect=as.character(NA)))
  conversionTables$areaCodes <- rbind(conversionTables$areaCodes, data.table(longitude=as.numeric(NA), latitude=as.numeric(NA), lokid=as.character(NA), lokasjon=as.character("00"), Homr=as.character("06"), ICESArea=as.character("27.2.a.2"), StatRect=as.character(NA)))
  conversionTables$areaCodes <- rbind(conversionTables$areaCodes, data.table(longitude=as.numeric(NA), latitude=as.numeric(NA), lokid=as.character(NA), lokasjon=as.character("00"), Homr=as.character("08"), ICESArea=as.character("27.4.a"), StatRect=as.character(NA)))

  return(conversionTables)
}

#' Prep CL example
#' Relies on resources external to the package. Used for updating example data
#' CLCodHadNOR <- prepLandings_COD_HAD_2018("~/landingsets/LSS/FDIR_HI_LSS_FANGST_2018_PR_2019-04-02.psv")
#' usethis::use_data(CLCodHadNOR, overwrite=T)
#' @param file file with LSS all landings 2018
#' @noRd
#' @keywords internal
prepLandings_COD_HAD_2018 <- function(file){
  landings <- prepRECA:::parseLSS(file)
  lset <- landings[landings$`Art - FDIR (kode)` %in% c("1022", "102201", "102202", "102203", "102204","1027", "102701", "102702", "102703", "102704"),]
  lset <- lset[lset$`Fartøynasjonalitet (kode)` == "NOR",]
  lset <- lset[lset$`Redskap (kode)`!=90,] #removed farmed fish
  lset <- lset[lset$`Hovedområde (kode)`!="81",] #remove landings in NAFO area
  clset <- convertCL(lset)
  clagg <- aggregateCL(clset)
  return(clagg)
}

#' Prep example data
#' Relies on resources external to the package. Used for updating example data
#' NORportsampling2018 <- prepData_portsampling_2018("~/code/github/rdbes_1.17.pop/portsampling_H5.csv")
#' usethis::use_data(NORportsampling2018, overwrite=T)
#' @param file file with portsampling for 2018 formatted as RDBES v 1.17
#' @noRd
#' @keywords internal
prepData_portsampling_2018 <- function(file){
  portsampling <- prepRECA:::parseRDBESexchangeH5(file)

  #
  # fix metier setting here for now (fix in rdbes export)
  #
  if (any(is.na(portsampling$LE$LEgear)) | any(is.na(portsampling$LE$LEmetier5))){
    stop("Can not annotate metier lvlv 5")
  }
  newPortsampling <- portsampling
  newPortsampling$LE$LEass <- substr(portsampling$LE$LEmetier5,5,7)
  newPortsampling$LE <- merge(newPortsampling$LE, conversionTables$metierlvl5Codes, by.x=c("LEgear", "LEass"), by.y=c("FAO1980gear", "assemblage"), all.x=T)
  newPortsampling$LE$LEmetier5 <- newPortsampling$LE$FishingActivityCategoryEuropeanLvl5
  newPortsampling$LE$LEass <- NULL
  newPortsampling$LE$FishingActivityCategoryEuropeanLvl5 <- NULL

  if (any(is.na(newPortsampling$LE$LEmetier5))){
    stop("Could not annotate metier5 for all samples")
  }

  return(newPortsampling)
}

#
# Update data
#

conversionTables <- create_conversion_tables()
usethis::use_data(conversionTables, internal = T, overwrite = T)

CLCodHadNOR <- prepLandings_COD_HAD_2018("~/landingsets/LSS/FDIR_HI_LSS_FANGST_2018_PR_2019-04-02.psv")
usethis::use_data(CLCodHadNOR, overwrite=T)

NORportsampling2018 <- prepData_portsampling_2018("~/code/github/rdbes_1.17.pop/portsampling_H5.csv")
usethis::use_data(NORportsampling2018, overwrite=T)
