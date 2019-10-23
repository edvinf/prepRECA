library(data.table)
library(rgdal)
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
  speciesCodes <- rbind(speciesCodes, data.table(aphia=as.character(""), FDIR=as.character("1022"), assemblage=as.character("DEF"), FAO=as.character("COD"), norwegianCommonName=as.character("Torsk")))
  speciesCodes <- rbind(speciesCodes, data.table(aphia=as.character(""), FDIR=as.character("1027"), assemblage=as.character("DEF"), FAO=as.character("HAD"), norwegianCommonName=as.character("Hyse")))
  speciesCodes <- rbind(speciesCodes, data.table(aphia=as.character(""), FDIR=as.character("102202"), assemblage=as.character("DEF"), FAO=as.character("COD"), norwegianCommonName=as.character("Nordøstarktisk torsk")))
  speciesCodes <- rbind(speciesCodes, data.table(aphia=as.character(""), FDIR=as.character("102701"), assemblage=as.character("DEF"), FAO=as.character("HAD"), norwegianCommonName=as.character("Nordøstarktisk hyse")))
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
  landings <- parseLSS(file)
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
  portsampling <- parseRDBESexchangeH5(file)

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
