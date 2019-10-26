#' parse DE line, add result to datalist
#' @param linevec list of elements on line
#' @param datalist list of data frames representing the RDBES data model
#' @param DEid id this line
#' @param verbose logical whether to print progress info
#' @noRd
#' @keywords internal
#' @import stats
#' @import data.table
parseDE <- function(linevec, datalist, DEid, verbose=F){

  if (verbose){
    print(paste("Reading DE", DEid))
  }

  linevec[linevec==""]<-NA
  dt <- data.table(
    DEid=as.integer(DEid),
    DErecType=as.character(linevec[[1]]),
    DEsampScheme=as.character(linevec[[2]]),
    DEyear=as.integer(linevec[[3]]),
    DEstratum=as.character(linevec[[4]]),
    DEhierarchyCor=as.character(linevec[[5]]),
    DEhierarchy=as.character(linevec[[6]])
  )
  datalist$DE <- rbind(datalist$DE, dt)
  return(datalist)
}

#' parse SD line, add result to datalist
#' @param linevec list of elements on line
#' @param datalist list of data frames representing the RDBES data model
#' @param SDid id this line
#' @param DEid id of parent DE table
#' @param verbose logical whether to print progress info
#' @noRd
#' @keywords internal
parseSD <- function(linevec, datalist, SDid, DEid, verbose=F){

  if (verbose){
    print(paste("Reading SD", SDid))
  }

  linevec[linevec==""]<-NA
  dt <- data.table(
                   SDid=as.integer(SDid),
                   DEid=as.integer(DEid),
                   SDrecType=as.character(linevec[[1]]),
                   SDctry=as.character(linevec[[2]]),
                   SDinst=as.character(linevec[[3]])
  )
  datalist$SD <- rbind(datalist$SD, dt)
  return(datalist)
}

#' parse FO line, add result to datalist
#' @param linevec list of elements on line
#' @param datalist list of data frames representing the RDBES data model
#' @param FOid id this line
#' @param FTid id of any parent FT table
#' @param SDid id of any parent SD table
#' @param verbose logical whether to print progress info
#' @noRd
#' @keywords internal
parseFO <- function(linevec, datalist, FOid, FTid=NA, SDid=NA, verbose=F){

  if (verbose){
    print(paste("Reading FO", FOid))
  }

  linevec[linevec==""]<-NA
  dt <- data.table(FOid=as.integer(FOid),
                   FTid=as.integer(FTid),
                   SDid=as.integer(SDid),
                   FOrecType=as.character(linevec[[1]]),
                   FOstratification=as.character(linevec[[2]]),
                   FOhaulNum=as.integer(linevec[[3]]),
                   FOstratum=as.character(linevec[[4]]),
                   FOclustering=as.character(linevec[[5]]),
                   FOclusterName=as.character(linevec[[6]]),
                   FOsampler=as.character(linevec[[7]]),
                   FOaggLev=as.character(linevec[[8]]),
                   FOval=as.character(linevec[[9]]),
                   FOcatReg=as.character(linevec[[10]]),
                   FOstartDate=as.POSIXct(linevec[[11]]),
                   FOstartTime=as.character(linevec[[12]]),
                   FOendDate=as.POSIXct(linevec[[13]]),
                   FOendTime=as.character(linevec[[14]]),
                   FOdur=as.integer(linevec[[15]]),
                   FOstartLat=as.numeric(linevec[[16]]),
                   FOstartLon=as.numeric(linevec[[17]]),
                   FOstopLat=as.numeric(linevec[[18]]),
                   FOstopLon=as.numeric(linevec[[19]]),
                   FOecoZone=as.character(linevec[[20]]),
                   FOarea=as.character(linevec[[21]]),
                   FOstatRect=as.character(linevec[[22]]),
                   FOsubRect=as.character(linevec[[23]]),
                   FOfu=as.character(linevec[[24]]),
                   FOdep=as.integer(linevec[[25]]),
                   FOwaterDep=as.integer(linevec[[26]]),
                   FOnatCat=as.character(linevec[[27]]),
                   FOmetier5=as.character(linevec[[28]]),
                   FOmetier6=as.character(linevec[[29]]),
                   FOgear=as.character(linevec[[30]]),
                   FOmeshSize=as.character(linevec[[31]]),
                   FOselDev=as.character(linevec[[33]]),
                   FOselDevMeshSize=as.character(linevec[[34]]),
                   FOtarget=as.character(linevec[[34]]),
                   FOobsCo=as.character(linevec[[35]]),
                   FOtotal=as.integer(linevec[[36]]),
                   FOsampled=as.integer(linevec[[37]]),
                   FOprob=as.numeric(linevec[[38]]),
                   FOselectMeth=as.character(linevec[[39]]),
                   FOselectMethCluster=as.character(linevec[[40]]),
                   FOtotalClusters=as.integer(linevec[[41]]),
                   FOsampledClusters=as.integer(linevec[[42]]),
                   FOprobCluster=as.integer(linevec[[43]]),
                   FOnoSampReason=as.character(linevec[[44]])
                   )
  datalist$FO <- rbind(datalist$FO, dt)
  return(datalist)
}

#' parse OS line, add result to datalist
#' @param linevec list of elements on line
#' @param datalist list of data frames representing the RDBES data model
#' @param OSid id for this line
#' @param SDid id of any parent SD table
#' @param verbose logical whether to print progress info
#' @noRd
#' @keywords internal
parseOS <- function(linevec, datalist, OSid, SDid=NA, verbose=F){
  if (verbose){
    print(paste("Reading OS", OSid))
  }

  linevec[linevec==""]<-NA
  dt <- data.table(OSid=as.integer(OSid),
                   SDid=as.integer(SDid),
                   OSrecType=as.character(linevec[[1]]),
                   OSnatName=as.character(linevec[[2]]),
                   OSstratification=as.character(linevec[[3]]),
                   OSloc=as.character(linevec[[4]]),
                   OSsamDate=as.POSIXct(linevec[[5]]),
                   OSsamTime=as.character(linevec[[6]]),
                   OSstratum=as.character(linevec[[7]]),
                   OSclustering=as.character(linevec[[8]]),
                   OSclusterName=as.character(linevec[[9]]),
                   OSsampler=as.character(linevec[[10]]),
                   OStimeUnit=as.POSIXct(linevec[[11]]),
                   OStimeValue=as.numeric(linevec[[12]]),
                   OStotal=as.integer(linevec[[13]]),
                   OSsampled=as.integer(linevec[[14]]),
                   OSprob=as.numeric(linevec[[15]]),
                   OSselectMeth=as.character(linevec[[16]]),
                   OSlocType=as.character(linevec[[17]]),
                   OSselectMethCluster=as.character(linevec[[18]]),
                   OStotalClusters=as.integer(linevec[[19]]),
                   OSsampledClusters=as.integer(linevec[[20]]),
                   OSprobCluster=as.numeric(linevec[[21]]),
                   OSnoSampReason=as.character(linevec[[22]])
  )
  datalist$OS <- rbind(datalist$OS, dt)
  return(datalist)
}

#' parse VD line, add result to datalist
#' @param linevec list of elements on line
#' @param datalist list of data frames representing the RDBES data model
#' @param VDid id for this line
#' @param verbose logical whether to print progress info
#' @noRd
#' @keywords internal
parseVD <- function(linevec, datalist, VDid, verbose=F){
  if (verbose){
    print(paste("Reading VD", VDid))
  }

  linevec[linevec==""]<-NA
  dt <- data.table(VDid=as.integer(VDid),
                   VDrecType=as.character(linevec[[1]]),
                   VDencrCode=as.character(linevec[[2]]),
                   VDhomePort=as.character(linevec[[3]]),
                   VDflgCtry=as.character(linevec[[4]]),
                   VDlen=as.integer(linevec[[5]]),
                   VDlenCat=as.character(linevec[[6]]),
                   VDpwr=as.integer(linevec[[7]]),
                   VDsize=as.integer(linevec[[8]]),
                   VDsizeUnit=as.character(linevec[[9]]),
                   VDtype=as.integer(linevec[[10]])
  )
  datalist$VD <- rbind(datalist$VD, dt)
  return(datalist)
}

#' parse LE line, add result to datalist
#' @param linevec list of elements on line
#' @param datalist list of data frames representing the RDBES data model
#' @param LEid id for this line
#' @param verbose logical whether to print progress info
#' @noRd
#' @keywords internal
parseLE <- function(linevec, datalist, LEid, OSid=NA, FTid=NA, VSid=NA, VDid=NA, TEid=NA, SAid=NA, verbose=F){
  if (verbose){
    print(paste("Reading LE", LEid))
  }

  linevec[linevec==""]<-NA
  dt <- data.table(LEid=as.integer(LEid),
                   OSid=as.integer(OSid),
                   FTid=as.integer(FTid),
                   VSid=as.integer(VSid),
                   VDid=as.integer(VDid),
                   TEid=as.integer(TEid),
                   SAid=as.integer(SAid),
                   LErecType=as.character(linevec[[1]]),
                   LEstratification=as.character(linevec[[2]]),
                   LEseqNum=as.integer(linevec[[3]]),
                   LEhaulNum=as.integer(linevec[[4]]),
                   LEstratum=as.character(linevec[[5]]),
                   LEclustering=as.character(linevec[[6]]),
                   LEclusterName=as.character(linevec[[7]]),
                   LEsampler=as.character(linevec[[8]]),
                   LEmixedTrip=as.character(linevec[[9]]),
                   LEcatReg=as.character(linevec[[10]]),
                   LEloc=as.character(linevec[[11]]),
                   LElocType=as.character(linevec[[12]]),
                   LEctry=as.character(linevec[[13]]),
                   LEdate=as.POSIXct(linevec[[14]]),
                   LEtime=as.character(linevec[[15]]),
                   LEecoZone=as.character(linevec[[16]]),
                   LEarea=as.character(linevec[[17]]),
                   LEstatRect=as.character(linevec[[18]]),
                   LEsubRect=as.character(linevec[[19]]),
                   LEfu=as.character(linevec[[20]]),
                   LEnatCat=as.character(linevec[[21]]),
                   LEmetier5=as.character(linevec[[22]]),
                   LEmetier6=as.character(linevec[[23]]),
                   LEgear=as.character(linevec[[24]]),
                   LEmeshSize=as.integer(linevec[[25]]),
                   LEselDev=as.character(linevec[[26]]),
                   LEselDevMeshSize=as.integer(linevec[[27]]),
                   LEtarget=as.integer(linevec[[28]]),
                   LEtotal=as.integer(linevec[[29]]),
                   LEsampled=as.integer(linevec[[30]]),
                   LEprob=as.numeric(linevec[[31]]),
                   LEselectMeth=as.character(linevec[[32]]),
                   LEselectMethCluster=as.character(linevec[[33]]),
                   LEtotalClusters=as.integer(linevec[[34]]),
                   LEsampledClusters=as.integer(linevec[[35]]),
                   LEprobCluster=as.numeric(linevec[[36]]),
                   LEnoSampReason=as.character(linevec[[37]]),
                   LEfullTripAva=as.character(linevec[[38]])
  )
  datalist$LE <- rbind(datalist$LE, dt)
  return(datalist)
}

#' parse SL line, add result to datalist
#' @param linevec list of elements on line
#' @param datalist list of data frames representing the RDBES data model
#' @param SLid id this line
#' @param FOid id of any parent FO table
#' @param LEid id of any parent LE table
#' @param FTid id of any parent FT table
#' @param OSid id of any parent OS table
#' @param TEid id of any parent TE table
#' @param verbose logical whether to print progress info
#' @noRd
#' @keywords internal
parseSL <- function(linevec, datalist, SLid, FOid=NA, LEid=NA, FTid=NA, OSid=NA, TEid=NA, verbose=F){

  if (verbose){
    print(paste("Reading SL", SLid))
  }

  linevec[linevec==""]<-NA
  dt <- data.table(SLid=as.integer(SLid),
                   FOid=as.integer(FOid),
                   LEid=as.integer(LEid),
                   FTid=as.integer(FTid),
                   OSid=as.integer(OSid),
                   TEid=as.integer(TEid),
                   SLrecType=as.character(linevec[[1]]),
                   SLlistName=as.character(linevec[[2]]),
                   Slyear=as.integer(linevec[[3]]),
                   SLsppCode=as.character(linevec[[4]]),
                   SLcommSpp=as.character(linevec[[5]]),
                   SLCatchFrac=as.character(linevec[[6]]))
  datalist$SL <- rbind(datalist$SL, dt)
  return(datalist)
}

#' parse SS line, add result to datalist
#' @param linevec list of elements on line
#' @param datalist list of data frames representing the RDBES data model
#' @param SSid id this line
#' @param SLid id for parent species selection table
#' @param LEid id for any parent LE table
#' @param FOid id for any parent FO table
#' @param verbose logical whether to print progress info
#' @noRd
#' @keywords internal
parseSS <- function(linevec, datalist, SSid, SLid, LEid=NA, FOid=NA, verbose=F){

  if (verbose){
    print(paste("Reading SS", SSid))
  }

  linevec[linevec==""]<-NA
  dt <- data.table(SSid=as.integer(SSid),
                   SLid=as.integer(SLid),
                   LEid=as.integer(LEid),
                   FOid=as.integer(FOid),
                   SSrecType=as.character(linevec[[1]]),
                   SSstratification=as.character(linevec[[2]]),
                   SSobsActTyp=as.character(linevec[[3]]),
                   SScatchCat=as.character(linevec[[4]]),
                   SSobsTyp=as.character(linevec[[5]]),
                   SSstratum=as.character(linevec[[6]]),
                   SSclustering=as.character(linevec[[7]]),
                   SSclusterName=as.character(linevec[[8]]),
                   SSsampler=as.character(linevec[[9]]),
                   SSsppListName=as.character(linevec[[10]]),
                   SStotal=as.integer(linevec[[11]]),
                   SSsampled=as.integer(linevec[[12]]),
                   SSselectMeth=as.character(linevec[[13]]),
                   SSselectMethCluster=as.character(linevec[[14]]),
                   SStotalClusters=as.integer(linevec[[15]]),
                   SSsampledClusters=as.integer(linevec[[16]]),
                   SSprobCluster=as.numeric(linevec[[17]]),
                   SSnoSampReason=as.character(linevec[[18]])
                   )
  datalist$SS <- rbind(datalist$SS, dt)
  return(datalist)
}

#' parse SA line, add result to datalist
#' @param linevec list of elements on line
#' @param datalist list of data frames representing the RDBES data model
#' @param SAid id this line
#' @param SSid id for parent SS table
#' @param SAparentid id for any parent sample (this line is a subsample)
#' @param verbose logical whether to print progress info
#' @noRd
#' @keywords internal
parseSA <- function(linevec, datalist, SAid, SSid, SAparentid=NA, verbose=F){

  if (verbose){
    print(paste("Reading SA", SAid))
  }
  linevec[linevec==""]<-NA

  dt <- data.table(SAid=as.integer(SAid),
                   SAparentid=as.integer(SAparentid),
                   SSid=as.integer(SSid),
                   SArecType=as.character(linevec[[1]]),
                   SAnatCode=as.character(linevec[[2]]),
                   SAstratification=as.character(linevec[[3]]),
                   SAstratum=as.character(linevec[[4]]),
                   SAsppCode=as.character(linevec[[5]]),
                   SAcommSpp=as.character(linevec[[6]]),
                   SApres=as.character(linevec[[7]]),
                   SAcatchCat=as.character(linevec[[8]]),
                   SAlandCat=as.character(linevec[[9]]),
                   SAcommCatScl=as.character(linevec[[10]]),
                   SAcommCat=as.character(linevec[[11]]),
                   SAsex=as.character(linevec[[12]]),
                   SAunitType=as.character(linevec[[13]]),
                   SAtotalWtLive=as.integer(linevec[[14]]),
                   SAsampWtLive=as.integer(linevec[[15]]),
                   SAtotal=as.numeric(linevec[[16]]),
                   SAsampled=as.numeric(linevec[[17]]),
                   SAprob=as.numeric(linevec[[18]]),
                   SAselectMeth=as.character(linevec[[19]]),
                   SAlowHierarchy=as.character(linevec[[20]]),
                   SAsampler=as.character(linevec[[21]]),
                   SAnoSampReasonFM=as.character(linevec[[22]]),
                   SAnoSampReasonBV=as.character(linevec[[23]]),
                   SAtotalWtMes=as.integer(linevec[[24]]),
                   SAsampWtMes=as.integer(linevec[[25]]),
                   SAconFacMesLive=as.numeric(linevec[[26]]),
                   SAspecState=as.character(linevec[[27]]))
  datalist$SA <- rbind(datalist$SA, dt)
  return(datalist)
}

#' parse BV line, add result to datalist
#' @param linevec list of elements on line
#' @param datalist list of data frames representing the RDBES data model
#' @param BVid id for this row
#' @param SAid id for any parent SA row
#' @param FMid id for any parent Fm row
#' @param verbose logical whether to print progress info
#' @noRd
#' @keywords internal
parseBV <- function(linevec, datalist, BVid, SAid=NA, FMid=NA, verbose=F){

  if (verbose){
    print(paste("Reading BV", BVid))
  }

  linevec[linevec==""]<-NA
  dt <- data.table(BVid=as.character(BVid),
                   SAid=as.integer(SAid),
                   FMid=as.integer(FMid),
                   BVrecType=as.character(linevec[[1]]),
                   BVfishID=as.character(linevec[[2]]),
                   BVstratification=as.character(linevec[[3]]),
                   BVstratum=as.character(linevec[[4]]),
                   BVtype=as.character(linevec[[5]]),
                   BVvalue=as.character(linevec[[6]]),
                   BVunitVal=as.character(linevec[[7]]),
                   BVunitRefList=as.character(linevec[[8]]),
                   BVmethod=as.character(linevec[[9]]),
                   BVMeEq=as.character(linevec[[10]]),
                   BVtotal=as.numeric(linevec[[11]]),
                   BVsampled=as.numeric(linevec[[12]]),
                   BVprob=as.numeric(linevec[[13]]),
                   BVselectMeth=as.character(linevec[[14]]),
                   BVsampler=as.character(linevec[[15]]))
  datalist$BV <- rbind(datalist$BV, dt)
  return(datalist)
}

#' Reads RDBES exhange format
#' @description
#'  Parser is only implemented for the tables needed for hiearachy 13
#' @param filename filename for csv file with RDBES exhcange format v1.17
#' @param verbose logical determining whether progress information should be printed while parsing
#' @return list of data.tables corresponding to the different tables parsed. Tables are identified with their Record Type, and columns are identified with their R Name
#' @noRd
#' @keywords internal
parseRDBESexchangeH13 <- function(filename, verbose=F){

  #populate this with one data frame for each table
  datalist <- list()

  # assigning id based on order in exhcange format
  # keeping track of last assigned id
  lastid <-list()
  lastid$DE <- 0
  lastid$SD <- 0
  lastid$FO <- 0
  lastid$SL <- 0
  lastid$SS <- 0
  lastid$SA <- 0
  lastid$BV <- 0

  f <- file(filename, open="rU")
  lines <- readLines(f)
  close(f)

  for (l in lines){

    linevec <- data.table::tstrsplit(paste(l,",",sep=""), ",") #add trailing comma as strsplit discards trailing empty values

    if (linevec[[1]]=="DE"){
      lastid$DE <- lastid$DE+1
      datalist <- parseDE(linevec, datalist, lastid$DE, verbose=verbose)
    }
    else if (linevec[[1]]=="SD"){
      lastid$SD <- lastid$SD+1
      datalist <- parseSD(linevec, datalist, lastid$SD, lastid$DE, verbose=verbose)
    }
    else if (linevec[[1]]=="FO"){
      lastid$FO <- lastid$FO+1
      datalist <- parseFO(linevec, datalist, lastid$FO, SDid = lastid$SD, verbose=verbose)
    }
    else if (linevec[[1]]=="SL"){
      datalist <- parseSL(linevec, datalist, lastid$SL, FOid = lastid$FO)
    }
    else if (linevec[[1]]=="SS"){
      lastid$SS <- lastid$SS+1
      lastid$SL <- lastid$SL+1 # SL comes after SS in exhange format
      datalist <- parseSS(linevec, datalist, lastid$SS, lastid$SL, FOid=lastid$FO)
    }
    else if (linevec[[1]]=="SA"){
      lastid$SA <- lastid$SA+1
      datalist <- parseSA(linevec, datalist, lastid$SA, lastid$SS, verbose=verbose)
    }
    else if (linevec[[1]]=="BV"){
      lastid$BV <- lastid$BV+1
      datalist <- parseBV(linevec, datalist, lastid$BV, SAid=lastid$SA)
    }
    else{
      stop(paste("Record type", linevec[[1]], "not supported"))
    }
  }

  return(datalist)
}

#' Reads RDBES exhange format
#' @description
#'  Parser is only implemented for the tables needed for hiearachy 5
#' @param filename filename for csv file with RDBES exhcange format v1.17
#' @param verbose logical determining whether progress information should be printed while parsing
#' @return list of data.tables corresponding to the different tables parsed. Tables are identified with their Record Type, and columns are identified with their R Name
#' @noRd
#' @keywords internal
parseRDBESexchangeH5 <- function(filename, verbose=F){

  #populate this with one data frame for each table
  datalist <- list()

  # assigning id based on order in exhcange format
  # keeping track of last assigned id
  lastid <-list()
  lastid$DE <- 0
  lastid$SD <- 0
  lastid$OS <- 0
  lastid$VD <- 0
  lastid$LE <- 0
  lastid$SL <- 0
  lastid$SS <- 0
  lastid$SA <- 0
  lastid$BV <- 0

  f <- file(filename, open="rU")
  lines <- readLines(f)
  close(f)

  for (l in lines){

    linevec <- data.table::tstrsplit(paste(l,",",sep=""), ",") #add trailing comma as strsplit discards trailing empty values

    if (linevec[[1]]=="DE"){
      lastid$DE <- lastid$DE+1
      datalist <- parseDE(linevec, datalist, lastid$DE, verbose=verbose)
    }
    else if (linevec[[1]]=="SD"){
      lastid$SD <- lastid$SD+1
      datalist <- parseSD(linevec, datalist, lastid$SD, lastid$DE, verbose=verbose)
    }
    else if (linevec[[1]]=="OS"){
      lastid$OS <- lastid$OS+1
      datalist <- parseOS(linevec, datalist, lastid$OS, SDid = lastid$SD, verbose=verbose)
    }
    else if (linevec[[1]]=="VD"){
      lastid$VD <- lastid$VD+1
      datalist <- parseVD(linevec, datalist, lastid$VD, verbose=verbose)
    }
    else if (linevec[[1]]=="LE"){
      lastid$LE <- lastid$LE+1
      datalist <- parseLE(linevec, datalist, lastid$LE, OSid=lastid$OS, VDid=lastid$VD, verbose=verbose)
    }
    else if (linevec[[1]]=="SL"){
      datalist <- parseSL(linevec, datalist, lastid$SL, LEid = lastid$LE)
    }
    else if (linevec[[1]]=="SS"){
      lastid$SS <- lastid$SS+1
      # SL is a bit misspecified in v 1.17. Work around by making SLid repeating
      lastid$SL <- lastid$SL+1 # SL comes after SS in exhange format
      datalist <- parseSS(linevec, datalist, lastid$SS, lastid$SL, LEid=lastid$LE)
    }
    else if (linevec[[1]]=="SA"){
      lastid$SA <- lastid$SA+1
      datalist <- parseSA(linevec, datalist, lastid$SA, lastid$SS, verbose=verbose)
    }
    else if (linevec[[1]]=="BV"){
      lastid$BV <- lastid$BV+1
      datalist <- parseBV(linevec, datalist, lastid$BV, SAid=lastid$SA)
    }
    else{
      stop(paste("Record type", linevec[[1]], "not supported"))
    }
  }

  return(datalist)
}

#' Flatten RDBES
#' @param datalist RDBES v 1.17 as parsed by functions in this file
#' @return data.table
#' @noRd
#' @keywords internal
flattenRDBES <- function(datalist){
  BV <- datalist$BV
  stop("Not implemented")
}

#' Export to csv
#' @description
#'  Exports RDBES data mode v1.17 to csv files.
#' @details
#'  One file will be produced for each table
#' @param data list of data.table one for each table in hierarchy
#' @param targetdir character() directory to write files to
#' @param overwrite logical() whether to overwrite existing files
#' @noRd
#' @keywords internal
exportCsv <- function(data, targetdir, overwrite=F){
  filenames <- paste(targetdir, unlist(lapply(names(data), FUN=function(x){paste(x, "csv", sep=".")})), sep="/")

  if (!overwrite & any(file.exists(filenames))){
    stop("Some files already exist. Consider overwrite=T")
  }

  for (i in 1:length(data)){
    utils::write.csv(data[[i]], file=filenames[i], quote = F, row.names = F)
  }
}

#' Parses landings on the LSS format
#' @noRd
#' @keywords internal
#' @param file file with LSS landings
#' @return data.table with LSS landings
#' @import data.table
#' @import readr
parseLSS <- function(file){
  loc <- readr::default_locale()
  loc$decimal_mark <- ","
  loc$encoding <- "latin1"
  db <- read_delim(file, delim="|", col_names=T, trim_ws=TRUE, na=c("", "na", "NA"), locale=loc, guess_max = 100000)
  return(db)
}



