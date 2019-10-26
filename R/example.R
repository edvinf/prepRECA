REA_example_haddock <- function(){

  #
  # Sample data preparation. Example formatted as RDBES v 1.17
  #

  # select samples of target species, aphia code for haddock is 126437
  SA <- NORportsampling2018$SA[NORportsampling2018$SA$SAsppCode == "126437",]

  # extract biological measurements corresponding to these samples
  BV <- NORportsampling2018$BV[NORportsampling2018$BV$SAid %in% SA$SAid,]

  # merge in needed upper levels.
  # SS, SD and DE is not strictly needed,
  # but included for checking for stratification and clustering at these levels.
  fishdata <- extractBV(BV, c("Age", "Length", "Weight"))
  fishdata <- merge(fishdata, SA, by="SAid")
  fishdata <- merge(fishdata, NORportsampling2018$SS, by="SSid")
  fishdata <- merge(fishdata, NORportsampling2018$LE, by="LEid", suffixes = c("", ".LE"))
  fishdata <- merge(fishdata, NORportsampling2018$VD, by="VDid")
  fishdata <- merge(fishdata, NORportsampling2018$OS, by="OSid")
  fishdata <- merge(fishdata, NORportsampling2018$SD, by="SDid")
  fishdata <- merge(fishdata, NORportsampling2018$DE, by="DEid")

  warningsRecaApplicability(fishdata)
  # Need to handle Gutted fish. Used for mean-weight estimation in cells, will attempt with removing Gutted fish
  # LEstratum: will use gear as covariate, but will use LEmetier5 as it has corresponding entry in landings CL.
  # OSstratum: will use as covariate season

  #remove Gutted weights
  fishdata[fishdata$SApres == "Gutted", "Weight"] <- NA

  #check that age, length and covariates (vessel, gear, season, area) are complete
  warningsDataCompleteness(fishdata, c("Age", "Length", "VDencrCode", "LEmetier5", "OSstratum", "LEarea"))

  #inspect missing ages
  plotSAnas(fishdata, "Age")
  # samples with 1 missing: readability issues
  # samples with 30-40% missing: subsampled during reading to save time
  # either case: remove missing
  fishdata <- fishdata[!is.na(fishdata$Age),]


  #
  # Landings data CL
  #
  landings <- CLCodHadNOR

  # select landings of target species, aphia code for haddock is 126437
  landings <- landings[landings$Species == "126437",]

  # aggregate on var, report count of unique SAid,
  warning("Implement check on cell completeness for selected variables") #gear, season, area, need landings


  #
  # Running R-ECA
  #

  warning("Implement prepECA") #gear, season, area

  # feed result from prepECA to eca.estimate
  # feed result from prepECA to eca.predict

  warning("Implement reportECA") #need result from predict, and mapping from prepECA
}

# remove stuff

# prepECA

# run data checks

# run ECA

# plot and report

# convergence check

# for different sampling frames
