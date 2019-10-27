REA_example_haddock <- function(){

  #
  # Sample data preparation. Example formatted as RDBES v 1.17
  #

  # select samples of target species, aphia code for haddock is 126437
  SA <- prepRECA::NORportsampling2018$SA[prepRECA::NORportsampling2018$SA$SAsppCode == "126437",]

  # extract biological measurements corresponding to these samples
  BV <- prepRECA::NORportsampling2018$BV[prepRECA::NORportsampling2018$BV$SAid %in% SA$SAid,]

  # merge in needed upper levels.
  # SS, SD and DE is not strictly needed,
  # but included for checking for stratification and clustering at these levels.
  fishdata <- extractBV(BV, c("Age", "Length", "Weight"), c("integer", "numeric", "numeric"))
  fishdata <- merge(fishdata, SA, by="SAid")
  fishdata <- merge(fishdata, prepRECA::NORportsampling2018$SS, by="SSid")
  fishdata <- merge(fishdata, prepRECA::NORportsampling2018$LE, by="LEid", suffixes = c("", ".LE"))
  fishdata <- merge(fishdata, prepRECA::NORportsampling2018$VD, by="VDid")
  fishdata <- merge(fishdata, prepRECA::NORportsampling2018$OS, by="OSid")
  fishdata <- merge(fishdata, prepRECA::NORportsampling2018$SD, by="SDid")
  fishdata <- merge(fishdata, prepRECA::NORportsampling2018$DE, by="DEid")

  warningsRecaApplicability(fishdata)
  # Need to handle Gutted fish. Used for mean-weight estimation in cells, will attempt with removing Gutted fish
  # LEstratum: will use gear as covariate, but will use LEmetier5 as it has corresponding entry in landings CL.
  # OSstratum: will use as covariate season

  #remove Gutted weights
  fishdata[fishdata$SApres == "Gutted", "Weight"] <- NA

  #check that age, length and covariates (vessel, gear, season, area) are complete
  warningsDataCompleteness(fishdata, c("Age", "Length", "VDencrCode", "LEmetier5", "OSstratum", "LEarea"))

  #inspect missing measurments
  plotSAnas(fishdata, "Age")
  plotSAnas(fishdata, "Weight")


  #
  # Landings data CL
  #
  landings <- prepRECA::CLCodHadNOR

  # select landings of target species, aphia code for haddock is 126437
  landings <- landings[landings$Species == "126437",]
  month <- landings$Month
  landings <- landings[,c("Metier5", "Area", "Quarter")]
  warning("Check definition of partcount wrp replicate sampling")
  warning("Check effect of nlev not correctly set for random effects not in landings")

  #
  # Running R-ECA
  #

  ecadata <- prepRECA(fishdata, landings, c(), c(), NULL, month = month)
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
