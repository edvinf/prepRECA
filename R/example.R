REA_example_haddock <- function(){

  #
  # Data preparation. Example formatted as RDBES v 1.17
  #

  # select samples of target species, aphia code for haddock is 126437
  SA <- NORportsampling2018$SA[NORportsampling2018$SA$SAsppCode == "126437",]

  # extract biological measurements corresponding to these samples
  BV <- NORportsampling2018$BV[NORportsampling2018$BV$SAid %in% SA$SAid,]

  # merge in needed upper levels.
  # SS, OS, SD and DE is not strictly needed,
  # but included for checking for stratification and clustering at these levels.
  fishdata <- extractBV(BV, c("Age", "Length", "Weight"))
  fishdata <- merge(fishdata, SA, by="SAid")
  fishdata <- merge(fishdata, NORportsampling2018$SS, by="SSid")
  fishdata <- merge(fishdata, NORportsampling2018$LE, by="LEid")
  fishdata <- merge(fishdata, NORportsampling2018$VD, by="VDid")
  fishdata <- merge(fishdata, NORportsampling2018$OS, by="OSid")
  fishdata <- merge(fishdata, NORportsampling2018$SD, by="SDid")
  fishdata <- merge(fishdata, NORportsampling2018$DE, by="DEid")

  warningsRecaApplicability(fishdata)

  #handle Gutted weights

  warning("Implement check on completeness for selected variables") #gear, season, area, vessel
  warning("Implement check on cell completeness for selected variables") #gear, season, area


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
