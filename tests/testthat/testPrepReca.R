SA <- NORportsampling2018$SA[NORportsampling2018$SA$SAsppCode == "126437",]
BV <- NORportsampling2018$BV[NORportsampling2018$BV$SAid %in% SA$SAid,]
fishdata <- extractBV(BV, c("Age", "Length", "Weight"))
fishdata <- merge(fishdata, SA, by="SAid")
fishdata <- merge(fishdata, NORportsampling2018$SS, by="SSid")
fishdata <- merge(fishdata, NORportsampling2018$LE, by="LEid", suffixes = c("", ".LE"))
fishdata <- fishdata[!is.na(fishdata$Age),]
fishdata$CatchSampleId <- fishdata$LEid
fishdata$Metier5 <- fishdata$LEmetier5
fishdata <- fishdata[,c("CatchSampleId", "Age", "Weight", "Length", "Metier5")]

landings <- CLCodHadNOR
landings <- landings[landings$Species == "126437",]
landings$Metier5 <- landings$FishingActivityCategoryEuropeanLvl5
landings$LiveWeightKG <- landings$OfficialLandingsWeight
landings$midseason <- (as.numeric(landings$Month)*30 - 15)/365
landings <- landings[,c("Metier5", "LiveWeightKG", "midseason")]

context("test prepRECA: minimal run")
fsmin <- fishdata
fsmin$Metier5 <- NULL
lmin <- landings
lmin$Metier5 <- NULL
prepRECA(fsmin, lmin, NULL, NULL, NULL)
prepRECA(fishdata, landings, c("Metier5"), NULL, NULL)

context("test prepRECA: missing spec")
expect_error(prepRECA(fishdata, landings, NULL, NULL, NULL))

context("test prepRECA: missing column random effect")
expect_error(prepRECA(fishdata, landings, NULL, c("gear"), NULL))

context("test prepRECA: missing column fixed effect")
expect_error(prepRECA(fishdata, landings, c("gear"), NULL, NULL))
