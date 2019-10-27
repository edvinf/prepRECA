SA <- NORportsampling2018$SA[NORportsampling2018$SA$SAsppCode == "126437",]
BV <- NORportsampling2018$BV[NORportsampling2018$BV$SAid %in% SA$SAid,]
fishdata <- extractBV(BV, c("Age", "Length", "Weight"))
fishdata <- merge(fishdata, SA, by="SAid")
fishdata <- merge(fishdata, NORportsampling2018$SS, by="SSid")
fishdata <- merge(fishdata, NORportsampling2018$LE, by="LEid", suffixes = c("", ".LE"))
fishdata <- merge(fishdata, prepRECA::NORportsampling2018$VD, by="VDid")
fishdata <- fishdata[!is.na(fishdata$Age),]
fishdata$catchId <- fishdata$LEid
fishdata$sampleId <- fishdata$SAid
fishdata$Metier5 <- fishdata$LEmetier5
fishdata$vessel <- fishdata$VDencrCode
fishdata <- fishdata[,c("catchId", "sampleId", "Age", "Weight", "Length", "Metier5", "vessel")]

landings <- CLCodHadNOR
landings <- landings[landings$Species == "126437",]
landings$Metier5 <- landings$FishingActivityCategoryEuropeanLvl5
landings$LiveWeightKG <- landings$OfficialLandingsWeight
landings$midseason <- (as.numeric(landings$Month)*30 - 15)/365
landings <- landings[,c("Metier5", "LiveWeightKG", "midseason")]

context("test prepRECA: minimal run")
fsmin <- fishdata
fsmin$Metier5 <- NULL
fsmin$vessel <- NULL
lmin <- landings
lmin$Metier5 <- NULL
prepRECA(fsmin, lmin, NULL, NULL, NULL)
prepRECA(fishdata, landings, c("Metier5"), c("vessel"), NULL)

context("test prepRECA: missing spec")
expect_error(prepRECA(fishdata, landings, NULL, NULL, NULL))

context("test prepRECA: missing column random effect")
expect_error(prepRECA(fishdata, landings, NULL, c("gear"), NULL))

context("test prepRECA: missing column fixed effect")
expect_error(prepRECA(fishdata, landings, c("gear"), NULL, NULL))

context("test rEcaDataReport: minimal run")
rEcaDataReport(fishdata, landings)

context("test rEcaDataReport: no covariates")
expect_error(rEcaDataReport(fsmin, lmin))

context("tets getCovariateMap: simple run")
map<-getCovariateMap(c("Metier5"), fishdata, landings)
expect_equal(length(map), length(unique(c(fishdata$Metier5, landings$Metier5))))
expect_true(map[[1]] %in% landings$Metier5)
