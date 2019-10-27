SA <- prepRECA::NORportsampling2018$SA[prepRECA::NORportsampling2018$SA$SAsppCode == "126437",]
BV <- prepRECA::NORportsampling2018$BV[prepRECA::NORportsampling2018$BV$SAid %in% SA$SAid,]
fishdata <- extractBV(BV, c("Age", "Length", "Weight"), c("integer", "numeric", "numeric"))
fishdata <- merge(fishdata, SA, by="SAid")
fishdata <- merge(fishdata, prepRECA::NORportsampling2018$SS, by="SSid")
fishdata <- merge(fishdata, prepRECA::NORportsampling2018$LE, by="LEid", suffixes = c("", ".LE"))
fishdata <- merge(fishdata, prepRECA::NORportsampling2018$VD, by="VDid")
fishdata <- fishdata[!is.na(fishdata$Age),]
fishdata$catchId <- fishdata$LEid
fishdata$sampleId <- fishdata$SAid
fishdata$Metier5 <- fishdata$LEmetier5
fishdata$vessel <- fishdata$VDencrCode
fishdata$date <- fishdata$LEdate
fishdata <- fishdata[,c("catchId", "sampleId", "date", "Age", "Weight", "Length", "Metier5", "vessel")]

landings <- prepRECA::CLCodHadNOR
landings <- landings[landings$Species == "126437",]
landings$Metier5 <- landings$FishingActivityCategoryEuropeanLvl5
landings$LiveWeightKG <- landings$OfficialLandingsWeight

context("test prepRECA: minimal run")

prepRECA(fishdata[1:1000], landings, NULL, NULL, NULL, month=landings$Month)
prepRECA(fishdata[1:1000], landings, c("Metier5"), c("vessel"), NULL, month=landings$Month)

context("test prepRECA: missing spec")
expect_error(prepRECA(fishdata, landings, NULL, NULL, NULL, month=landings$Month))

context("test prepRECA: missing column random effect")
expect_error(prepRECA(fishdata, landings, NULL, c("gear"), NULL, month=landings$Month))

context("test prepRECA: missing column fixed effect")
expect_error(prepRECA(fishdata, landings, c("gear"), NULL, NULL, month=landings$Month))

context("test rEcaDataReport: minimal run")
rEcaDataReport(fishdata, landings)

context("test rEcaDataReport: no covariates")
expect_error(rEcaDataReport(fsmin, lmin))

context("tets getCovariateMap: simple run")
map <- prepRECA:::getCovariateMap(c("Metier5"), fishdata, landings)
expect_equal(length(map), length(unique(c(fishdata$Metier5, landings$Metier5))))
expect_true(map[[1]] %in% landings$Metier5)

context("tets getInfoMatrix: simple run")
infom <- getInfoMatrix(fishdata, landings, c("Metier5"), c("vessel"), NULL)
expect_equal(nrow(infom), 3)
expect_true(all(c("constant", "Metier5", "vessel") %in% rownames(infom)))
expect_true(all(c("random", "CAR", "nlev") %in% colnames(infom)))


context("tets getDataMatrixAgeLength: simple run")
dmAgeLength <- getDataMatrixAgeLength(fishdata[1:10,], NULL)
expect_true(all(dmAgeLength$DataMatrix$part.year > 0))
expect_true(all(dmAgeLength$DataMatrix$part.year <= 1))
expect_equal(max(dmAgeLength$DataMatrix$samplingID), length(unique(fishdata[1:10,"catchId"])))

context("tets getDataMatrixAgeLength: nFish error")
expect_error(getDataMatrixAgeLength(fishdata, NULL)) #delprøve on some sample

context("tets getDataMatrixWeightLength: simple run")
dmWeightLength <- getDataMatrixWeightLength(fishdata[1:10,], NULL)
expect_equal(max(dmWeightLength$DataMatrix$samplingID), length(unique(fishdata[1:10,"catchId"])))

context("tets getDataMatrixWeightLength: nFish error")
expect_error(getDataMatrixWeightLength(fishdata, NULL)) #delprøve on some sample

context("tets CovariateMatrix: simple run")
cv <- getCovariateMatrix(fishdata, c(), NULL)
expect_equal(nrow(cv), length(unique(fishdata$catchId)))
expect_equal(ncol(cv),1)

context("tets getCovariateMatrix: one covariate")
covariateMaps <- list()
covariateMaps[["vessel"]] <- getCovariateMap("vessel", fishdata, landings)
cv <- getCovariateMatrix(fishdata, c("vessel"), covariateMaps)
expect_equal(nrow(cv), length(unique(fishdata$catchId)))
expect_equal(ncol(cv),2)
expect_true(all(c("vessel", "constant") %in% names(cv)))


context("tets getLandings: one covariate")
covariateMaps[["Metier5"]] <- getCovariateMap("Metier5", fishdata, landings)
land <- getLandings(landings, c("Metier5"), covariateMaps, month=landings$Month)
expect_equal(nrow(land$AgeLengthCov), length(land$LiveWeightKG))
expect_equal(nrow(land$WeightLengthCov), length(land$LiveWeightKG))
expect_equal(length(unique(land$AgeLengthCov$Metier5)), length(unique(landings$Metier5)))
expect_equal(max(land$AgeLengthCov$Metier5), length(unique(landings$Metier5)))
expect_true(all(c("Metier5", "midseason") %in% names(land$AgeLengthCov)))
expect_true(all(c("Metier5", "midseason") %in% names(land$WeightLengthCov)))

warning("Add test for neighbour")

warning("Add test for partcount")

warning("Add test for Age error")
