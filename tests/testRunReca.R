
#
# Too slow to run as unit tests. Build and install. Then run manually
#

SA <- prepRECA::NORportsampling2018$SA[prepRECA::NORportsampling2018$SA$SAsppCode == "126436",]
BV <- prepRECA::NORportsampling2018$BV[prepRECA::NORportsampling2018$BV$SAid %in% SA$SAid,]
fishdata <- prepRECA::extractBV(BV, c("Age", "Length", "Weight"), c("integer", "numeric", "numeric"))
fishdata <- merge(fishdata, SA, by="SAid")
fishdata <- merge(fishdata, prepRECA::NORportsampling2018$SS, by="SSid")
fishdata <- merge(fishdata, prepRECA::NORportsampling2018$LE, by="LEid", suffixes = c("", ".LE"))
fishdata <- merge(fishdata, prepRECA::NORportsampling2018$VD, by="VDid")
fishdata <- merge(fishdata, prepRECA::NORportsampling2018$OS, by="OSid")
fishdata <- fishdata[!is.na(fishdata$Age),]
fishdata$Weight <- fishdata$Weight/1000
fishdata$Length <- fishdata$Length/10
fishdata$catchId <- fishdata$LEid
fishdata$sampleId <- fishdata$SAid
fishdata$Metier5 <- fishdata$LEmetier5
fishdata$gear <- fishdata$LEmetier5
fishdata$vessel <- fishdata$VDencrCode
fishdata$quarter <- fishdata$OSstratum
fishdata$date <- fishdata$LEdate

nFish <- fishdata[,c("sampleId", "SAtotalWtLive", "Weight")]
nFish$count <- nFish$SAtotalWtLive/mean(nFish$Weight, na.rm=T)
nFish$Weight <- NULL
nFish$SAtotalWtLive <- NULL
nFish <- unique(nFish)

landings <- prepRECA::CLCodHadNOR
landings <- landings[landings$Species == "126436",]
landings$Metier5 <- landings$FishingActivityCategoryEuropeanLvl5
landings$gear <- landings$FishingActivityCategoryEuropeanLvl5
landings$LiveWeightKG <- landings$OfficialLandingsWeight
landings$quarter <- paste("Q", landings$Quarter, sep="")


# try with no covariates
RECAobj <- prepRECA::prepRECA(fishdata, landings, fixedEffects = NULL, randomEffects = NULL, NULL, minAge = 1, maxAge = 20, lengthResolution = 1, quarter = landings$Quarter, nFish = nFish)
nocov <- prepRECA::runRECA(RECAobj,100,100, thin=1)
nocov2 <- prepRECA::runRECA(RECAobj,100,100, thin=1)
nocovFixedSeed1 <- prepRECA::runRECA(RECAobj,100,100,seed = 41, thin=1)
nocovFixedSeed2 <- prepRECA::runRECA(RECAobj,100,100,seed = 41, thin=1)
stopifnot(all(nocovFixedSeed1$prediction$TotalCount == nocovFixedSeed2$prediction$TotalCount))
stopifnot(!all(nocov$prediction$TotalCount == nocovFixedSeed1$prediction$TotalCount))
#stopifnot(!all(nocov$prediction$TotalCount == nocov2$prediction$TotalCount)) #stil getting default seed ?

# try with hatchday
#RECAobj <- prepRECA::prepRECA(fishdata[1:100,], landings, NULL, c("Metier5", "vessel"), NULL, neighbours = NULL, quarter=landings$Quarter, nFish = NULL, hatchDay=90)
#prepRECA::runRECA(RECAobj,100,100)

# try with nfish
RECAobj <- prepRECA::prepRECA(fishdata, landings, NULL, c("Metier5", "vessel"), NULL, neighbours = NULL, quarter=landings$Quarter, nFish = NULL)
res <- prepRECA::runRECA(RECAobj,100,100, thin=1)

#s<-load("~/code/github/Rstox_utils/Work/testfiles/herring_2015_tempfixed_gearrandom_100samples.Rdata")
# check if partnumber matters
ageError <- diag(rep(1,20))
colnames(ageError) <- 1:20
rownames(ageError) <- 1:20
RECAobj <- prepRECA::prepRECA(fishdata[1:1000,], landings, NULL, c("gear"), NULL, neighbours = NULL, quarter=landings$Quarter, nFish = nFish, minAge = 1, maxAge = 20, maxLength=240, lengthResolution = 1, ageError = ageError)
RECAobj$AgeLength$DataMatrix$partnumber<-1
RECAobj$WeightLength$DataMatrix$partnumber<-1
stopifnot(all(!is.na(RECAobj$AgeLength$DataMatrix$partcount)))
res <- prepRECA::runRECA(RECAobj,100,100, thin=1)


RECAobj <- prepRECA::prepRECA(fishdata, landings, NULL, c("Metier5"), NULL, neighbours = NULL, quarter=landings$Quarter, nFish = nFish, minAge = 1, maxAge = 20, maxLength=240, lengthResolution = 1)
stopifnot(all(!is.na(RECAobj$AgeLength$DataMatrix$partcount)))
res <- prepRECA::runRECA(RECAobj,100,100, thin=1)

RECAobj <- prepRECA::prepRECA(fishdata, landings, NULL, c("Metier5", "vessel"), NULL, neighbours = NULL, quarter=landings$Quarter, nFish = nFish)
stopifnot(all(!is.na(RECAobj$AgeLength$DataMatrix$partcount)))
res <- prepRECA::runRECA(RECAobj,100,100, thin=1)

# try without partcounts
RECAobj <- prepRECA::prepRECA(fishdata[1:100,], landings, NULL, c("Metier5", "vessel"), NULL, neighbours = NULL, quarter=landings$Quarter, nFish = NULL)
res <- prepRECA::runRECA(RECAobj,100,500, thin=1)


#try with CAR
neighbours <- list()
neighbours[["a"]] <- c("b","c")
neighbours[["b"]] <- c("a")
neighbours[["c"]] <- c("a")
carefftest <- fishdata[1:1000,]
carefftestland <- landings
dummycareff <- unique(carefftest[,c("catchId")])
dummycareff$dummyArea <- c(rep(c("a", "b"), nrow(dummycareff)/2))
carefftest <- merge(carefftest, dummycareff, by="catchId")
carefftestland$dummyArea <- c(rep(c("a", "b", "c"), nrow(carefftestland)/3), "a")
RECAobj <- prepRECA::prepRECA(carefftest, carefftestland, NULL, c("Metier5"), "dummyArea", neighbours = neighbours, quarter=landings$Quarter)
res <- prepRECA::runRECA(RECAobj,100,100, thin=1)
