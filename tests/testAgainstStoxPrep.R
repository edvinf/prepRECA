library(prepRECA)
stoxRobj <- readRDS(system.file(package = "prepRECA", "testresources", "stoxprepreca"))

samples <- data.table(catchId = as.character(stoxRobj$StoxExport$biotic$serialnumber),
                      sampleId = paste(stoxRobj$StoxExport$biotic$serialnumber, samples$catchpartnumber, sep="/"),
                      date = as.POSIXct(stoxRobj$StoxExport$biotic$stationstartdate),
                      Age = stoxRobj$StoxExport$biotic$age,
                      Length = stoxRobj$StoxExport$biotic$lengthcentimeter,
                      Weight = (stoxRobj$StoxExport$biotic$individualweightgram / 1000),
                      temporal = stoxRobj$StoxExport$biotic$temporal,
                      platformfactor = stoxRobj$StoxExport$biotic$platformfactor)

landings <- stoxRobj$StoxExport$landing
landings$LiveWeightKG <- landings$rundvekt
landings$Month <- substr(landings$sistefangstdato, 6,7)
landings$quarter <- NA
landings[landings$Month %in% c("01", "02", "03"), "quarter"] <- 1
landings[landings$Month %in% c("04", "05", "06"), "quarter"] <- 2
landings[landings$Month %in% c("07", "08", "09"), "quarter"] <- 3
landings[landings$Month %in% c("10", "11", "12"), "quarter"] <- 4

prepRecaRobj <- prepRECA::prepRECA(samples, landings, c("temporal"), c("platformfactor"), minAge = 1, maxAge = 20, quarter = landings$quarter)
prepRecaResults <- prepRECA::runRECA(prepRecaRobj, 400, 400, seed=42)

stoxRobj$GlobalParameters$age.error = F
stoxRobj$GlobalParameters$CC = F
stoxRobj$GlobalParameters$CCerror = F
stoxRecaResults <- prepRECA::runRECA(stoxRobj, 400, 400, seed=42)

prepRECA::plotCatchAtAge(prepRecaResults$prediction, title="RecaPrep results")
prepRECA::plotCatchAtAge(stoxRecaResults$prediction, title="StoxPrep results")
