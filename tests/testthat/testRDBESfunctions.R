context("test extractBV: normal run")
testBV <- NORportsampling2018$BV
extr <- extractBV(testBV, c("Age", "Length", "Weight"), c("integer", "numeric", "numeric"))
expect_equal(nrow(extr), sum(testBV$BVtype == "Length"))
context("test extractBV: NA reflists")
testBVna <- NORportsampling2018$BV
testBVna$BVunitRefList <- NA
extr <- extractBV(testBV, c("Age", "Length", "Weight"), c("integer", "numeric", "numeric"))

context("test extractBV: bvtypes error")
expect_error(extractBV(testBV, c("Age", "Length", "Weigth", c("integer", "numeric", "numeric"))))

context("test extractBV: heterogenous units error")
badBV <- testBV
badBV$BVunitVal[1] <- "m"
expect_error(extractBV(badBV, c("Age", "Length", "Weight", c("integer", "numeric", "numeric"))))

context("test extractBV: heterogenous reflist error")
badBV <- testBV
badBV$BVunitRefList[1] <- NA
expect_error(extractBV(badBV, c("Age", "Length", "Weight", c("integer", "numeric", "numeric"))))

context("test warningsRecaApplicability")
expect_warning(warningsRecaApplicability(NORportsampling2018$SA))
expect_warning(warningsRecaApplicability(NORportsampling2018$LE))
expect_warning(warningsRecaApplicability(NORportsampling2018$OS))
expect_silent(warningsRecaApplicability(NORportsampling2018$DE))
badSA <- NORportsampling2018$SA
badSA$SAstratum[1] <- "Z"
badSA$SAstratum[2] <- "W"
expect_warning(warningsRecaApplicability(badSA))
