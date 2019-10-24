context("test extractBV: normal run")
testBV <- NORportsampling2018$BV
extr <- extractBV(testBV, c("Age", "Length", "Weight"))
expect_equal(nrow(extr), sum(testBV$BVtype == "Length"))
context("test extractBV: NA reflists")
testBVna <- NORportsampling2018$BV
testBVna$BVunitRefList <- NA
extr <- extractBV(testBV, c("Age", "Length", "Weight"))

context("test extractBV: bvtypes error")
expect_error(extractBV(testBV, c("Age", "Length", "Weigth")))

context("test extractBV: heterogenous units error")
badBV <- testBV
badBV$BVunitVal[1] <- "m"
expect_error(extractBV(badBV, c("Age", "Length", "Weight")))

context("test extractBV: heterogenous reflist error")
badBV <- testBV
badBV$BVunitRefList[1] <- NA
expect_error(extractBV(badBV, c("Age", "Length", "Weight")))

context("test warningsRecaApplicability")
expect_warning(warningsRecaApplicability(NORportsampling2018$SA))
expect_silent(warningsRecaApplicability(NORportsampling2018$DE))
