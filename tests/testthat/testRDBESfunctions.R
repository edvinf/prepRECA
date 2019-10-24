context("testExtractBV: normal run")
testBV <- NORportsampling2018$BV
extr <- extractBV(testBV, c("Age", "Length", "Weight"))
expect_equal(nrow(extr), sum(testBV$BVtype == "Length"))
context("testExtractBV: NA reflists")
testBVna <- NORportsampling2018$BV
testBVna$BVunitRefList <- NA
extr <- extractBV(testBV, c("Age", "Length", "Weight"))

context("testExtractBV: bvtypes error")
expect_error(extractBV(testBV, c("Age", "Length", "Weigth")))

context("testExtractBV: heterogenous units error")
badBV <- testBV
badBV$BVunitVal[1] <- "m"
expect_error(extractBV(badBV, c("Age", "Length", "Weight")))

context("testExtractBV: heterogenous reflist error")
badBV <- testBV
badBV$BVunitRefList[1] <- NA
expect_error(extractBV(badBV, c("Age", "Length", "Weight")))
