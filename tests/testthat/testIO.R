
context("parseRDBESexchange h13: read")
data <- parseRDBESexchangeH13(system.file("testresources","herringlottery_trimmed_H13.csv", package="prepRECA"))
expectedtables <- c("BV", "SA", "SS", "SL", "FO", "SD", "DE")
expect_equal(length(data), length(expectedtables))
expect_true(all(names(data) %in% expectedtables))

testkeys <- function(table, key){
  expect_true(key %in% names(table))
  expect_false(any(is.na(table[[key]])))
  expect_equal(nrow(table), length(unique(table[[key]])))
}
context("parseRDBESexchange h13: keys unique")
testkeys(data$DE, "DEid")
testkeys(data$SD, "SDid")
testkeys(data$FO, "FOid")
testkeys(data$SS, "SSid")
#testkeys(data$SL, "SLid") should not necesariily be unique, due to misspecification of V 1.17
testkeys(data$SA, "SAid")
testkeys(data$BV, "BVid")

test_foreign <- function(table, foreignkeys, unusedkeys){
  expect_true(all(c(foreignkeys, unusedkeys) %in% names(table)))
  expect_true(all(is.na(table[,unusedkeys, with=F])))
  expect_false(any(is.na(table[,foreignkeys, with=F])))
}

# these are specific to hierarchy 13, lower hiearchy C without subsampling of SA
context("parseRDBESexchange h13: foreign keys correct")
test_foreign(data$SD, c("DEid"), c())
test_foreign(data$FO, c("SDid"), c("FTid"))
test_foreign(data$SS, c("FOid", "SLid"), c("LEid"))
test_foreign(data$SL, c("FOid", "SLid"), c("LEid", "FTid", "OSid", "TEid"))
test_foreign(data$SA, c("SSid"), c("SAparentid"))
test_foreign(data$BV, c("SAid"), c("FMid"))

context("parseRDBESexchange h13: data records")
# try a few, add more when bugs come up
expect_gt(nrow(data$BV[data$BV$BVtype=="Age",]), 0)
expect_gt(nrow(data$BV[data$BV$BVtype=="Length",]), 0)
ages <- data$BV[data$BV$BVtype=="Age",]
expect_true(any(!is.na(ages$BVvalue)))
expect_true(all(data$FO$FOselectMeth=="SRSWR"))

context("parseRDBESexchange h13: SS SL relation")
expect_true(all(data$SS$SSid == data$SL$SLid ))

context("parseRDBESexchange h5: read")
data <- parseRDBESexchangeH5(system.file("testresources","portsampling_trimmed_H5.csv", package="prepRECA"))
expectedtables <- c("BV", "SA", "SS", "SL", "LE", "VD", "OS", "SD", "DE")
expect_equal(length(data), length(expectedtables))
expect_true(all(names(data) %in% expectedtables))

# these are specific to hierarchy 5, lower hiearchy C without subsampling of SA
context("parseRDBESexchange h5: foreign keys correct")
test_foreign(data$SD, c("DEid"), c())
test_foreign(data$OS, c("SDid"), c())
test_foreign(data$VD, c("VDid"), c())
test_foreign(data$LE, c("OSid","VDid"), c("FTid", "VSid"))
test_foreign(data$SS, c("LEid", "SLid"), c("FOid"))
test_foreign(data$SL, c("LEid", "SLid"), c("FOid", "FTid", "OSid", "TEid"))
test_foreign(data$SA, c("SSid"), c("SAparentid"))
test_foreign(data$BV, c("SAid"), c("FMid"))

context("parseRDBESexchange h5: keys unique")
testkeys(data$DE, "DEid")
testkeys(data$SD, "SDid")
testkeys(data$OS, "OSid")
testkeys(data$VD, "VDid")
testkeys(data$LE, "LEid")
testkeys(data$SS, "SSid")
#testkeys(data$SL, "SLid") should not necesariily be unique, due to misspecification of V 1.17
testkeys(data$SA, "SAid")
testkeys(data$BV, "BVid")

context("parseRDBESexchange h5: data records")
# try a few, add more when bugs come up
expect_gt(nrow(data$BV[data$BV$BVtype=="Age",]), 0)
expect_gt(nrow(data$BV[data$BV$BVtype=="Length",]), 0)
ages <- data$BV[data$BV$BVtype=="Age",]
expect_true(any(!is.na(ages$BVvalue)))
expect_true(all(data$OS$OSselectMeth=="NPEJ"))
expect_true(all(data$LE$LEselectMeth=="NPEJ"))
expect_true(all(!is.na(data$VD$VDencrCode)))
expect_true(all(is.na(data$SA$SAlowHierarchy) | data$SA$SAlowHierarchy == "C"))
