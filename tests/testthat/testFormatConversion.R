#
# Converting LSS
#
data <- parseLSS(system.file("testresources","landings_trimmed_2018.lss", package="prepRECA"))

context("convertCL")
conv <- convertCL(data)
expect_equal(nrow(conv), nrow(data))
expect_equal(sum(conv$OfficialLandingsWeight), sum(data$Rundvekt))
expect_true(all(!is.na(conv$OfficialLandingsWeight)))
expect_true(all(!is.na(conv$Quarter)))
expect_true(all(!is.na(conv$Month)))

context("aggregateCL")
agg <- aggregateCL(conv)
expect_lt(nrow(agg), nrow(conv))
expect_equal(sum(conv$OfficialLandingsWeight), sum(agg$OfficialLandingsWeight))
mock_conv <- conv
mock_conv$Quarter[2]<-NA
expect_error(aggregateCL(mock_conv))
