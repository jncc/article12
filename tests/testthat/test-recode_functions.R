context("test-recode_functions")

test_that("season correctly recoded", {
  expect_match(recode_season("Breeding"), "B")
  expect_match(recode_season("Passage"), "P")
  expect_match(recode_season("Winter"), "W")
})

test_that("population units correctly recoded", {
  expect_match(recode_population_units("number of breeding females"), "bfemales")
  expect_match(recode_population_units("number of calling males"), "cmales")
  expect_match(recode_population_units("number of individuals"), "i")
  expect_match(recode_population_units("number of males"), "males")
  expect_match(recode_population_units("number of pairs"), "p")})

test_that("type of estimate correctly recoded", {
  expect_match(recode_type_of_estimate("Best estimate"), "estimate")
  expect_match(recode_type_of_estimate("95% confidence interval"), "interval")
  expect_match(recode_type_of_estimate("Multi-year mean"), "mean")
  expect_match(recode_type_of_estimate("Minimum"), "minimum")
})

test_that("methods used correctly recoded", {
  expect_match(recode_methods_used("Insufficient or no data available"), "absentData")
  expect_match(recode_methods_used("Complete survey or a statistically robust estimate"), "completeSurvey")
  expect_match(recode_methods_used("Based mainly on expert opinion with very limited data"), "estimateExpert")
  expect_match(recode_methods_used("Based mainly on extrapolation from a limited amount of data"), "estimatePartial")
})

test_that("yes and no are reversed", {
  expect_match(reverse_yes_no("Yes"), "No")
  expect_match(reverse_yes_no("No"), "Yes")
})

test_that("trends correctly recoded", {
  expect_match(recode_trends("Decreasing (-)"), "D")
  expect_match(recode_trends("Fluctuating (F)"), "F")
  expect_match(recode_trends("Increasing (+)"), "I")
  expect_match(recode_trends("Stable (0)"), "S")
  expect_match(recode_trends("Uncertain (U)"), "U")
  expect_match(recode_trends("Unknown (X)"), "UNK")
})