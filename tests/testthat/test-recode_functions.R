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

test_that("plans correctly recoded", {
  expect_match(recode_plans("Brief Management Statement"), "BMS")
  expect_match(recode_plans("Management Plan"), "MP")
  expect_match(recode_plans("No plan"), "NA")
  expect_match(recode_plans("Species Action Plan"), "SAP")
})

test_that("effectiveness SAPs correctly recoded", {
  expect_match(recode_effectiveness_sap("further deteriorating away from the plan’s aim/objective(s)"), "deteriorating")
  expect_match(recode_effectiveness_sap("moving towards the plan's aim/objective(s)"), "towards")
  expect_match(recode_effectiveness_sap("unchanged"), "unchanged")
})

test_that("effectiveness MPs correctly recoded", {
  expect_match(recode_effectiveness_mp("further deteriorating"), "deteriorating")
  expect_match(recode_effectiveness_mp("improving"), "improving")
  expect_match(recode_effectiveness_mp("unchanged"), "unchanged")
})

test_that("effectiveness MPs correctly recoded", {
  expect_match(recode_ranking("high importance"), "H")
  expect_match(recode_ranking("medium importance"), "M")
})

test_that("pressure and threat locations correctly recoded", {
  expect_match(recode_pressure_threat_locations("4"), "inMS")
  expect_match(recode_pressure_threat_locations("inside the Member State"), "inMS")
  expect_match(recode_pressure_threat_locations("3"), "elseEU")
  expect_match(recode_pressure_threat_locations("elsewhere in the EU"), "elseEU")
  expect_match(recode_pressure_threat_locations("2"), "outEU")
  expect_match(recode_pressure_threat_locations("outside EU"), "outEU")
  expect_match(recode_pressure_threat_locations("1"), "inOutEU")
  expect_match(recode_pressure_threat_locations("both inside and outside EU"), "inOutEU")
  expect_match(recode_pressure_threat_locations("x"), "Unk")
  expect_match(recode_pressure_threat_locations("unknown"), "Unk")

})

test_that("measures identified correctly recoded", {
  expect_match(recode_measures_identified("Measures identified, but none yet taken"), "ident")
  expect_match(recode_measures_identified("measures_status_ident"), "ident")
  expect_match(recode_measures_identified("Measures needed but cannot be identified"), "notident")
  expect_match(recode_measures_identified("measures_status_notident"), "notident")
  expect_match(recode_measures_identified("Measures identified and taken"), "taken")
  expect_match(recode_measures_identified("measures_status_taken"), "taken")
})

test_that("measures purpose correctly recoded", {
  expect_match(recode_measures_purpose("Expand the current distribution of the species"), "expand")
  expect_match(recode_measures_purpose("measures_purpose_expand"), "expand")
  expect_match(recode_measures_purpose("Increase the population size and/or improve population dynamics (improve reproduction success, reduce mortality, improve age/sex structure)"), "increase")
  expect_match(recode_measures_purpose("measures_purpose_increase"), "increase")
  expect_match(recode_measures_purpose("Maintain the current distribution, population and/or habitat for the species"), "maintain")
  expect_match(recode_measures_purpose("measures_purpose_maintain"), "maintain")
  expect_match(recode_measures_purpose("Restore the habitat of the species"), "restore")
  expect_match(recode_measures_purpose("measures_purpose_restore"), "restore")
})

test_that("measures location correctly recoded", {
  expect_match(recode_measures_location("Only inside Natura 2000"), "in")
  expect_match(recode_measures_location("measures_location_in"), "in")
  expect_match(recode_measures_location("Both inside and outside Natura 2000"), "inOut")
  expect_match(recode_measures_location("measures_location_inout"), "inOut")
  expect_match(recode_measures_location("Only outside Natura 2000"), "out")
  expect_match(recode_measures_location("measures_location_out"), "out")
})

test_that("measures response correctly recoded", {
  expect_match(recode_measures_response("Long-term results (after 2030)"), "lonTerm")
  expect_match(recode_measures_response("measures_response_long"), "lonTerm")
  expect_match(recode_measures_response("Medium-term results (within the next two reporting periods, 2019-2030)"), "medTerm")
  expect_match(recode_measures_response("measures_response_medium"), "medTerm")
  expect_match(recode_measures_response("Short-term results (within the current reporting period, 2013-2018)"), "srtTerm")
  expect_match(recode_measures_response("measures_response_short"), "srtTerm")
})

test_that("reason change correctly recoded", {
  expect_match(recode_reason_change("Genuine change"), "genuine")
  expect_match(recode_reason_change("Improved knowledge/more accurate data"), "knowledge")
  expect_match(recode_reason_change("Use of different method"), "method")
})



