context("test_recode_functions")

test_that("season correctly recoded", {
  expect_match(recode_season("Breeding"), "B")
  expect_match(recode_season("Passage"), "P")
  expect_match(recode_season("Winter"), "W")
})
