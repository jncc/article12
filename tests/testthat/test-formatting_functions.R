context("test-formatting_functions")

test_that("single year correctly formatted", {
  expect_match(format_date("2006"), "2006-")
})
