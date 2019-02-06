context("test-formatting_functions")

test_that("single year correctly formatted", {
  expect_match(format_date("2006"), "2006-")
})

test_that("equals not converted to less than or equal", {
  expect_match(format_text("=="), "==")
})
