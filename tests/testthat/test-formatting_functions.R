context("test-formatting_functions")

test_that("single year correctly formatted", {
  expect_match(format_date("2006"), "2006-")
})

test_that("date period correctly formatted", {
  expect_match(format_date_period(2013, 2013), "2013-")
  expect_match(format_date_period(2013, 2018), "2013-2018")
})

test_that("equals not converted to less than or equal", {
  expect_match(format_text("=="), "==")
})
