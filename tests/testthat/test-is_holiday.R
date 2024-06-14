



test_that("is_holiday correctly identifies New Year's Day", {
  dates <- as.Date(c("2024-01-01", "2024-07-05", "2024-12-20"))
  expected <- c(TRUE, FALSE, FALSE)
  result <- is_holiday(dates)
  expect_equal(result, expected)
})

test_that("is_holiday correctly identifies Martin Luther King Jr. Day", {
  dates <- as.Date(c("2024-01-15", "2024-01-16")) # MLK Jr. Day is 2024-01-15
  expected <- c(TRUE, FALSE)
  result <- is_holiday(dates)
  expect_equal(result, expected)
})

test_that("is_holiday correctly identifies Presidents Day", {
  dates <- as.Date(c("2024-02-19", "2024-02-20")) # Presidents Day is 2024-02-19
  expected <- c(TRUE, FALSE)
  result <- is_holiday(dates)
  expect_equal(result, expected)
})

test_that("is_holiday correctly identifies Memorial Day", {
  dates <- as.Date(c("2024-05-27", "2024-05-28")) # Memorial Day is 2024-05-27
  expected <- c(TRUE, FALSE)
  result <- is_holiday(dates)
  expect_equal(result, expected)
})

test_that("is_holiday correctly identifies Juneteenth", {
  dates <- as.Date(c("2024-06-19", "2024-06-20"))
  expected <- c(TRUE, FALSE)
  result <- is_holiday(dates)
  expect_equal(result, expected)
})

test_that("is_holiday correctly identifies Independence Day", {
  dates <- as.Date(c("2024-07-04", "2024-07-05"))
  expected <- c(TRUE, FALSE)
  result <- is_holiday(dates)
  expect_equal(result, expected)
})

test_that("is_holiday correctly identifies Labor Day", {
  dates <- as.Date(c("2024-09-02", "2024-09-03")) # Labor Day is 2024-09-02
  expected <- c(TRUE, FALSE)
  result <- is_holiday(dates)
  expect_equal(result, expected)
})

test_that("is_holiday correctly identifies Thanksgiving Day", {
  dates <- as.Date(c("2024-11-28", "2024-11-23")) # Thanksgiving Day is 2024-11-28
  expected <- c(TRUE, FALSE)
  result <- is_holiday(dates)
  expect_equal(result, expected)
})

test_that("is_holiday correctly identifies Christmas Day", {
  dates <- as.Date(c("2024-12-25", "2024-12-26"))
  expected <- c(TRUE, FALSE)
  result <- is_holiday(dates)
  expect_equal(result, expected)
})

test_that("is_holiday correctly identifies non-holidays", {
  dates <- as.Date(c("2024-02-15", "2024-03-01", "2024-10-31"))
  expected <- c(FALSE, FALSE, FALSE)
  result <- is_holiday(dates)
  expect_equal(result, expected)
})
