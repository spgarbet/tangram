# Test Exclude

context("exclude")

exclude_test_data <- data.frame(
  id = 1:5,
  x = c(NaN, 2, 3, -Inf, NA),
  y = factor(c("A", "A", "B", NA, "B")),
  z = c("Joe", "Jim", NA, "Jack", "Jorge")
)
exclude_test_data$z <- as.character(exclude_test_data$z)

test_that("All x:NA removed",  expect_true(
  all(exclude_data(exclude_test_data, list(x=NA))[,"id"] == 1:4)
))

test_that("All x:NA,2 removed",  expect_true(
  all(exclude_data(exclude_test_data, list(x=c(NA,2)))[,"id"] == c(1, 3, 4))
))

test_that("All y:A removed",  expect_true(
  all(exclude_data(exclude_test_data, list(y="A"))[,"id"] == 3:5)
))

test_that("y:A factor dropped",  expect_true(
  length(levels(exclude_data(exclude_test_data, list(y=c("A")))[,"y"])) == 2
))

test_that("All y:A,NA removed",  expect_true(
  all(exclude_data(exclude_test_data, list(y=c("A",NA)))[,"id"] == c(3,5))
))

test_that("All y:A, z:Joe removed",  expect_true(
  all(exclude_data(exclude_test_data, list(y=c("A"),x="Joe"))[,"id"] == 3:5)
))

test_that("All NA removed",  expect_true(
  all(exclude_data(exclude_test_data, NA)[,"id"] == 1:2)
))

test_that("All NaN removed",  expect_true(
  all(exclude_data(exclude_test_data, NaN)[,"id"] == 2:5)
))

test_that("All -Inf removed",  expect_true(
  all(exclude_data(exclude_test_data, -Inf)[,"id"] == c(1,2,3,5))
))

test_that("All 2 removed",  expect_true(
  all(exclude_data(exclude_test_data, 2)[,"id"] == c(1,3,4,5))
))

test_that("All NA, NaN removed",  expect_true(
  all(exclude_data(exclude_test_data, c(NA, NaN))[,"id"] == 2)
))


