context("Hmisc Bundle Intercept Handling")

test_that("Column Intercept Handling Works on Iris Data",
{
  t1 <- tangram("1~Sepal.Length + Sepal.Width", data=iris, id="joe")

  expect_equal(length(t1), 4)
  for(i in 1:4) expect_equal(length(t1[[i]]), 3)
})

test_that("Row Intercept Handling Works on Iris Data",
{
  t1 <- tangram("Sepal.Length + Sepal.Width~1", data=iris, id="joe")

  expect_equal(length(t1), 3)
  for(i in 1:3) expect_equal(length(t1[[i]]), 5)
})

test_that("Group By Species (row) forms expected table",
{
  t1 <- tangram("Sepal.Length + Sepal.Width~Species", data=iris, id="joe")

  expect_equal(length(t1), 6)
  for(i in 1:6) expect_equal(length(t1[[i]]), 5)
})

test_that("Group By Species (column) forms expected table",
{
  t1 <- tangram("Species ~ Sepal.Length + Sepal.Width", data=iris, id="joe")

  expect_equal(length(t1), 4)
  for(i in 1:4) expect_equal(length(t1[[i]]), 5)
})
