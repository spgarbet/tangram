test_that("Categorical versus Numerical generates a table",
{
  test_table <- tangram(drug ~ bili, pbc, "test")

  expect_true(inherits(test_table, "tangram"))
})

test_that("Categorical versus a function of Numerical generates a table",
{
  test_table <- tangram(drug ~ log(bili), pbc, "test")

  expect_true(inherits(test_table, "tangram"))
})

test_that("Categorical versus a Categorical generates a table",
{
  test_table <- tangram(drug ~ sex, pbc, "test")

  expect_true(inherits(test_table, "tangram"))
})

test_that("Categorical versus a Categorical of coerced type generates a table",
{
  test_table <- tangram(drug ~ stage::Categorical, pbc, "test")

  expect_true(inherits(test_table, "tangram"))
})

test_that("A multiterm expression generates a table",
{
  test_table <- tangram(drug ~ bili + albumin + stage::Categorical + protime + sex + age + spiders, pbc, "test")

  expect_true(inherits(test_table, "tangram"))
})

test_that("A Numerical versus a Numerical generates a table",
{
  test_table <- tangram(age ~ albumin, pbc, "test")

  expect_true(inherits(test_table, "tangram"))
})

test_that("Intercept handling works",
{
  d1 <- iris
  d1$A <- d1$Sepal.Length > 5.1
  attr(d1$A,"label") <- "Sepal Length > 5.1"
  tbl1 <- tangram(Species + 1 ~ A + Sepal.Width,data = d1, "test")

  expect_true(inherits(tbl1, "tangram"))
})

test_that("trailing spaces in a formula work",
{
  tbl1 <- tangram("drug ~ bili ", pbc)

  expect_true(inherits(tbl1, "tangram"))
})
