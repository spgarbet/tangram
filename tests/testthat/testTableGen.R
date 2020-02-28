context("Compile")

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
  tbl1 <- tangram("drug ~ bili ", pbc, "test")

  expect_true(inherits(tbl1, "tangram"))
})

test_that("data.frame with names having spaces renders",
{
  df <- data.frame("Given Name"  = c("John",         "Jacob"),
                   "Sur Name"    = c("Jingleheimer", "Smith"),
                   check.names=FALSE)
  tbl1 <- tangram(df, as.character=TRUE, id="tbl1")

  expect_true(nchar(summary(tbl1)) > 0)
})


test_that("2 contingency table is correctly rendered",
{
  x <- with(warpbreaks, table(wool)) %>% tangram(id="tbl1")

  expect_true(length(x) == 2)
  expect_true(length(x[[1]]) == 2)
})

test_that("2x3 contingency table is correctly rendered",
{
  x <- with(warpbreaks, table(wool, tension)) %>% tangram(id="tbl1")

  expect_true(length(x) == 3)
  expect_true(length(x[[1]]) == 4)
})

test_that("2x2X6 contingency table is correctly rendered",
{
  x <- tangram(UCBAdmissions, id="tbl1", style="nejm")

  expect_true(length(x)      == 5)
  expect_true(length(x[[1]]) == 8)
})

test_that("handles columns with spaces",
{
  df <- data.frame(z=1:3, x=4:6)
  names(df) <- c("foo bar", "x")

  tbl1 <- tangram(x ~ `foo bar`, df)

  expect_true(inherits(tbl1, "tangram"))
})


