test_that("Addition and multiplication is parseable.",
{
  pr  <- Parser$new()
  ast <- pr$run("col1 + col2 + col3 ~ drug*age+spiders")

  ###############################################################
  ##
  ##  Abstract Syntax Tree (AST) for
  ##   col1 + col2 + col3 ~ drug*age+spiders
  ##
  ##                    table
  ##                     / \
  ##                 ___/   \___
  ##                /           \
  ##               /             \
  ##              +               +
  ##             / \             / \
  ##            /   \           /   \
  ##          col1   +         *   spiders
  ##                / \       / \
  ##               /   \     /   \
  ##             col2 col3 drug  age
  ##
  ##################################################################

  expect_equal(ast$symbol, "table")

  expect_equal(ast$left$symbol, "plus")

  expect_equal(ast$left$left$symbol, "variable")
  expect_equal(ast$left$left$value,  "col1")

  expect_equal(ast$left$right$symbol, "plus")

  expect_equal(ast$left$right$left$symbol, "variable")
  expect_equal(ast$left$right$left$value,  "col2")

  expect_equal(ast$left$right$right$symbol, "variable")
  expect_equal(ast$left$right$right$value,  "col3")

  expect_equal(ast$right$symbol, "plus")

  expect_equal(ast$right$left$symbol, "multiply")

  expect_equal(ast$right$left$left$symbol, "variable")
  expect_equal(ast$right$left$left$value,  "drug")

  expect_equal(ast$right$left$right$symbol, "variable")
  expect_equal(ast$right$left$right$value,  "age")

  expect_equal(ast$right$right$symbol, "variable")
  expect_equal(ast$right$right$value,  "spiders")
})

test_that("functions with nested parenthesis and r-expression characters are parsed.",
{
  pr  <- Parser$new()
  ast <- pr$run("x+func(10^23/([[rough]]~2)+3) ~ y")

  expect_equal(ast$symbol, "table")
  expect_equal(ast$left$symbol, "plus")
  expect_equal(ast$left$left$symbol, "variable")
  expect_equal(ast$left$left$value, "x")
  expect_equal(ast$left$right$symbol, "function")
  expect_equal(ast$left$right$value, "func")
  expect_equal(ast$left$right$left$symbol, "r_expr")
  expect_equal(ast$left$right$left$value, "10^23/([[rough]]~2)+3")
})

test_that("type specifier information is extracted correctly.",
{
  pr  <- Parser$new()
  ast <- pr$run("a+b[23]+c::Binomial ~ d[\"%03.2g\"] * f[4]::Categorical")

  expect_equal(ast$symbol, "table")

  expect_equal(ast$left$symbol, "plus")

  expect_equal(ast$left$left$symbol, "variable")
  expect_equal(ast$left$left$value, "a")
  expect_identical(ast$left$left$format, NA)
  expect_identical(ast$left$left$type, NA)

  expect_equal(ast$left$right$symbol, "plus")

  expect_equal(ast$left$right$left$symbol, "variable")
  expect_equal(ast$left$right$left$value,  "b")
  expect_equal(ast$left$right$left$format, "23")
  expect_identical(ast$left$right$left$type, NA)

  expect_equal(ast$left$right$right$symbol, "variable")
  expect_equal(ast$left$right$right$value,  "c")
  expect_identical(ast$left$right$right$format, NA)
  expect_equal(ast$left$right$right$type, "Binomial")

  expect_equal(ast$right$symbol, "multiply")

  expect_equal(ast$right$left$symbol, "variable")
  expect_equal(ast$right$left$value,  "d")
  expect_equal(ast$right$left$format, "\"%03.2g\"")
  expect_identical(ast$right$left$type, NA)

  expect_equal(ast$right$right$symbol, "variable")
  expect_equal(ast$right$right$value,  "f")
  expect_equal(ast$right$right$format, "4")
  expect_equal(ast$right$right$type, "Categorical")

})

