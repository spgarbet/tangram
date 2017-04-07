test_that("Addition and multiplication is parseable.",
{
  ast <- Parser$new()$run("col1 + col2 + col3 ~ drug*age+spiders")

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

  expect_true(inherits(ast, "ASTTableFormula"))

  expect_true(inherits(ast$left, "ASTPlus"))

  expect_true(inherits(ast$left$left, "ASTVariable"))
  expect_equal(ast$left$left$value,  "col1")

  expect_true(inherits(ast$left$right, "ASTPlus"))

  expect_true(inherits(ast$left$right$left, "ASTVariable"))
  expect_equal(ast$left$right$left$value,  "col2")

  expect_true(inherits(ast$left$right$right, "ASTVariable"))
  expect_equal(ast$left$right$right$value,  "col3")

  expect_true(inherits(ast$right, "ASTPlus"))

  expect_true(inherits(ast$right$left, "ASTMultiply"))

  expect_true(inherits(ast$right$left$left, "ASTVariable"))
  expect_equal(ast$right$left$left$value,  "drug")

  expect_true(inherits(ast$right$left$right, "ASTVariable"))
  expect_equal(ast$right$left$right$value,  "age")

  expect_true(inherits(ast$right$right, "ASTVariable"))
  expect_equal(ast$right$right$value,  "spiders")
})

test_that("functions with nested parenthesis and r-expression characters are parsed.",
{
  ast <- Parser$new()$run("x+func_.123(10^23/([[rough]]~2)+3) ~ y")

  expect_true(inherits(ast, "ASTTableFormula"))

  expect_true(inherits(ast$left, "ASTPlus"))

  expect_true(inherits(ast$left$left, "ASTVariable"))
  expect_equal(ast$left$left$value, "x")

  expect_true(inherits(ast$left$right, "ASTFunction"))
  expect_equal(ast$left$right$value, "func_.123")
  expect_equal(ast$left$right$r_expr, "10^23/([[rough]]~2)+3")
})

test_that("type specifier information is extracted correctly.",
{
  ast <- Parser$new()$run("a+b[23]+c::Binomial ~ d[\"%03.2g\"] * f::Categorical[4]")

  expect_true(inherits(ast, "ASTTableFormula"))

  expect_true(inherits(ast$left, "ASTPlus"))

  expect_true(inherits(ast$left$left, "ASTVariable"))
  expect_equal(ast$left$left$value, "a")
  expect_identical(ast$left$left$format, NA)
  expect_identical(ast$left$left$type, NA)

  expect_true(inherits(ast$left$right, "ASTPlus"))

  expect_true(inherits(ast$left$right$left, "ASTVariable"))
  expect_equal(ast$left$right$left$value,  "b")
  expect_equal(ast$left$right$left$format, "23")
  expect_identical(ast$left$right$left$type, NA)

  expect_true(inherits(ast$left$right$right, "ASTVariable"))
  expect_equal(ast$left$right$right$value,  "c")
  expect_identical(ast$left$right$right$format, NA)
  expect_equal(ast$left$right$right$type, "Binomial")

  expect_true(inherits(ast$right, "ASTMultiply"))

  expect_true(inherits(ast$right$left, "ASTVariable"))
  expect_equal(ast$right$left$value,  "d")
  expect_equal(ast$right$left$format, "%03.2g")
  expect_identical(ast$right$left$type, NA)

  expect_true(inherits(ast$right$right, "ASTVariable"))
  expect_equal(ast$right$right$value,  "f")
  expect_equal(ast$right$right$format, "4")
  expect_equal(ast$right$right$type, "Categorical")

})

test_that("multiplication distributes correctly",
{
  expect_equal(
    Parser$new()$run("(a+b)*(c+d)~e*f*(g+h)*i")$distribute()$string(),
    "a*c+a*d+b*c+b*d ~ e*f*g*i+e*f*h*i"
  )
})

test_that("reduction via data works",
{
  df <- data.frame(x=rnorm(20), y=1:20)
  reducto <- Parser$new()$run("y ~ x")$reduce(df)

  expect_equal(reducto$left$data,  df[,"y"])
  expect_equal(reducto$right$data, df[,"x"])
})

