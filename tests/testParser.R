library(RUnit)

source("tableGrammar/parser.R")

test.parserExpr <- function() 
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
  
  checkEquals(ast$symbol, "table")
  
  checkEquals(ast$left$symbol, "plus")
  
  checkEquals(ast$left$left$symbol, "name")
  checkEquals(ast$left$left$value,  "col1")
  
  checkEquals(ast$left$right$symbol, "plus")
  
  checkEquals(ast$left$right$left$symbol, "name")
  checkEquals(ast$left$right$left$value,  "col2")
  
  checkEquals(ast$left$right$right$symbol, "name")
  checkEquals(ast$left$right$right$value,  "col3")
  
  checkEquals(ast$right$symbol, "plus")
  
  checkEquals(ast$right$left$symbol, "permute")
  
  checkEquals(ast$right$left$left$symbol, "name")
  checkEquals(ast$right$left$left$value,  "drug")
  
  checkEquals(ast$right$left$right$symbol, "name")
  checkEquals(ast$right$left$right$value,  "age")
  
  checkEquals(ast$right$right$symbol, "name")
  checkEquals(ast$right$right$value,  "spiders")
}

test.parserRExpr <- function() 
{
  pr  <- Parser$new()
  ast <- pr$run("x+I(10^23/([[rough]]~2)+3) ~ y")
  
  checkEquals(ast$symbol, "table")
  checkEquals(ast$left$symbol, "plus")
  checkEquals(ast$left$left$symbol, "name")
  checkEquals(ast$left$left$value, "x")
  checkEquals(ast$left$right$symbol, "r_expr")
  checkEquals(ast$left$right$value, "10^23/([[rough]]~2)+3")
}

test.parserEmbeddedTable <- function()
{
  pr  <- Parser$new()
  ast <- pr$run("y ~ (i ~ (j~k) ) + x")
  
  checkEquals(ast$symbol, "table")
  checkEquals(ast$right$symbol, "plus")
  checkEquals(ast$right$left$symbol, "table")
  checkEquals(ast$right$left$left$symbol, "name")
  checkEquals(ast$right$left$left$value,  "i")
  checkEquals(ast$right$left$right$symbol, "table")
  checkEquals(ast$right$left$right$left$symbol, "name")
  checkEquals(ast$right$left$right$left$value, "j")
  checkEquals(ast$right$left$right$right$symbol, "name")
  checkEquals(ast$right$left$right$right$value, "k")
}