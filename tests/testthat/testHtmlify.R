# Test Rmd Escaped specials

context("HTMLify")

test_that("Bold asterisk",  expect_true(
  htmlify("*\\***created\\* asdf ** **x** ")  ==
          "**<strong>created* asdf </strong> <strong>x</strong> "
))

test_that("Bold underline", expect_true(
  htmlify("_\\___created\\_ asdf __ __x__ ")  ==
          "__<strong>created_ asdf </strong> <strong>x</strong> "
))

test_that("Emphasis asterisk",  expect_true(
  htmlify(" \\**created\\* asdf * *x* ")  ==
          "&nbsp;&nbsp;&nbsp;&nbsp;*<em>created* asdf </em> <em>x</em> "
))

test_that("Emphasis underline",  expect_true(
  htmlify("\\__created\\_ asdf _ _x_ ")  ==
          "_<em>created_ asdf </em> <em>x</em> "
))

test_that("Code quoted",  expect_true(
  htmlify("`x` asdfasdf `inline` asdfasdf")  ==
          "<code>x</code> asdfasdf <code>inline</code> asdfasdf"
))

test_that("Strikethrough double tilde",  expect_true(
  htmlify("`x` asdfasdf ~~`inline`~~ asdfasdf")  ==
          "<code>x</code> asdfasdf <del><code>inline</code></del> asdfasdf"
))

test_that("Subscript tilde",  expect_true(
  htmlify("x~2~")  ==
          "x<sub>2</sub>"
))

test_that("Superscript caret",  expect_true(
  htmlify("\u03c7^2^")  ==
          "\u03c7<sup>2</sup>"
))

test_that("Superscript and Subscript",  expect_true(
  htmlify("\u03c7^2^~142~")  ==
          "\u03c7<span class=\"supsub\">2<br/>142</span>"
))

test_that("Subscript and Superscript",  expect_true(
  htmlify("\u03c7~142~^2^")  ==
          "\u03c7<span class=\"supsub\">2<br/>142</span>"
))

test_that("Fraction",  expect_true(
  htmlify("\\frac{1 234}{5 678}")  ==
          "&nbsp;<span class=\"fraction\"><span class=\"numerator\">1 234</span>/<span class=\"denominator\">5 678</span></span>"
))

test_that("Escapes Less Than", expect_true(
  htmlify("<") == "&lt;"
))

test_that("Escapes Greater Than", expect_true(
  htmlify(">") == "&gt;"
))
