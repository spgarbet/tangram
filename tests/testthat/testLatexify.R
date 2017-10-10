# Test Rmd Escaped specials

test_that("Escaped hash",        expect_true(latexify("\\#")  == "\\#"))
test_that("Escaped dollar",      expect_true(latexify("\\$")  == "\\textdollar{}"))
test_that("Escaped asterisk",    expect_true(latexify("\\*")  == "\\ast{}"))
test_that("Escaped backslash",   expect_true(latexify("\\\\") == "\\textbackslash{}"))
test_that("Escaped caret",       expect_true(latexify("\\^")  == "\\textasciicircum{}"))
test_that("Escaped underscore",  expect_true(latexify("\\_")  == "\\_"))
test_that("Escaped backtick",    expect_true(latexify("\\`")  == "\\textasciigrave{}"))
test_that("Escaped tilde",       expect_true(latexify("\\~")  == "\\textasciitilde{}"))

# Test a few really special LaTeX characters

# This is broken on windows. FIXME
#test_that("NBSP",                expect_true(latexify("Space\u00A0Out")  == "Space~Out"))


test_that("percent",             expect_true(latexify("100%")            == "100\\%"))
test_that("endash",              expect_true(latexify("--")              == "\\textendash{}"))
test_that("emdash",              expect_true(latexify("---")             == "\\textemdash{}"))
test_that("ellipsis",            expect_true(latexify("...")             == "\\ldots{}"))
test_that("greater",             expect_true(latexify(">")               == "\\textgreater{}"))
test_that("less",                expect_true(latexify("<")               == "\\textless{}"))
test_that("bar",                 expect_true(latexify("|")               == "\\vert{}"))

# Test Rmd font declarations
test_that("emphasis asterisk",   expect_true(latexify("This is *text*!")    == "This is \\textit{text}!"))
test_that("emphasis underscore", expect_true(latexify("This is _text_!")    == "This is \\textit{text}!"))
test_that("bold asterisk",       expect_true(latexify("This is **text**!")  == "This is \\textbf{text}!"))
test_that("bold underscore",     expect_true(latexify("This is __text__!")  == "This is \\textbf{text}!"))
test_that("strikethrough",       expect_true(latexify("This is ~~text~~!")  == "This is \\sout{text}!"))
test_that("code",                expect_true(latexify("This is `code`!")    == "This is \\texttt{code}!"))
test_that("subscript",           expect_true(latexify("See footnote~123~.") == "See footnote\\textsubscript{123}."))
test_that("superscript",         expect_true(latexify("See authors^123^.")  == "See authors\\textsuperscript{123}."))
test_that("header 1",            expect_true(latexify("# big stuff")        == "\\Huge{big stuff}"))
test_that("header 2",            expect_true(latexify("## big stuff")       == "\\huge{big stuff}"))
test_that("header 3",            expect_true(latexify("### big stuff")      == "\\LARGE{big stuff}"))
test_that("header 4",            expect_true(latexify("#### big stuff")     == "\\Large{big stuff}"))
test_that("header 5",            expect_true(latexify("##### big stuff")    == "\\large{big stuff}"))
test_that("header 6",            expect_true(latexify("###### big stuff")   == "big stuff"))

# Test Regex Specials as well
test_that("regex specials",      expect_true(latexify(".?+()-")             == ".?+()-"))

# Test that things between dollar signs are ignored
test_that("Escapes math", {
  expect_true(latexify("blah \\* $a^{23}_{\\cos(\\theta)}$ blah \\$") ==
              "blah \\ast{} \\[a^{23}_{\\cos(\\theta)}\\] blah \\textdollar{}")
})

