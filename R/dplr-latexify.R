# dplR::latexify 1.6.6 modified for use in tangram
# Copyright (C) 2017 Andy Bunn
# Copyright (C) 2017 Shawn Garbett, modifications
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

## Usage: \Sexpr{latexify(string_produced_by_R_code)}
##
## It seems that Sweave needs doublebackslash = TRUE
## but knitr needs doublebackslash = FALSE.
#' @include render-latex-map.R
#' @importFrom stringi stri_trans_nfc
#' @importFrom stringi stri_trans_nfd
#' @importFrom utils capture.output
latexify <- function(x, doublebackslash = FALSE, dashdash = FALSE,
                     quotes = c("straight", "curved"),
                     packages = c("fontenc", "textcomp")) {
    y <- as.character(x)
    ## Kludge for converting from "byte" to the current encoding
    ## in a way which preserves the hex notation.
    encBytes <- Encoding(y) == "bytes"
    if (any(encBytes)) {
        y[encBytes] <- capture.output(cat(y[encBytes], sep = "\n"))
    }
    ## Convert strings to UTF-8 encoding, NFD (decomposed) form, for
    ## processing of accented characters. Doing this early to
    ## circumvent pecularities in gsub() (and nchar()) when working in
    ## the C locale.
    y <- stri_trans_nfd(y)
    Letters <- paste0(c(LETTERS, letters), collapse="")
    fontenc <- "fontenc" %in% packages
    textcomp <- "textcomp" %in% packages
    eurosym <- "eurosym" %in% packages
    straightQuotes <- match.arg(quotes) == "straight"
    ## Remove control characters (not spaces!)
    y <- gsub("(?![[:space:]])[[:cntrl:]]", "", y, perl=TRUE)
    ## Convert any sequence of whitespace to a single space.  This
    ## substitution must be done before control characters because
    ## newline belongs to both groups.
    y <- gsub("[[:space:]]+", " ", y)

    ## Handle LaTeX special characters in the ASCII range.
    ## Some substitutions are mandatory, others affect matters such as
    ## the rendering of the character in question (e.g. \textquote...)
    ## or line breaks (\slash).  Some substitutions are unnecessary if
    ## using a font encoding other than OT1 (\textbar, ...), but are
    ## performed every time nonetheless.  The dash (-) is given
    ## special treatment to disable the em and en dash ligatures.
    ## Source: Scott Pakin (2009) The Comprehensive LaTeX Symbol List.
    ## Accessible through "texdoc symbols".
    ## Particularly section 8.6 "ASCII and Latin 1 quick reference".
    ##
    ## The order of the elements in the list matters!
    ## First, { and } are replaced with \{ and \}, respectively.
    ## Then, \ is replaced with \textbackslash{},
    ## but not if followed by { or }.
    ## After that, the order does not matter.
    ##
    ## This starts the 'substitutions' list, which is finally
    ## processed close to the end of the function.
    substitutions <-
        list(#c("([{}])", "\\\\\\1"),
             c("\\\\(?![{}])", "\\\\textbackslash{}"),
             #c("\\^", "\\\\textasciicircum{}"),
             c("~", "\\\\textasciitilde{}"),
             c("<", "\\\\textless{}"),
             c(">", "\\\\textgreater{}"),
             c("\\|", "\\\\textbar{}"),
             #c("([#$%&_])", "\\\\\\1"),
             c("([#$%&])", "\\\\\\1"),
             if (isTRUE(dashdash)) {
                 c("-", "\\\\mbox{-}")
             },
             if (textcomp && straightQuotes) {
                 c("'", "\\\\textquotesingle{}")
             },
             if (textcomp && straightQuotes) {
                 c("`", "\\\\textasciigrave{}")
             },
             c('"', if (fontenc && straightQuotes) {
                 "\\\\textquotedbl{}"
             } else {
                 "\\\\textquotedblright{}"
             }),
             c("/", "\\\\slash{}"))
    substitutions <- substitutions[!vapply(substitutions, is.null, logical(1))]


    ## A more comprehensive list from render-latex-map.R
    substitutions <- c(substitutions, sub_table)

    ## After a command, remove empty group (when at the end of the
    ## string or some suitable character follows) or replace it with a
    ## space (when a space does not follow).
    tmp <- paste0("(\\\\[", Letters, "]+){}")
    substitutions <-
        c(substitutions,
          list(c(paste0(tmp, "(?=$|[[:digit:],.?!;:\\\\}+*/-])"), "\\1"),
               c(paste0(tmp, "(?! )"), "\\1 ")))

    ## Apply the substitutions in the list
    i <- 1
    for (subst in substitutions) {
        before <- y
        y <- gsub(subst[1], subst[2], y, perl = TRUE)
        if(before != y)
        {
          cat("applying ", i, "\n")
          cat("  before: ", before, "\n")
          cat("  after:  ", y, "\n")
          cat(subst)
          cat("\n","\n")

        }
        i <- i + 1
    }
    if (isTRUE(doublebackslash)) {
        y <- gsub("\\", "\\\\", y, fixed=TRUE)
    }
    ## Convert result to UTF-8 NFC encoding, although most non-ASCII
    ## content has probably been converted to LaTeX commands.
    stri_trans_nfc(y)
}
