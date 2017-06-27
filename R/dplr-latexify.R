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
latexify <- function(x, doublebackslash = TRUE, dashdash = TRUE,
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

    ## Treatment of non-ASCII characters follows.

    ## Digraphs and ligatures broken into their parts, except the
    ## Dutch digraphs IJ and ij which have their own commands.
    substitutions <-
        c(substitutions,
          list(c("\u0132", "\\\\IJ{}"),
               c("\u0133", "\\\\ij{}"),
               c("\u01f1", "DZ"),
               c("\u01f2", "Dz"),
               c("\u01f3", "dz"),
               c("\u01c4", "DZ\u030c"),
               c("\u01c5", "Dz\u030c"),
               c("\u01c6", "dz\u030c"),
               c("\u01c7", "LJ"),
               c("\u01c8", "Lj"),
               c("\u01c9", "lj"),
               c("\u01ca", "NJ"),
               c("\u01cb", "Nj"),
               c("\u01cc", "nj"),
               c("\ufb00", "ff"),
               c("\ufb01", "fi"),
               c("\ufb02", "fl"),
               c("\ufb03", "ffi"),
               c("\ufb04", "ffl"),
               c("\ufb05", "\u017ft"),
               c("\ufb06", "st")))

    ## Accents (diacritics) above the letter (drop i and j dots)
    above <- list(diaeresis   = c("\u0308", "\""),
                  acute       = c("\u0301", "'"),
                  dotabove    = c("\u0307", "."),
                  macron      = c("\u0304", "="),
                  circumflex  = c("\u0302", "^"),
                  grave       = c("\u0300", "`"),
                  tilde       = c("\u0303", "~"),
                  doubleacute = c("\u030b", "H"),
                  ringabove   = c("\u030a", "r"),
                  breve       = c("\u0306", "u"),
                  caron       = c("\u030c", "v"),
                  invbreve    = c("\u0311", "newtie")) # textcomp
    ## Accents that co-exist with i and j dots (mainly below the
    ## letter, but also the ligature tie)
    below <- list(macronbelow = c("\u0331", "b"),
                  cedilla     = c("\u0327", "c"),
                  dotbelow    = c("\u0323", "d"),
                  tie         = c("\u0361", "t"),
                  ogonek      = c("\u0328", "k")) # not in OT1 fontenc
    accents <- c(above, below)
    command <- paste0("\\\\[", Letters, "]+|\\\\.")
    combining <- paste0(vapply(accents, "[", character(1), 1),
                        collapse="")
    accPre <- paste0("(", command, "|.)({})?(?<![", combining, "])")
    accPost <- paste0("(?![", combining, "])")

    ## Accent above the letter
    aboveInCode <- vapply(above, "[", character(1), 1)
    ijPattern <- paste0("([ij])", aboveInCode, accPost)
    otherPattern <- paste0(accPre, aboveInCode, accPost)
    aboveOutCode <- vapply(above, "[", character(1), 2)
    ijReplacement <- paste0("\\\\", aboveOutCode, "{\\\\\\1}")
    otherReplacement <- paste0("\\\\", aboveOutCode, "{\\1}")
    ## for (accent in above) {
    ##     code <- accent[1]
    ##     replacement <- accent[2]
    ##     y <- gsub(paste0("([ij])", code, accPost),
    ##               paste0("\\\\", replacement, "{\\\\\\1}"), y, perl = TRUE)
    ##     y <- gsub(paste0(accPre, code, accPost),
    ##               paste0("\\\\", replacement, "{\\1}"), y, perl = TRUE)
    ## }

    ## Accent below the letter, and ligature tie
    belowInCode <- vapply(below, "[", character(1), 1)
    belowPattern <- paste0(accPre, belowInCode, accPost)
    belowOutCode <- vapply(below, "[", character(1), 2)
    belowReplacement <- paste0("\\\\", belowOutCode, "{\\1}")
    ## for (accent in below) {
    ##     code <- accent[1]
    ##     replacement <- accent[2]
    ##     y <- gsub(paste0(accPre, code, accPost),
    ##               paste0("\\\\", replacement, "{\\1}"), y, perl = TRUE)
    ## }

    ## Combining an enclosing circle with an accent seems to work
    circPre <- paste0("(", command, "({([^}]|\\\\})+})?|.)({})?")
    circPattern <- paste0(circPre, "\u20dd", accPost)
    circReplacement <- "\\\\textcircled{\\1}"

    substitutions <-
        c(substitutions,
          lapply(lapply(mapply(list, list(as.name("c")),
                               c(ijPattern, otherPattern,
                                 belowPattern, circPattern),
                               c(ijReplacement, otherReplacement,
                                 belowReplacement, circReplacement),
                               SIMPLIFY=FALSE), as.call), eval))

    ## The output of sQuote() and dQuote() may contain non-ASCII
    ## quoting characters.  If the input is ASCII, it may be a
    ## surprise to the user that an UTF-8 input encoding is then
    ## needed in LaTeX.  Converting the quotes to commands solves
    ## this problem.  The substitution list also contains
    ## non-ASCII letters and other unicode characters.
    substitutions <-
        c(substitutions,
          list(c("\u00a1", "\\\\textexclamdown{}"),
               c("\u00a3", "\\\\pounds{}"),
               c("\u00a7", "\\\\S{}"),
               c("\u00a9", "\\\\copyright{}"),
               c("\u00aa", "\\\\textordfeminine{}"),
               c("\u00ae", "\\\\textregistered{}"),
               c("\u00b6", "\\\\P{}"),
               c("\u00b7", "\\\\textperiodcentered{}"),
               c("\u00ba", "\\\\textordmasculine{}"),
               c("\u00bf", "\\\\textquestiondown{}"),
               c("\u2013", "\\\\textendash{}"),
               c("\u2014", "\\\\textemdash{}"),
               c("\u2018", "\\\\textquoteleft{}"),
               c("\u2019", "\\\\textquoteright{}"),
               c("\u201c", "\\\\textquotedblleft{}"),
               c("\u201d", "\\\\textquotedblright{}"),
               c("\u2020", "\\\\dag{}"),
               c("\u2021", "\\\\ddag{}"),
               c("\u2022", "\\\\textbullet{}"),
               c("\u2026", "\\\\dots{}"),
               c("\u2122", "\\\\texttrademark{}"),
               c("\u2423", "\\\\textvisiblespace{}"),
               c("\u00c6", "\\\\AE{}"),
               c("\u00e6", "\\\\ae{}"),
               c("\u0152", "\\\\OE{}"),
               c("\u0153", "\\\\oe{}"),
               c("\u00d8", "\\\\O{}"),
               c("\u00f8", "\\\\o{}"),
               c("\u0141", "\\\\L{}"),
               c("\u0142", "\\\\l{}"),
               ## U+1E9E Latin capital letter sharp s. Works with
               ## XeTeX and LuaTeX, provided that the character is
               ## present in the font. Otherwise \SS, which usually
               ## produces "SS".
               c("\u1e9e", "\\\\ifdefined\\\\XeTeXrevision\\\\iffontchar\\\\font\"1E9E\\\\symbol{\"1E9E}\\\\else\\\\SS\\\\fi\\\\else\\\\ifdefined\\\\directlua\\\\iffontchar\\\\font\"1E9E\\\\symbol{\"1E9E}\\\\else\\\\SS\\\\fi\\\\else\\\\SS\\\\fi\\\\fi{}"),
               c("\u00df", "\\\\ss{}"),
               ## U+017F Latin small letter long s
               c("\u017f", "\\\\ifdefined\\\\XeTeXrevision\\\\symbol{\"017F}\\\\else\\\\ifdefined\\\\directlua\\\\symbol{\"017F}\\\\else{\\\\fontencoding{TS1}\\\\selectfont s}\\\\fi\\\\fi{}")))
    ## Other non-ASCII letters, punctuation marks.
    ## These don't work with the OT1 font encoding.
    substitutions <-
        c(substitutions,
          list(c("\u00d0", "\\\\DH{}"),
               c("\u00f0", "\\\\dh{}"),
               c("\u0110", "\\\\DJ{}"),
               c("\u0111", "\\\\dj{}"),
               c("\u014a", "\\\\NG{}"),
               c("\u014b", "\\\\ng{}"),
               c("\u00de", "\\\\TH{}"),
               c("\u00fe", "\\\\th{}"),
               c("\u00ab", "\\\\guillemotleft{}"),
               c("\u00bb", "\\\\guillemotright{}"),
               c("\u201a", "\\\\quotesinglbase{}"),
               c("\u201e", "\\\\quotedblbase{}"),
               c("\u2039", "\\\\guilsinglleft{}"),
               c("\u203a", "\\\\guilsinglright{}")))
    ## Miscellaneous, arrows, delimiters, legal symbols, science
    ## and engineering, currencies, diacritics, text mode math.
    ## These require textcomp.
    substitutions <-
        c(substitutions,
          list(c("\u00a0", "~"),# no-break space (NBSP)
               c("\u00ad", "\\\\-"),# soft hyphen (SHY)
               c("\u200b", "\\\\hspace{0pt}"),# zero width space (ZWSP)
               c("\u2217", "\\\\textasteriskcentered{}"),
               c("\u2016", "\\\\textbardbl{}"),
               c("\u25ef", "\\\\textbigcircle{}"),
               c("\u2422", "\\\\textblank{}"),
               c("\u00a6", "\\\\textbrokenbar{}"),
               c("\u2052", "\\\\textdiscount{}"),
               c("\u212e", "\\\\textestimated{}"),
               c("\u203d", "\\\\textinterrobang{}"),
               c("\u2e18", "\\\\textinterrobangdown{}"),
               c("\u2116", "\\\\textnumero{}"),
               c("\u25e6", "\\\\textopenbullet{}"),
               c("\u2030", "\\\\textperthousand{}"),
               c("\u2031", "\\\\textpertenthousand{}"),
               c("\u211e", "\\\\textrecipe{}"),
               c("\u203b", "\\\\textreferencemark{}"),
               c("\u02f7", "\\\\texttildelow{}"),
               c("\u2190", "\\\\textleftarrow{}"),
               c("\u2191", "\\\\textuparrow{}"),
               c("\u2192", "\\\\textrightarrow{}"),
               c("\u2193", "\\\\textdownarrow{}"),
               c("\u3008", "\\\\textlangle{}"),
               c("\u3009", "\\\\textrangle{}"),
               c("\u301a", "\\\\textlbrackdbl{}"),
               c("\u301b", "\\\\textrbrackdbl{}"),
               c("\u2045", "\\\\textlquill{}"),
               c("\u2046", "\\\\textrquill{}"),
               c("\u2117", "\\\\textcircledP{}"),
               c("\u2120", "\\\\textservicemark{}"),
               c("\u2103", "\\\\textcelsius{}"),
               c("\u2127", "\\\\textmho{}"),
               c("\u00b5", "\\\\textmu{}"),
               c("\u03a9", "\\\\textohm{}"),
               c("\u0e3f", "\\\\textbaht{}"),
               c("\u00a2", "\\\\textcent{}"),
               c("\u20a1", "\\\\textcolonmonetary{}"),
               c("\u00a4", "\\\\textcurrency{}"),
               c("\u20ab", "\\\\textdong{}"),
               c("\u20ac", if (eurosym) "\\\\euro{}" else "\\\\texteuro{}"),
               c("\u20b2", "\\\\textguarani{}"),
               c("\u20a4", "\\\\textlira{}"),
               c("\u20a6", "\\\\textnaira{}"),
               c("\u20b1", "\\\\textpeso{}"),
               c("\u20a9", "\\\\textwon{}"),
               c("\u00a5", "\\\\textyen{}"),
               c("\u02dd", "\\\\textacutedbl{}"),
               c("\u00b4", "\\\\textasciiacute{}"),
               c("\u00b8", "\\\\c{}"),
               c("\u02d8", "\\\\textasciibreve{}"),
               c("\u02c7", "\\\\textasciicaron{}"),
               c("\u00a8", "\\\\textasciidieresis{}"),
               c("\u00af", "\\\\textasciimacron{}"),
               c("\u00b0", "\\\\textdegree{}"),
               c("\u00f7", "\\\\textdiv{}"),
               c("\u00bc", "\\\\textonequarter{}"),
               c("\u00bd", "\\\\textonehalf{}"),
               c("\u00be", "\\\\textthreequarters{}"),
               c("\u00d7", "\\\\texttimes{}"),
               c("\u00b1", "\\\\textpm{}"),
               c("\u00b9", "\\\\textonesuperior{}"),
               c("\u00b2", "\\\\texttwosuperior{}"),
               c("\u00b3", "\\\\textthreesuperior{}"),
               c("\u2044", "\\\\textfractionsolidus{}"),
               c("\u221a", "\\\\textsurd{}"),
               c("\u00ac", "\\\\textlnot{}"),
               c("\u2212", "\\\\textminus{}")))

    ## A more comprehensive list
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
    for (subst in substitutions) {
        y <- gsub(subst[1], subst[2], y, perl = TRUE)
    }
    if (isTRUE(doublebackslash)) {
        y <- gsub("\\", "\\\\", y, fixed=TRUE)
    }
    ## Convert result to UTF-8 NFC encoding, although most non-ASCII
    ## content has probably been converted to LaTeX commands.
    stri_trans_nfc(y)
}
