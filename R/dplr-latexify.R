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
latexify <- function(x)
{
  y <- as.character(x) # Make sure a character string was passed

  ## Kludge for converting from "byte" to the current encoding
  ## in a way which preserves the hex notation.
  encBytes <- Encoding(y) == "bytes"
  if (any(encBytes)) y[encBytes] <- capture.output(cat(y[encBytes], sep = "\n"))

  ## Convert strings to UTF-8 encoding, NFD (decomposed) form, for
  ## processing of accented characters. Doing this early to
  ## circumvent pecularities in gsub() (and nchar()) when working in
  ## the C locale.
  y <- stri_trans_nfd(y)

  ## Run all conversions as appropriate
  for (subst in gsub_table) y <- gsub(subst[1], subst[2], y, perl = TRUE)

  # Save this for a bit see if we need it.
  # Letters <- paste0(c(LETTERS, letters), collapse="")
  #
  #
  #   ## After a command, remove empty group (when at the end of the
  #   ## string or some suitable character follows) or replace it with a
  #   ## space (when a space does not follow).
  #   tmp <- paste0("(\\\\[", Letters, "]+){}")
  #   substitutions <-
  #       c(substitutions,
  #         list(c(paste0(tmp, "(?=$|[[:digit:],.?!;:\\\\}+*/-])"), "\\1"),
  #              c(paste0(tmp, "(?! )"), "\\1 ")))
  #

  # # DEBUG VERSION
  # # Apply the substitutions in the list
  # i <- 1
  # for (subst in gsub_table) {
  #     before <- y
  #     y <- gsub(subst[1], subst[2], y, perl = TRUE)
  #     if(before != y)
  #     {
  #       cat("applying ", i, "\n")
  #       cat("  before: ", before, "\n")
  #       cat("  after:  ", y, "\n")
  #       cat(subst)
  #       cat("\n","\n")
  #
  #     }
  #     i <- i + 1
  # }

    ## Convert result to UTF-8 NFC encoding, although most non-ASCII
    ## content has probably been converted to LaTeX commands.
    stri_trans_nfc(y)
}
