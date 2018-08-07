# tangram a general purpose table toolkit for R
# Copyright (C) 2017-2018 Shawn Garbett
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


#' @importFrom stringi stri_trans_nfc
#' @importFrom stringi stri_trans_nfd
#' @importFrom utils capture.output
iify <- Vectorize(function(x, sub_table)
{
  if(is.na(x) || is.null(x) || x == "") return("")

  y <- as.character(x)                        # Make sure a character string was passed
  if(is.null(x) || nchar(y) == 0) return("")  # Abort early for zero characters

  ## Kludge for converting from "byte" to the current encoding
  ## in a way which preserves the hex notation.
  encBytes <- Encoding(y) == "bytes"
  if (any(encBytes)) y[encBytes] <- capture.output(cat(y[encBytes], sep = "\n"))

  ## Convert strings to UTF-8 encoding, NFD (decomposed) form, for
  ## processing of accented characters. Doing this early to
  ## circumvent pecularities in gsub() (and nchar()) when working in
  ## the C locale.
  y <- stri_trans_nfd(y)

  ## Run all conversions as appropriate not inside "$"
  pieces  <- strsplit(y, "(?<!\\\\)\\$", perl=TRUE)[[1]]
  for(i in 1:length(pieces))
  {
    if((i %% 2) != 0)
    {
      j <- 0
      for (subst in sub_table)
      {
        #j <- j + 1
        #old <- pieces[i]
        pieces[i] <- gsub(subst[1], subst[2], pieces[i], perl = TRUE)
        #if(old != pieces[i]) cat("Applied '", j, dput(subst[1]), "=>", dput(subst[2]), "'\n")
      }
    }
  }
  y <- paste0(pieces, collapse="")

  ## Convert result to UTF-8 NFC encoding, although most non-ASCII
  ## content has probably been converted to LaTeX commands.
  stri_trans_nfc(y)
}, vectorize.args="x", USE.NAMES=FALSE)
