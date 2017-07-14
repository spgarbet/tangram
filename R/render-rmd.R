# tangram a general purpose table toolkit for R
# Copyright (C) 2017 Shawn Garbett
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

#######
# Given the compiled tree of data, render as a text rmd
#' @include compile-cell.R

rmd.default <- function(object,...) paste0(as.character(object), collapse=", ")

rmd.cell <- function(object, ...)
{
  sep  <- if(is.null(attr(object, "sep"))) ", " else attr(object, "sep")

  if(is.null(names(object)))
  {
    paste(object, collapse=sep)
  } else {
    name <- vapply(names(object), function(n) if(nchar(n)>0) paste0(n,"=") else "", "character")
    paste(paste0(name, as.character(object)), collapse=sep)
  }
}

rmd.cell_iqr <- function(object,...)
{
  paste0(object[1],
         " **", object[2], "** ",
         object[3])
}


rmd.cell_estimate <- function(object,...)
{
  paste0("(", rmd(object[1]), ", ", rmd(object[2]), ")")
}

rmd.cell_fstat <- function(object,...)
{
  paste0("F<sub>",object[2],",",object[3],"</sub>=",object[1],", P=",object[4])
}

rmd.cell_fraction <- function(object,...)
{
  den <- object["denominator"]
  num <- paste0(rep("", nchar(den) - nchar(object["numerator"])), object["numerator"])
  paste0(object["ratio"], "  ", num,"/",den)
}

rmd.cell_chi2 <- function(object,...)
{
  paste0("\u03a7^2^~", object[2], "~=", object[1], ", P=", object[3])
}

rmd.cell_studentt <- function(object,...)
{
  paste0("T~",object[2],"~=",object[1], ", P=",object[3])
}

rmd.cell_spearman <- function(object,...)
{
  paste0("S=",object[1],", P=",object[1])
}

rmd.cell_n <- function(object,...)
{
  if (inherits(object, "cell_header"))
    paste0("(N=",as.character(object),")")
  else
    as.character(object)
}

#' Generate an Rmd table entry from a tangram object
#'
#' Given a tangram object generate the corresponding piece of an Rmd table
#'
#' @param object The cell_fstat for indexing
#' @param ... additional arguments to renderer. Unused
#' @return A string representation of the table
#' @export
#'
#' @importFrom stringr str_pad
rmd.tangram <- function(object,...)
{
  nrows <- rows(object)
  ncols <- cols(object)

  text <- matrix(data=rep("", nrows*ncols), nrow=nrows, ncol=ncols)

  last_header_row <- 0 # Current Header Row
  sapply(1:nrows, FUN=function(row) {
    sapply(1:ncols, FUN=function(col) {
      if(last_header_row == 0 && !inherits(object[[row]][[col]], "cell_header")) last_header_row <<- row - 1
      text[row,col] <<- rmd(object[[row]][[col]])
    })
  })

  # Pad strings
  sapply(1:ncols, FUN=function(col) {
    if(is.na(text[1,col]))
    {
      text[1,col] <<- "          "
    } else if(nchar(text[1,col]) < 10)
    {
      text[1,col] <<- str_pad(text[1,col], width=10, side="both");
    }
  })

  pasty <- apply(text, 1, function(x) paste(c("|", paste(x, collapse="|"), "|"), collapse=""))

  cat(pasty[1], '\n')
  cat(gsub("-\\|", ":|", gsub("[^\\|]", "-", pasty[1])), '\n')

  for(row in pasty[2:nrows]) cat(row, '\n')

}

#' Generate an Rmd table entry from a cell object
#'
#' Given a cell object generate the corresponding piece of an Rmd table
#'
#' @param object The cell_fstat for indexing
#' @param ... additional arguments to renderer. Unused
#' @return A string representation of the table
#' @export
#'
rmd <- function(object, ...)
{
  UseMethod("rmd", object)
}

#' @rdname summary
#' @export
rmd.table_builder <- function(object,...)
{
  rmd(table_flatten(object$table))
}


