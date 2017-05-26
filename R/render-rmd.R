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

rmd.default <- function(object,...) ""

rmd.cell_label <- function(object,...)
{
  if(is.na(object$units))
  {
    if(length(object$label) == 0) return("") else return(object$label)
  } else {
    return(paste(object$label, " (", object$units, ")", sep=""))
  }
}

rmd.cell_quantile <- function(object,...)
{
  paste(render_f(object$'25%',object$format),
        " **", render_f(object$'50%', object$format), "** ",
        render_f(object$'75%', object$format),
        sep="")
}


rmd.cell_estimate <- function(object,...)
{
  if(is.na(object$low))
    render_f(object$value)
  else
    paste(render_f(object$value)," (",render_f(object$low),", ",render_f(object$high),")", sep='')
}

rmd.cell_fstat <- function(object,...)
{
  paste("F<sub>",object$n1,",",object$n2,"</sub>=",render_f(object$f),", P=",render_f(object$p),sep="")
}

rmd.cell_fraction <- function(object,...)
{
  x <- render_f(object$ratio)
  den <- as.character(object$denominator)
  num <- sprintf(paste("%",nchar(den),"s",sep=''), object$numerator)
  paste(x, "  ",
        num,"/",den,
        sep="")
}

rmd.cell_chi2 <- function(object,...)
{
  paste("&chi;<span class=\"supsub\" style=\"display:inline-block;margin:-9em 0;vertical-align: -0.55em;line-height: 1.35em;font-size: x-small;text-align: left;\">2<br/>",
        object$df,"</span>=",render_f(object$chi2),", P=",render_f(object$p),sep="")
}

rmd.cell_studentt <- function(object,...)
{
  paste("T<sub>",object$df,"</sub>=",render_f(object$t), ", P=",render_f(object$p), sep="")
}

rmd.cell_spearman <- function(object,...)
{
  paste("S=",render_f(object$S),", P=",render_f(object$p), sep="")
}

rmd.cell_n <- function(object,...)
{
  if (inherits(object, "cell_header"))
    paste("(N=",as.character(object$n),")",sep='')
  else
    as.character(object$n)
}

#' Generate an Rmd table entry from a cell_table object
#'
#' Given a cell_table object generate the corresponding piece of an Rmd table
#'
#' @param object The cell_fstat for indexing
#' @param ... additional arguments to renderer. Unused
#' @return A string representation of the table
#' @export
#'
#' @importFrom stringr str_pad
rmd.cell_table <- function(object,...)
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

