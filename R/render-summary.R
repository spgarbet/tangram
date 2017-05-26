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
# Given the compiled tree of data, render as a text summary
#' @include compile-cell.R
summary.default <- function(object, ...) 
{
  warning(paste("summary unhandled class : ", paste(base::class(object), collapse=', ')))
  ""
}

summary.cell <- function(x, ...)
{
  sep  <- if(is.null(attr(x, "sep"))) ", " else attr(x, "sep")
  
  if(is.null(names(x)))
  {
    paste(x, collapse=sep)
  } else {
    name <- vapply(names(x), function(n) if(nchar(n)>0) paste0(n,"=") else "", "character")
    paste(paste0(name, as.character(x)), collapse=sep)
  }
}

summary.cell_iqr <- function(x, ...) 
{
  if(is.null(names(x)))
    paste0(x[1], " *", x[2], "* ", x[3])
  else
    paste0(names(x)[1], "=", x[1], " *", x[2], "* ", x[3])
}

summary.cell_estimate <- function(x,...)
{
  x <- summary(x[[1]])
  if(length(x) == 1) x else paste(x, " (", summary(x[[2]]), ")")
}

summary.cell_fraction <- function(x,...)
{
  den <- as.character(x['denominator'])
  num <- sprintf(paste("%",nchar(den),"s",sep=''), x['numerator'])
  paste0(x['ratio'], "  ", num, "/", den)
}

#' Create a text summary of a given table
#'
#' @param object The cell table to render to text
#' @param ... additional arguments to renderer. Unused at present.
#' @return A text string rendering of the given table
#' @export
#' @importFrom stringr str_pad
#'
summary.cell_table <- function(object,...)
{
  nrows <- rows(object)
  ncols <- cols(object)

  text <- matrix(data=rep("", nrows*ncols), nrow=nrows, ncol=ncols)

  last_header_row <- 0 # Current Header Row
  sapply(1:nrows, FUN=function(row) {
    sapply(1:ncols, FUN=function(col) {
      if(last_header_row == 0 && !inherits(object[[row]][[col]], "cell_header")) last_header_row <<- row - 1
      text[row,col] <<- summary(object[[row]][[col]])
    })
  })

  maxwidths <- apply(text, 2, FUN=function(x) max(nchar(x), na.rm=TRUE))

  sapply(1:nrows, FUN=function(row) {
    sapply(1:ncols, FUN=function(col) {
      if(col == 1)
      {
        text[row,col] <<- str_pad(text[row,col], maxwidths[col], "right")
      }
      else
      {
        text[row,col] <<- str_pad(text[row,col], maxwidths[col], "both")
      }
    })
  })

  pasty <- apply(text, 1, function(x) paste(x, collapse="  "))

  cat(paste(rep("=",nchar(pasty[1])),collapse=''),'\n')
  for(row in pasty)
  {
    cat(row,'\n')
    if(last_header_row > 0              &&
       length(pasty) >= last_header_row &&
       row == pasty[last_header_row])
    {
      cat(paste(rep("-",nchar(pasty[1])),collapse=''),'\n')
    }
  }
  cat(paste(rep("=",nchar(pasty[1])),collapse=''),'\n')

}

#' Print a text summary of a given table
#'
#' @param x The cell table to render to text
#' @param ... additional arguments, unused at present
#' @return A text string rendering of the given table
#' @export
#' @importFrom stringr str_pad
#'
print.cell_table <- function(x,...) {summary(x,...)}



