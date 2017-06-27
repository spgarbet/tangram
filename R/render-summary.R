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
# The default method for rendering tangram objects
# Not exported since this would impact core R.
#' @include compile-cell.R
#' @include compile.R
summary.default <- function(object, ...)
{
  warning(paste("summary unhandled class : ", paste(base::class(object), collapse=', ')))

  ""
}

#####################################################
# The character method for rendering tangram objects
# Not exported since this would impact core R.
summary.character <- function(object, ...) object

#' Render methods for tangram cell objects
#'
#' Each of these methods will render the cell object as a text summary
#'
#' @param object object; the item to render
#' @param ... additional arguments passed to summary
#' @return the text summary
#' @examples
#' summary(cell_label("123"))
#' summary(cell_iqr(rnorm(20)))
#' summary(cell_estimate(2.1,0.8, 3.3))
#' summary(cell_fraction(45, 137))
#' summary(table_builder()   %>%
#'         row_header("row") %>%
#'         col_header(1,2,3) %>%
#'         add_col("A","B","C"))
#' summary(tangram(drug~bili, pbc))
#' @rdname summary
#' @export
summary.cell <- function(object, ...)
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

#' @rdname summary
#' @export
summary.cell_iqr <- function(object, ...)
{
  if(is.null(names(object)))
    paste0(object[1], " *", object[2], "* ", object[3])
  else
    paste0(names(object)[1], "=", object[1], " *", object[2], "* ", object[3])
}

#' @rdname summary
#' @export
summary.cell_range <- function(object, ...)
{
  sep <- if(is.null(attr(object, "sep"))) ", " else attr(object, "sep")
  paste0("(", object[1], sep, object[2], ")")
}

#' @rdname summary
#' @export
summary.cell_estimate <- function(object,...)
{
  paste0(c(summary(object[[1]]), summary(object[[2]])), collapse=' ')
}

#' @rdname summary
#' @export
summary.cell_fraction <- function(object,...)
{
  den <- as.character(object['denominator'])
  num <- sprintf(paste("%",nchar(den),"s",sep=''), object['numerator'])
  paste0(object['ratio'], "  ", num, "/", den)
}

#' @rdname summary
#' @export
summary.table_builder <- function(object,...)
{
  summary(table_flatten(object$table))
}

#' @rdname summary
#' @export
summary.cell_fstat <- function(object, ...)
{
  paste0("F_{", object[2], ",", object[3], "}=", object[1], ", P=", object[4])
}

#' @rdname summary
#' @export
summary.cell_chi2 <- function(object, ...)
{
  paste0("X^2_", object[2], "=", object[1], ", P=", object[3])
}

summary.cell_studentt <- function(object, ...)
{
  paste0("t_", object[2], "=", object[1], ", P=", object[3])
}

#' @rdname summary
#' @export
summary.tangram <- function(object,...)
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
#' @param x The tangram to render to text
#' @param ... additional arguments, unused at present
#' @return A text string rendering of the given table
#' @export
print.cell <- function(x,...) print(summary(x,...))

#' Print a text summary of a given table_builder
#'
#' @param x The table_builder to render to text
#' @param ... additional arguments, unused at present
#' @return A text string rendering of the given table
#' @export
print.table_builder <- function(x,...) print(summary(x,...))

