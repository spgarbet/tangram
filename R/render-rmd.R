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
#' Generate an Rmd table entry from a cell object
#'
#' Given a cell object generate the corresponding piece of an Rmd table
#'
#' @include compile-cell.R
#' @param object The cell_fstat for indexing
#' @param key A filename to write key values into. Can be false if no key file is desired.
#' @param append logical; Should the key file be appended too, or overwritten
#' @param ... additional arguments to renderer. Unused
#' @return A string representation of the table
#' @rdname rmd
#' @export
#' @examples
#'  rmd(tangram(drug ~ bili, pbc))
rmd <- function(object, key=FALSE, ...)
{
  UseMethod("rmd", object)
}

#' @rdname rmd
#' @export
rmd.default <- function(object, key=FALSE, ...) paste0(as.character(object), collapse=", ")

#' @export
#' @rdname rmd
rmd.cell <- function(object, key=FALSE, ...)
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

#' @export
#' @rdname rmd
rmd.cell_iqr <- function(object, key=FALSE, ...)
{
  if(key)
  {
    idx <- index(object, ...)
    paste0("((",    object[1], "))%", word_ref(idx[[1]]), "%",
           " **((", object[2], "))%", word_ref(idx[[2]]), "%** ",
           "((",    object[3], "))%", word_ref(idx[[3]]), "%"
           )
  } else
  {
    paste0(object[1],
           " **", object[2], "** ",
           object[3])
  }
}

#' @export
#' @rdname rmd
rmd.cell_estimate <- function(object, key=FALSE, ...)
{
  paste0("(", rmd(object[1]), ", ", rmd(object[2]), ")")
}

#' @export
#' @rdname rmd
rmd.cell_fstat <- function(object, key=FALSE, ...)
{
  paste0("F~",object[2],",",object[3],"~=",object[1],", P=",object[4])
}

#' @export
#' @rdname rmd
rmd.cell_fraction <- function(object, key=FALSE, ...)
{
  den <- object["denominator"]
  num <- object["numerator"]
  paste0(object["ratio"], "  ", num,"/",den)
}

#' @export
#' @rdname rmd
rmd.cell_chi2 <- function(object, key=FALSE, ...)
{
  if(key)
  {
    idx <- index(object, ...)
    paste0("X^2^((~", object[2], "~))%", word_ref(idx[[2]]), "%",
           "=((", object[1], "))%",    word_ref(idx[[1]]), "%",
           ", P=((", object[3], "))%",  word_ref(idx[[3]]), "%")
  } else
  {
    paste0("X^2^~", object[2],
           "~=", object[1],
           ", P=", object[3])
  }
}

#' @export
#' @rdname rmd
rmd.cell_studentt <- function(object, key=FALSE, ...)
{
  idx <- index(object, key=FALSE, ...)

  paste0("T~",object[2],"~=",object[1], ", P=",object[3])
}

#' @export
#' @rdname rmd
rmd.cell_spearman <- function(object, key=FALSE, ...)
{
  paste0("S=",object[1],", P=",object[1])
}

#' @export
#' @rdname rmd
rmd.cell_n <- function(object, key=FALSE, ...)
{
  rep <- if(key)
  {
    idx <- index(object, ...)
    paste0("((", as.character(object), "))%", word_ref(idx[[1]]), "%")
  } else
  {
    as.character(object)
  }
  if (inherits(object, "cell_header")) paste0("(N=",rep,")") else rep
}


#' @rdname rmd
#' @export
#'
#' @importFrom stringr str_pad
#' @importFrom utils write.table
rmd.tangram <- function(object, key=NULL, append=FALSE, ...)
{
  nrows <- rows(object)
  ncols <- cols(object)

  text <- matrix(data=rep("", nrows*ncols), nrow=nrows, ncol=ncols)

  last_header_row <- 0 # Current Header Row
  sapply(1:nrows, FUN=function(row) {
    sapply(1:ncols, FUN=function(col) {
      if(last_header_row == 0 && !inherits(object[[row]][[col]], "cell_header")) last_header_row <<- row - 1
      text[row,col] <<- rmd(object[[row]][[col]], key=!is.null(key), ...)
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

  cat('\n') # An RMarkdown table must be preceeded by a newline or bad things happen

  pasty <- apply(text, 1, function(x) paste(c("|", paste(x, collapse="|"), "|"), collapse=""))

  cat(pasty[1], '\n')
  cat(gsub("-\\|", ":|", gsub("[^\\|]", "-", pasty[1])), '\n')

  for(row in pasty[2:nrows]) cat(row, '\n')

  if(!is.null(key)) write.table(index(object, ...), key, col.names=FALSE, row.names=FALSE, append=append, sep=",", quote=FALSE)
}

#' @rdname rmd
#' @export
rmd.table_builder <- function(object, key=FALSE, ...)
{
  rmd(table_flatten(object$table), key=key, ...)
}


