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

#######
#' Generate an Rmd table entry from a cell object
#'
#' Given a cell object generate the corresponding piece of an Rmd table
#'
#' @include compile-cell.R
#' @param object The cell_fstat for indexing
#' @param key A filename to write key values into. Can be false if no key file is desired.
#' @param append logical; Should the key file be appended too, or overwritten
#' @param pad numeric; Minimum width of columns can be a single or vector of numerics.
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

#' @include iify.R
rmdify <- function(x) iify(x, list(
  #c("&nbsp;",     "  "),
  c("\\\\frac\\{\\s*([^\\}]*)}\\{\\s*([^\\}]*)\\}", "\\1/\\2"),
  c("  ", "&nbsp;&nbsp;"),
  c("\u03A7", "X")
))

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
rmd.tangram <- function(object, key=NULL, append=FALSE, pad=10, ...)
{
  if(!is.null(attr(object, "caption"))) cat('\n', attr(object, "caption"), '\n',sep='')

  nrows <- rows(object)
  ncols <- cols(object)

  text <- matrix(data=rep("", nrows*ncols), nrow=nrows, ncol=ncols)

  last_header_row <- 0 # Current Header Row
  sapply(1:nrows, FUN=function(row) {
    sapply(1:ncols, FUN=function(col) {
      if(last_header_row == 0 && !inherits(object[[row]][[col]], "cell_header")) last_header_row <<- row - 1
      if(!is.null(attr(object[[row]][[col]], "colspan"))) warning("colspan not supported for Rmd tangram rendering")
      if(!is.null(attr(object[[row]][[col]], "rowspan"))) warning("rowspan not supported for Rmd tangram rendering")

      text[row,col] <<- rmd(rmdify(object[[row]][[col]]), key=!is.null(key), ...)
    })
  })

  # Pad strings in first row
  sapply(1:ncols, FUN=function(col) {
    ipad <- if(length(pad) == 1) pad else max(pad[col], 5)
    if(is.na(text[1,col]))
    {
      text[1,col] <<- paste0(rep(" ", ipad),      collapse='')
    } else if(nchar(text[1,col]) < ipad)
    {
      text[1,col] <<- str_pad(text[1,col], width=ipad, side="both")
    }
  })

  results <- '\n' # An RMarkdown table must be preceeded by a newline or bad things happen

  pasty <- apply(text, 1, function(x) paste(c("|", paste(x, collapse="|"), "|"), collapse=""))

  results <- paste0(results, pasty[1], '\n')
  results <- paste0(results, sub(":\\|", "-|", gsub("\\|-", "|:", gsub("-\\|", ":|", gsub("[^\\|]", "-", pasty[1])))), '\n')

  for(row in pasty[2:nrows]) results <- paste0(results, row, '\n')

  if(!is.null(key)) write.table(index(object, ...), key, col.names=FALSE, row.names=FALSE, append=append, sep=",", quote=FALSE)

  if(!is.null(attr(object, "footnote")))
  {
    results <- paste0(results, '\n')
    results <- paste0(results, rmdify(paste0(attr(object, "footnote"), collapse="\n")), collapse='\n' )
  }

  class(results) <- c("knit_asis", "character")
  attr(results, "knit_cacheable") <- NA

  results
}



