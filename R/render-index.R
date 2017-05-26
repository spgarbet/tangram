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
# Given the compiled tree of data, render as a text index

#' Key derivation helper function
#'
#' @param x cell object to derive key for
#' @export
key <- function(x)
{
  if(is.null(attr(x, "row")) || is.null(attr(x, "col"))) return(NA)

  row    <- attr(x, "row")
  col    <- attr(x, "col")
  label  <- attr(x, "names")
  subrow <- attr(x, "subrow")
  subcol <- attr(x, "subcol")

  rv <- if(is.null(subrow)) row$value else paste0(row$value, '[',subrow,']')
  cv <- if(is.null(subcol)) col$value else paste0(col$value, '[',subcol,']')
  if(is.null(label)) paste0(rv,":",cv) else paste0(rv,":",cv,":",paste0(label, collapse=''))
}

#' Generate an index from a cell object
#'
#' Given a cell class create an index representation.
#'
#' @param object The cell header to render to HTML5
#' @param ... additional arguments to renderer. Unused
#' @return A matrix of strings containing key, source and value
#' @export
#'
index <- function(object, ...)
{
  UseMethod("index", object)
}

#' @importFrom base64enc base64encode
#' @importFrom digest digest
index_content <- function(object,caption,value)
{
  if(!("src" %in% names(object))) return(NULL)
  if(is.na(object$src)) return(NULL)
  src <- paste(caption, object$src, sep=":")
  idx <- substr(base64encode(charToRaw(digest(src))), 1, 4)

  result <- c(idx, src, value)
  names(result) <- c("key", "src", "value")
  result
}


#' Generate an index from a cell object
#'
#' Given a cell class create an index representation. If no source
#' is specified no index will be generated.
#'
#' @param object The cell for indexing
#' @param caption an additional specifier for the object key
#' @param ... additional arguments to renderer. Unused
#' @return A matrix of strings containing key, source and value
#' @export
#'
index.default <- function(object,caption, ...)
{
  if(!("src" %in% names(object))) return(NULL)
  if(is.na(object$src)) return(NULL)
  src <- paste(caption, object$src, sep=":")
  nms <- names(object)
  lapply(nms[!nms %in% c('label','src','units')],
         function(y)
         {
           idx <- substr(base64encode(charToRaw(digest(c(src,y)))), 1, 4)

           c(idx, paste(src, y, sep=':'), as.character(object[[y]]))
           #paste(idx, paste(src, y, sep=':'), object[[y]], sep=",")
         })
}

#' Generate an index from a cell_n object
#'
#' Given a cell_n class create an index representation. If no source
#' is specified no index will be generated.
#'
#' @param object The cell_n for indexing
#' @param caption an additional specifier for the object key
#' @param ... additional arguments to renderer. Unused
#' @return A matrix of strings containing key, source and value
#' @export
#'
index.cell_n <- function(object, caption, ...)
{
  index_content(object, caption, object$n)
}

#' Generate an index from a cell_estimate object
#'
#' Given a cell_estimate class create an index representation. If no source
#' is specified no index will be generated.
#'
#' @param object The cell_estimate for indexing
#' @param caption an additional specifier for the object key
#' @param ... additional arguments to renderer. Unused
#' @return A matrix of strings containing key, source and value
#' @export

#'
index.cell_estimate <- function(object, caption, ...)
{
  content <- if(is.na(object$low))
    as.character(object$value)
  else
    paste(object$value,
          " (",object$low,", ",object$high,")",
          sep="")

  index_content(object, caption, content)
}

#' Generate an an index from a cell_quantile object
#'
#' Given a cell_quantile class create an index representation. If no source
#' is specified no index will be generated.
#'
#' @param object The cell_quantile for indexing
#' @param caption an additional specifier for the object key
#' @param ... additional arguments to renderer. Unused
#' @return A matrix of strings containing key, source and value
#' @export
#'
index.cell_quantile <- function(object, caption, ...)
{
  content <-
    paste(object$'50%',
          " [",object$'25%',", ",object$'75%',"]",
          sep="")

  index_content(object, caption, content)
}

#' Generate an an index from a cell_fstat object
#'
#' Given a cell_fstat class create an index representation. If no source
#' is specified no index will be generated.
#'
#' @param object The cell_fstat for indexing
#' @param caption an additional specifier for the object key
#' @param ... additional arguments to renderer. Unused
#' @return A matrix of strings containing key, source and value
#' @export
#'
index.cell_fstat <- function(object, caption, ...)
{
  content <-
    paste("F=", object$f,", p = ", object$p, sep='')

  index_content(object, caption, content)
}

#' Generate an an index from a cell_fraction object
#'
#' Given a cell_fraction class create an index representation. If no source
#' is specified no index will be generated.
#'
#' @param object The cell_fraction for indexing
#' @param caption an additional specifier for the object key
#' @param ... additional arguments to renderer. Unused
#' @return A matrix of strings containing key, source and value
#' @export
#'
index.cell_fraction <- function(object, caption, ...)
{
  content <-
    paste(round(object$numerator/object$denominator, 3),
          "  ",
          object$numerator,
          "/",
          object$denominator,
          sep='')

  index_content(object, caption, content)
}

#' Generate an an index from a cell_chi2 object
#'
#' Given a cell_chi2 class create an index representation. If no source
#' is specified no index will be generated.
#'
#' @param object The cell_chi2 for indexing
#' @param caption an additional specifier for the object key
#' @param ... additional arguments to renderer. Unused
#' @return A matrix of strings containing key, source and value
#' @export
#'
index.cell_chi2 <- function(object, caption, ...)
{
  content <-
    paste("chisq=",
          object$chi2,
          ", p=",
          object$p,
          sep='')

  index_content(object, caption, content)
}

#' Generate an an index from a cell_table object
#'
#' Given a cell_table class create an index representation.
#'
#' @param object The cell_table for indexing
#' @param caption an additional specifier for the object key
#' @param ... additional arguments to renderer. Unused
#' @return A matrix of strings containing key, source and value
#' @export
#'
index.cell_table <- function(object, caption="Table",...)
{
  nrows <- rows(object)
  ncols <- cols(object)

  # Render it all
  result<-
  unlist(sapply(1:nrows, simplify=FALSE, FUN=function(row) {
    unlist(sapply(1:ncols, simplify=FALSE, FUN=function(col) {
      c(index(object[[row]][[col]], caption))
    }))
  }))

  names(result) <- NULL
  result <- matrix(result, ncol=3, byrow=TRUE)
  colnames(result) <- c("key", "src", "value")
  result
}

