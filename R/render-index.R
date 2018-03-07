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

word_ref <- function(idx) paste(idx$index, idx$src)

# Helper function to copy source attributes used in key generation
# from y, to x and returns modified x.
copy_src <- function(x, y)
{
  attr(x, "row") <- attr(y, "row")
  attr(x, "col") <- attr(y, "col")
  attr(x, "subrow") <- attr(y, "subrow")
  attr(x, "subcol") <- attr(y, "subcol")

  x
}

#' Key derivation helper function
#'
#' This function should generate a string that uniquely identifies a piece
#' of data present in a table. In a report with multiple tables the id
#' is used to preserve uniqueness.
#'
#' This function relies on the object being keyed having at a minimum
#' character attributes for \code{row} and \code{col}. Additional
#' specifies for embedded tables are given with \code{subrow} and
#' \code{subcol}. The \code{row} and \code{col} are automatically
#' appended when using a \code{table_builder}. However the
#' \code{subrow} and \code{subcol} must be added by the user to
#' a cell of a table.
#'
#' @param x cell object to derive key for
#' @param id the unique id of the table being keyed
#' @export
key <- function(x, id)
{
  if(is.null(attr(x, "row")) || is.null(attr(x, "col"))) return(NULL)

  row    <- attr(x, "row")
  col    <- attr(x, "col")
  subrow <- attr(x, "subrow")
  subcol <- attr(x, "subcol")

  rv <- if(is.null(subrow)) row else paste0(row, '[',subrow,']')
  cv <- if(is.null(subcol)) col else paste0(col, '[',subcol,']')

  paste0(id, ":", rv,":",cv)
}

#' Generate an index from a tangram or cell object
#'
#' Given a tangram object create an index representation.
#'
#' @param object The cell header to render to HTML5
#' @param ... additional arguments to renderer. Unused
#' @return A matrix or list of strings containing key, source and value
#' @export
index <- function(object, ...)
{
  UseMethod("index", object)
}

#' Generate an an index from a tangram object
#'
#' Given a tangram class create an index representation.
#'
#' @param object The tangram for indexing
#' @param id an additional specifier for the object key
#' @param key.len numeric; length of keys generated (affects collision probability)
#' @param ... additional arguments to renderer. Unused
#' @return A matrix of strings containing key, source and value
#' @export
#'
index.tangram <- function(object, id="tangram", key.len=4, ...)
{
  nrows <- rows(object)
  ncols <- cols(object)

  # Render it all
  result<-
  unlist(sapply(1:nrows, simplify=FALSE, FUN=function(row) {
    unlist(sapply(1:ncols, simplify=FALSE, FUN=function(col) {
      c(index(object[[row]][[col]], id, key.len=key.len))
    }))
  }))

  names(result) <- NULL
  result <- matrix(result, ncol=3, byrow=TRUE)
  colnames(result) <- c("key", "src", "value")
  result
}


#' Generate an index from a cell object
#'
#' Given a cell class create an index representation. If no source
#' is specified no index will be generated.
#'
#' @param object cell; The cell for indexing
#' @param id character; an additional specifier for the object key
#' @param name character; optional names of elements inside object
#' @param key.len numeric; length of generated key
#' @param ... additional arguments to renderer. Unused
#' @return A list of strings containing key, source and value
#' @export
#'
#' @importFrom base64enc base64encode
#' @importFrom digest digest
index.default <- function(object, id="tangram", name=NULL, key.len=4, ...)
{
  src <- key(object, id)
  if(is.null(src)) return(NULL)

  nms <- if(is.null(name)) names(object)    else nms
  nms <- if(is.null(nms))  paste0(class(object)[1], 1:length(object)) else nms

  value <- as.character(object[!is.na(nms) & nchar(nms) > 0])
  nms   <- nms[!is.na(nms) & nchar(nms) > 0]
  srcs  <- paste0(src, ":", nms)

  idx   <- vapply(srcs,
                  function(x) substr(base64encode(charToRaw(digest(x))),1,key.len),
                  "character")

  lapply(1:length(idx), function(i){
    list(index=idx[i], src=srcs[i], value=value[i])
  })
}

#' Generate an index from a list object
#'
#' Given a cell class create an index representation. If no source
#' is specified no index will be generated.
#'
#' @param object cell; The cell for indexing
#' @param id character; an additional specifier for the object key
#' @param key.len numeric; length of key to generate
#' @param ... additional arguments to renderer. Unused
#' @return A list of strings containing key, source and value
#' @export
#'
index.list <- function(object, id="tangram", key.len=4, ...)
{
  x <- lapply(object,
         function(i) {
           i <- copy_src(i, object)
           index(i,id=id,key.len=key.len, ...)
         })

  do.call(c, x)
}

#' Generate an index from a label object
#'
#' Overrides to generate no indexing on labels
#'
#' @param object cell; The cell for indexing
#' @param id character; an additional specifier for the object key
#' @param key.len numeric; length of key to generate
#' @param ... additional arguments to renderer. Unused
#' @return A list of strings containing key, source and value
#' @export
index.cell_label <- function(object, id="tangram", key.len=4, ...)
{
  if("cell_value" %in% class(object))
  {
    cls <- class(object)
    pos <- match("cell_label",cls) + 1
    class(object) <- cls[pos:length(cls)]
    index(object)
  } else {
    NULL
  }
}

