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


#' Generate an csv from a tangram or cell object
#'
#' Given a tangram object create an index representation.
#'
#' @param object The cell header to render to HTML5
#' @param ... additional arguments to renderer. Unused
#' @return A string containing the csv file
#' @rdname csv
#' @export
csv <- function(object, ...)
{
  UseMethod("csv", object)
}

#' @param file File to write result into
#' @param sep separator to use
#' @rdname csv
#' @export
csv.tangram <- function(object, file=NULL, sep=',', ...)
{
  nrows <- rows(object)
  ncols <- cols(object)

  # Render it all
  result<-
  paste0(sapply(1:nrows, simplify=FALSE, FUN=function(row) {
    paste0(sapply(1:ncols, simplify=FALSE, FUN=function(col) {
      csv(object[[row]][[col]])
    }),collapse=sep)
  }), collapse="\n")

  if(!is.null(file)) write(result, file=file)

  invisible(result)
}

#' @rdname csv
#' @export
csv.cell_subheader <- function(object, ...)
{
  if(nchar(object) < 1) object <- " "
  paste0("\"_", object, "_\"")
}

#' @rdname csv
#' @export
csv.cell_header <- function(object, ...)
{
  if(nchar(object) < 1) object <- " "
  paste0("\"**", object, "**\"")
}


#' @rdname csv
#' @export
csv.cell_fstat <- function(object, ...)
{
  paste0("F~", object[2], ",", object[3], "~=", object[1], ", P=", object[4])
}

#' @rdname csv
#' @export
csv.cell_chi2 <- function(object, ...)
{
  paste0("X^2^~", object[2], "~=", object[1], ", P=", object[3])
}

#' @rdname csv
#' @export
csv.cell_studentt <- function(object, ...)
{
  paste0("t~", object[2], "~=", object[1], ", P=", object[3])
}

#' @rdname csv
#' @export
csv.default <- function(object, ...) paste0("\"", summary(object), "\"", collapse='')

#' @rdname csv
#' @export
csv.table_builder <- function(object,...) csv(table_flatten(object$table))

