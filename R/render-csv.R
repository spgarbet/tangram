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
#' @return A matrix or list of strings containing key, source and value
#' @export
csv <- function(object, ...)
{
  UseMethod("csv", object)
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

  result
}


csv.numeric   <- function(object, ...) as.character(object)

csv.default <- function(object, ...) paste0("\"", object, "\"")


