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

#' Create a function to transform all cells of a table
#'
#' Given a function that operates on a table cell and returns
#' the modified cell, return a function that given a table
#' applies that function to all cells and returns the modified
#' table.
#'
#' @param FUN function to apply, must return the modified cell
#' @param ... additional arguments to pass into function
#' @return a table modification function
#' @export
cell_transform <- function(FUN, ...)
{
  function(table)
  {
    sapply(1:rows(table), function(row) {
      sapply(1:cols(table), function(col) {
        table[[row]][[col]] <<- FUN(table[[row]][[col]], ...)
      })
    })
    table
  }
}

#' Delete a given column from a table
#'
#' Given a table, remove the specified column
#' @param table the table to modify
#' @param col the number of the column to drop
#' @return the modified table
#' @export
del_col <- function(table, col)
{
  sapply(1:length(table), function(row) {
    cols <- length(table[[row]])
    if(col < cols) sapply((col+1):cols, function(i) table[[row]][[i-1]] <<- table[[row]][[i]])
    table[[row]][[cols]] <<- NULL
  })
  table
}

#' Delete a given row from a table
#'
#' Given a table, remove the specified row
#' @param table the table to modify
#' @param row the number of the row to drop
#' @return the modified table
#' @export
del_row <- function(table, row)
{
  rows <- length(table)
  if(row < rows)
    sapply((row+1):rows, function(i) table[[i-1]] <<- table[[i]])
  table[[rows]] <- NULL
  table
}

#' Drop all statistics columns from a table.
#'
#' Delete from a table all columns that contain statistics
#'
#' @param table the table to remove statistical columns
#' @return the modified table
#' @export
drop_statistics <- function(table)
{
  columns <- (1:length(table[[1]]))[sapply(1:length(table[[1]]), function(col) {
    any(sapply(1:length(table), function(row) {
      "statistics" %in% class(table[[row]][[col]])
    }))
  })]

  # Deleting columns changes the number of columns, so do in reverse order
  sapply(rev(columns), function(col) {table <<- del_col(table, col)})

  table
}

#' Cleanup an intercept only model
#'
#' Cleanup an intercept only table that was generated from the hmisc default
#' transform. This drops the statistics column, and modifies the header
#' to eliminate blank space.
#'
#' @param table the table to modify
#' @return the modified table
#' @export
hmisc_intercept_cleanup <- function(table)
{
  table <- drop_statistics(table)

  # Roll up header here
  sapply(1:length(table[[1]]), function(col)
  {
    up    <- table[[1]][[col]]
    below <- table[[2]][[col]]

    if(!("cell_label" %in% class(up)) ||
       up == "")
    {
      class(below) <- class(below)[length(class(below))]
      table[[1]][[col]] <<- below
    }
  })

  del_row(table, 2)
}
