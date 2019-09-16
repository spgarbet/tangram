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

#' Delete given column(s) from a table
#'
#' Given a table, remove the specified column
#' @param table the table to modify
#' @param col vector containing column(s) to drop
#' @return the modified table
#' @export
del_col <- function(table, col)
{
  sapply(sort(col, decreasing = TRUE), function(col) {
    sapply(1:length(table), function(row) {
      cols <- length(table[[row]])
      if(col < cols) sapply((col+1):cols, function(i) table[[row]][[i-1]] <<- table[[row]][[i]])
      table[[row]][[cols]] <<- NULL
    })
  })
  table
}

#' Delete a row(s) from a table
#'
#' Given a table, remove the specified row
#' @param table the table to modify
#' @param row vector with row numbers to drop
#' @return the modified table
#' @export
del_row <- function(table, row)
{
  sapply(sort(row, decreasing = TRUE), function(row) {
    rows <- length(table)
    if(row < rows) sapply((row+1):rows, function(i) table[[i-1]] <<- table[[i]])
    table[[rows]] <<- NULL
  })
  table
}

#' Select given column(s) from a table
#'
#' Given a table, select the specified column(s)
#' @param table the table to modify
#' @param col vector containing column(s) to select
#' @return the modified table
#' @export
select_col <- function(table, col)
{
  if(length(table) == 0) return(table)

  icol <- 1:length(table[[1]])
  icol <- icol[!icol %in% col]

  del_col(table, icol)
}

#' Select given row(s) from a table
#'
#' Given a table, select the specified rows
#' @param table the table to modify
#' @param row vector with row numbers to select
#' @return the modified table
#' @export
select_row <- function(table, row)
{
  if(length(table) == 0) return(table)

  irow <- 1:length(table)
  irow <- irow[!irow %in% row]

  del_row(table, irow)
}

#' Insert a row into a tangram table
#'
#' Insert a row into a tangram table. Will fill with empty cells is not enough cells are specified.
#'
#' @param table the table to modify
#' @param after numeric; The row to position the new row after. Can be zero for inserting a new first row.
#' @param ... Table cells to insert. Cannot be larger than existing table.
#' @param class character; Classes to apply as directives to renderers
#' @return the modified table
#' @export
insert_row <- function(table, after, ..., class=NULL)
{
  # Get the cells from ..., and make sure they are cells
  cells <- lapply(list(...), FUN=function(x) if("cell" %in% class(x)) x else cell(x, class=class) )
  N     <- length(table)

  # Check for mismatch in arguments
  if(length(cells) > length(table[[1]])) stop("tangram::insert_row() number of cells provided larger than current number of columns")
  if(after > N) stop("tangram::insert_row() after parameter larger than number of rows")
  if(after < 0) stop("tangram::insert_row() negative after row")

  # Make room
  if(after < N) for(i in N:(after+1)) table[[i+1]] <- table[[i]]

  # Fill in blanks
  N <- length(table[[1]])
  if(length(cells) < N) for(i in (length(cells)+1):N) cells[[i]] <- cell_label("")

  # Put in the row
  table[[after+1]] <- cells

  table
}

#' Insert a column into a tangram table
#'
#' Insert a column into a tangram table. Will fill with empty cells is not enough cells are specified.
#'
#' @param table the table to modify
#' @param after numeric; The column to position the new row after. Can be zero for inserting a new first row.
#' @param ... Table cells to insert. Cannot be larger than existing table.
#' @param class character; Classes to apply as directives to renderers
#' @return the modified table
#' @export
insert_column <- function(table, after, ..., class=NULL)
{
  # Get the cells from ..., and make sure they are cells
  cells <- lapply(list(...), FUN=function(x) if("cell" %in% class(x)) x else cell(x, class=class) )
  nrows <- length(table)
  ncols <- length(table[[1]])

  # Check for mismatch in arguments
  if(length(cells) > nrows) stop("tangram::insert_column() number of cells provided larger than current number of rows")
  if(after > nrows) stop("tangram::insert_column() after parameter larger than number of rows")
  if(after < 0) stop("tangram::insert_column() negative after column")

  # Make room
  for(i in 1:nrows)
  {
    if(after < ncols) for(j in ncols:(after+1)) table[[i]][[j+1]] <- table[[i]][[j]]

    # Fill in blanks, just in case
    table[[i]][[after+1]] <- cell_label("")
  }

  # Put in the column
  for(i in 1:length(cells)) table[[i]][[after+1]] <- cells[[i]]

  table
}

#' Replace a cell's contents
#'
#' Replace a cell in a table
#'
#' @param table the tangram table to modify
#' @param row numeric; The row to modify
#' @param col numeric; The column to modify
#' @param object The cell or object to replace in a table
#' @param ... Additional parameters passed to cell function if not given a cell object
#' @return the modified table
#' @export
replace_cell <- function(table, row, col, object, ...)
{
  table[[row]][[col]] <- if(inherits(object, "cell")) object else cell(object, ...)

  table
}

#' Add a footnote to a table
#'
#' Add a footnote to a table
#'
#' @param table tangram; the tangram table to modify
#' @param footnote character; The footnote to add
#' @return the modified table
#' @export
add_footnote <- function(table, footnote)
{
  footnotes <- attr(table, "footnote")

  attr(table, "footnote") <- if(is.null(footnotes)) footnotes else paste(footnotes, footnote)

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
  del_col(del_row(table, 2), 4)
}

#' Add indentations to left column row headers
#'
#' Add indentations to left column row headers. Note: will only work on cell_header cells.
#'
#' @param table Output of tangram::tangram()
#' @param rows numeric; A vector of numeric row numbers for the rows that need to be indented. Defaults to NULL which indents all.
#' @param amounts numeric; Specifies number of spaces to add. A vector that is either a single value or vector of the same size as the height of the table. If positions is specified then it must be the same length. Defaults to 2, which each pair of spaces converts naturally in rendering to HTML, LaTeX, etc..
#' @param columns numeric; Column to apply indent to, defaults to 1
#' @return the modified table
#' @export
#' @examples
#' x <- tangram(drug ~ bili + albumin, pbc)
#' add_indent(x)
#' add_indent(x, amounts=10)
#' add_indent(x, amounts=c(0, 0, 2, 4))
#' add_indent(x, rows=c(3))
#' add_indent(x, rows=c(3, 4), amounts=c(4, 2))
add_indent <- function(table, amounts=2, rows=NULL, columns=NULL)
{
  if(!is.null(rows) && length(amounts) > 1 && length(rows) != length(amounts)) stop("tangram::add_indent rows length must match amounts length")
  if(is.null(rows)) rows <- 1:length(table) # Defaults to all rows
  if(is.null(columns)) columns <- 1 # Defaults to first column

  if(length(amounts) == 1) amounts <- rep(amounts, length(rows))
  for(i in 1:length(rows))
  {
    row     <- rows[i]
    amount  <- amounts[i]

    for(column in columns)
    {
      if(nchar(table[[row]][[column]]) > 0)
      {
        x <- paste0(paste(rep(" ", amount), collapse=""), table[[row]][[column]])
        class(x) <- class(table[[row]][[column]])
        attributes(x) <- attributes(table[[row]][[column]])

        table[[row]][[column]] <- x
      }
    }
  }

  table
}
