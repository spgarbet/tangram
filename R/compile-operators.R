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

  #############################################################################
 ##
## Set of functions to use in building a table, cell by cell

#' Derive label of AST node.
#'
#' Determine the label of a given AST node.
#' NOTE: Should have data attached via reduce before calling.
#'
#' @param node Abstract syntax tree node.
#'
#' @return A string with a label for the node
#' @include compile-cell.R
#' @export
derive_label <- function(node)
{
  l <- node$name()
  units <- NULL
  try({
        l2 <- attr(node$data, "label")
        if(!is.null(l2))
        {
          # Since a label was found, see if it has units
          u2 <- str_match(l2, "(.*)\\((.*)\\)")
          if(is.na(u2[1,1]))
          {
            l <- l2
          } else {
            l     <- trimws(u2[1,2])
            units <- u2[1,3]
          }
        }
  })

  # Find units if they exist
  try({
    u2 <- attr(node$data, "units")

    if(!is.null(u2)) {units<-u2}
  })

  cell_label(l, units)
}

  #############################################################################
 ##
## Helper functions for adding headers

#' Flatten variable arguments
#'
#' Take variable arguments, flatten vectors and lists, but do not flatten cells (which are lists)
#' e.g. args_flatten(NA, list(1,2,3), 4:6, c(7,8,9))
#'
#' @param ... variable arguments
#' @return a list of the arguments, with vectors and lists flattened
#'
args_flatten <- function(...)
{
  ls   <- list(...)
  flat <- list()
  el   <- 1

  for(a in ls)
  {
    if("list" %in% class(a) || is.vector(a) || "N" %in% class(a))
    {
      for(b in a)
      {
        flat[[el]] <- b
        if(!"list" %in% class(a))
        {
          class(flat[[el]]) <- class(a)
          names(flat[[el]]) <- names(a)
        }
        el <- el+1
      }
    } else {
      flat[[el]] <- a
      el <- el + 1
    }
  }
  flat
}

new_header <- function(table_builder, attribute, sub, ...)
{
  # Grab old header if it exists
  old_hdr   <- attr(table_builder$table, attribute)

  # Either a header or subheader
  hdr <- if (is.null(old_hdr) | !sub) cell_header else cell_subheader

  # Convert every element to an appropriate cell from request
  new_hdr   <- lapply(args_flatten(...), FUN=function(x) {
    hdr(x,
        row=table_builder$row,
        col=table_builder$col)
  })

  # If the old header is null, then create one
  attr(table_builder$table, attribute) <- if(is.null(old_hdr))
  {
    hdr      <- tangram(1,1,embedded=FALSE)
    hdr[[1]] <- new_hdr
    hdr
  } else { # extend existing
    old_hdr[[length(old_hdr)+1]] <- new_hdr
    old_hdr
  }

  # Return table_builder for pipe operator
  table_builder
}

#' Table Construction Toolset
#'
#' These functions help build a table. A table can be embedded
#' inside another table as a cell as well. The typical transform functions
#' that provide bundles of functionality utilize this approach and each
#' row column pair are rendered as a cell that is a table and later the
#' whole table is flattened.
#'
#' This library is designed to use a core \code{table_builder} object that
#' is passed from function to function using the pipe \code{\%>\%} operator.
#' First create a \code{table_builder} using the \code{table_builder()} function and
#' use the operators to build out the table. The row and column given to
#' the \code{table_builder} are what is used in later construction of an
#' index key. The table_builder object contains an item table which
#' is the current table being built.
#'
#' Column and row headers are attached as attributes to each table
#' constructed are are tables in their own right that should match
#' the proper dimension of the contained table. When later flattening
#' a table of embedded tables, only the left and top most headers are
#' used.
#'
#' The table builder also has a cursor which maintains the state
#' of where cell items are being written in table construction. It
#' is possible to move the cursor into undefined portions of the table.
#' Therefore it is best to use cursor movement to move in defined
#' rows or columns of information.
#'
#' @param column character; Value to use for indexing
#' @param embedded logical; is this to be embedded in another table
#' @param FUN the function to use in iteration
#' @param n integer; Number of positions to move cursor, defaults to 1
#' @param ncol integer; specifies desired col
#' @param nrow integer; specifies desired row
#' @param row character; Value to use for indexing
#' @param sub logical; treat as subheader if after first header, defaults to TRUE
#' @param table_builder The table builder object to modify
#' @param x any; a value to use for a cell in operation
#' @param X list or vector; items to iterate over
#' @param ... object; the elements to add or additional values to pass to FUN
#' @return the modified table_builder
#' @examples
#' library(magrittr)
#' table_builder()                        %>%
#' col_header("One","Two","Three","Four") %>%
#' row_header("A",   "B",   "C")          %>%
#' write_cell("A1")                       %>%
#' cursor_right()                         %>%
#' add_col("A2", "A3")                    %>%
#' home()                                 %>%
#' new_line()                             %>%
#' table_builder_apply(1:3, FUN=function(tb, x) {
#'   tb %>% write_cell(paste0("B",x)) %>% cursor_right()
#' })                                     %>%
#' new_col()                              %>%
#' add_row(paste0(c("A","B","C"), 4))     %>%
#' cursor_up(2)                           %>%
#' line_feed()                            %>%
#' cursor_left(3)                         %>%
#' add_col(paste0("C", 1:4))
#' @rdname table_builder
#' @export
table_builder <- function(row=NA, column=NA, embedded=FALSE)
{
  x <- list(nrow=1, ncol=1, table=tangram(1,1,embedded), row=row, col=column)
  class(x) <- c("table_builder", "list")
  x
}

#' @rdname table_builder
#' @export
col_header <- function(table_builder, ..., sub=TRUE) new_header(table_builder, "col_header", sub, ...)

#' @rdname table_builder
#' @export
row_header <- function(table_builder, ..., sub=TRUE) new_header(table_builder, "row_header", sub, ...)

#' @rdname table_builder
#' @export
write_cell <- function(table_builder, x, ...)
{
  if(table_builder$nrow > length(table_builder$table))
  {
    table_builder$table[[table_builder$nrow]] <- list()
  }
  table_builder$table[[table_builder$nrow]][[table_builder$ncol]] <- cell(x, row=table_builder$row, col=table_builder$col, ...)
  table_builder
}

#' @rdname table_builder
#' @export
home <- function(table_builder)
{
  table_builder$ncol <- 1
  table_builder$nrow <- 1
  table_builder
}

#' @rdname table_builder
#' @export
cursor_up <- function(table_builder, n=1)
{
  table_builder$nrow <- table_builder$nrow - n
  if(table_builder$nrow <= 0) stop("cursor_up beyond available cells")
  table_builder
}

#' @rdname table_builder
#' @export
cursor_down <- function(table_builder, n=1)
{
  table_builder$nrow <- table_builder$nrow + n
  if(table_builder$nrow <= 0) stop("cursor_down beyond available cells")
  table_builder
}

#' @rdname table_builder
#' @export
cursor_left <- function(table_builder, n=1)
{
  table_builder$ncol <- table_builder$ncol - n
  if(table_builder$ncol <= 0) stop("cursor_left beyond available cells")
  table_builder
}

#' @rdname table_builder
#' @export
cursor_right <- function(table_builder, n=1)
{
  table_builder$ncol <- table_builder$ncol + n
  if(table_builder$ncol <= 0) stop("cursor_right beyond available cells")
  table_builder
}

#' @rdname table_builder
#' @export
cursor_pos <- function(table_builder, nrow, ncol)
{
  if(nrow <= 0 || ncol <= 0) stop("cursor_pos does not allow negative values")
  table_builder$ncol <- ncol
  table_builder$nrow <- nrow
  table_builder
}

#' @rdname table_builder
#' @export
carriage_return <- function(table_builder)
{
  table_builder$ncol <- 1
  table_builder
}

#' @rdname table_builder
#' @export
line_feed <- cursor_down

#' @rdname table_builder
#' @export
new_line <- function(table_builder)
{
  table_builder     %>%
  carriage_return() %>%
  line_feed()
}

#' @rdname table_builder
#' @export
new_row <- function(table_builder)
{
  table_builder %>%
  home()        %>%
  cursor_down(length(table_builder$table))
}

#' @rdname table_builder
#' @export
new_col <- function(table_builder)
{
  table_builder %>%
  home()        %>%
  cursor_right(length(table_builder$table[[1]]) )
}

#' @rdname table_builder
#' @export
table_builder_apply <- function(table_builder, X, FUN, ...)
{
  sapply(X, FUN=function(x) {
    table_builder <<- FUN(table_builder, x, ...)
  })
  table_builder
}

#' @rdname table_builder
#' @export
add_col <- function(table_builder, ...)
{
  table_builder %>%
  table_builder_apply(args_flatten(...), FUN=function(tbl, object) {
    tbl %>%
    write_cell(object) %>%
    cursor_right()
  })
}

#' @rdname table_builder
#' @export
add_row <- function(table_builder, ...)
{
  # Get flattened args list
  table_builder %>%
  table_builder_apply(args_flatten(...), FUN=function(tbl, object) {
    tbl %>%
    write_cell(object) %>%
    cursor_down()
  })
}
#'
#' #' @export
#' cbind.tangram <- function(..., deparse.level=1)
#' {
#'
#' }
#'
#' #' @export
#' cbind.table_builder <- function(..., deparse.level=1)
#' {
#'
#' }
#'
#' #' @export
#' rbind.tangram <- function(..., deparse.level=1)
#' {
#'
#' }
#'
#' #' @export
#' rbind.table_builder <- function(..., deparse.level=1)
#' {
#'
#' }

