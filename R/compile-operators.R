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

  #############################################################################
 ##
## Set of functions to use in building a table, cell by cell

#' Derive label of AST node.
#'
#' Determine the label of a given AST node.
#' NOTE: Should have data attached via reduce before calling.
#'
#' @param node Abstract syntax tree node.
#' @param capture_units logical; Capture units from parenthesis ending a label
#' @param ... Other arguments, ignored
#'
#' @return A string with a label for the node
#' @include compile-cell.R
#' @export
derive_label <- function(node, capture_units=FALSE, ...)
{
  l     <- node$name() # Default Label
  units <- NULL        # Default Units

  u2 <- NULL
  try({u2 <- attr(node$data, "units")})
  l2 <- NULL
  try({l2 <- attr(node$data, "label")})

  if(!is.null(u2))
  {
    units <- u2
  }

  if(!is.null(l2))
  {
    if(capture_units && is.null(units)) # Capture and not already specified
    {
      # Assumes units are in parenthesis at end of label
      u2 <- str_match(l2, "(.*)\\((.*)\\)$")
      if(!is.na(u2[1,1]))
      {
        l     <- trimws(u2[1,2])
        units <- u2[1,3]
      } else
      {
        l <- l2
      }
    } else
    {
      l <- l2
    }
  }

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

new_header <- function(table, attribute, sub, ...)
{
  # Grab old header if it exists
  old_hdr   <- attr(table, attribute)

  # Either a header or subheader
  hdr <- if (is.null(old_hdr) | !sub) cell_header else cell_subheader

  # Convert every element to an appropriate cell from request
  new_hdr   <- lapply(args_flatten(...), FUN=function(x) hdr(x))
# FIXME: Is row/col reference still needed?
#        row=table_builder$row,
#        col=table_builder$col)

  # If the old header is null, then create one
  attr(table, attribute) <- if(is.null(old_hdr))
  {
    hdr      <- tangram(1,1)
    hdr[[1]] <- new_hdr
    hdr
  } else { # extend existing
    old_hdr[[length(old_hdr)+1]] <- new_hdr
    old_hdr
  }

  # Return table for possible magrittr use
  table
}

#' A set of magrittr operators for tangram tables
#'
#' A set of magrittr operators for tangram tables
#'
#' @rdname table_builder
#' @param caption character; caption of table
#' @param id character; id of table
#' @param footnote character; footnote to add
#' @param FUN function; function to apply
#' @param n numeric; number of times to perform operation
#' @param ncol numeric; number of columns
#' @param nrow numeric; number of rows
#' @param style character; styling in compiling table and in rendering
#' @param span numeric; number of rows or columns to span
#' @param sub logical; Is this a subheader
#' @param table tangram; The tangram table being built
#' @param x object of focus in operation
#' @param ... additional argument passed
#' @export
col_header <- function(table, ..., sub=TRUE) new_header(table, "col_header", sub, ...)

#' @rdname table_builder
#' @export
row_header <- function(table, ..., sub=TRUE) new_header(table, "row_header", sub, ...)

#' @rdname table_builder
#' @export
write_cell <- function(table, x, ...)
{
  if(attr(table, "row") > length(table)) table[[attr(table, "row")]] <- list()

# FIXME IS ROW / COL REFERENCE STILL NEEDED?
#  table$table[[attr(table, "row")]][[attr(table, "col")]] <- cell(x, row=table$row, col=table$col, ...)
  table[[attr(table, "row")]][[attr(table, "col")]] <- cell(x, ...)
  table
}

#' @rdname table_builder
#' @export
home <- function(table)
{
  attr(table,"col") <- 1
  attr(table,"row") <- 1
  table
}

#' @rdname table_builder
#' @export
cursor_up <- function(table, n=1)
{
  attr(table,"row") <- attr(table,"row") - n
  if(attr(table,"row") <= 0) stop("cursor_up beyond available cells")
  table
}

#' @rdname table_builder
#' @export
cursor_down <- function(table, n=1)
{
  attr(table,"row") <- attr(table,"row") + n
  if(attr(table,"row") <= 0) stop("cursor_down beyond available cells")
  table
}

#' @rdname table_builder
#' @export
cursor_left <- function(table, n=1)
{
  attr(table,"col") <- attr(table,"col") - n
  if(attr(table,"col") <= 0) stop("cursor_left beyond available cells")
  table
}

#' @rdname table_builder
#' @export
cursor_right <- function(table, n=1)
{
  attr(table,"col") <- attr(table,"col") + n
  if(attr(table,"col") <= 0) stop("cursor_right beyond available cells")
  table
}

#' @rdname table_builder
#' @export
cursor_pos <- function(table, nrow, ncol)
{
  if(nrow <= 0 || ncol <= 0) stop("cursor_pos does not allow negative values")
  attr(table,"col") <- ncol
  attr(table,"row") <- nrow
  table
}

#' @rdname table_builder
#' @export
carriage_return <- function(table)
{
  attr(table,"col") <- 1
  table
}

#' @rdname table_builder
#' @export
line_feed <- cursor_down

#' @rdname table_builder
#' @export
new_line <- function(table)
{
  table     %>%
  carriage_return() %>%
  line_feed()
}

#' @rdname table_builder
#' @export
new_row <- function(table)
{
  table %>%
  home()        %>%
  cursor_down(length(table))
}

#' @rdname table_builder
#' @export
new_col <- function(table)
{
  table %>% home() %>% cursor_right(length(table[[1]]))
}

#' @rdname table_builder
#' @export
table_apply <- function(table, x, FUN, ...)
{
  sapply(x, FUN=function(y) table <<- FUN(table, y, ...))
  table
}

#' @rdname table_builder
#' @export
add_col <- function(table, ...)
{
  table %>%
  table_apply(args_flatten(...), FUN=function(tbl, object) {
    tbl %>%
    write_cell(object) %>%
    cursor_right()
  })
}

#' @rdname table_builder
#' @export
add_row <- function(table, ...)
{
  # Get flattened args list
  table %>%
  table_apply(args_flatten(...), FUN=function(tbl, object) {
    tbl %>%
    write_cell(object) %>%
    cursor_down()
  })
}

#' @rdname table_builder
#' @export
set_footnote <- function(table, footnote)
{
  attr(table, "footnote") <- footnote

  table
}

#' @rdname table_builder
#' @export
set_id <- function(table, id)
{
  attr(table, "id") <- id

  table
}

#' @rdname table_builder
#' @export
set_caption <- function(table, caption)
{
  attr(table, "caption") <- caption

  table
}

#' @rdname table_builder
#' @export
set_style <- function(table, style)
{
  attr(table, "style") <- style

  table
}

#' @rdname table_builder
#' @export
set_colspan <- function(table, span)
{
  attr(table[[attr(table,"row")]][[attr(table,"col")]], "colspan") <- span

  table
}

#' @rdname table_builder
#' @export
set_rowspan <- function(table, span)
{
  attr(table[[attr(table,"row")]][[attr(table,"col")]], "rowspan") <- span

  table
}

#' A cbind for generated table tangram objects.
#'
#' Execute the equivalent of an cbind for generated tables
#'
#' @param ... tangram objects to cbind
#' @param deparse.level numeric; not used
#' @return A merged tangram object
#' @export
cbind.tangram <- function(..., deparse.level=1)
{
  elements <- list(...)

  x <- elements[[1]]

  for(i in 2:length(elements))
  {
    z <- elements[[i]]
    len <- length(z)

    if(len != length(x)) warning("Mismatched row size in cbind.tangram")

    for(j in 1:len)
    {
      x[[j]] <- c(x[[j]], z[[j]])
    }
  }

  x
}

#' Provide a "|" operator for cbind of tangram tables
#'
#' The pipe operator provides an cbind for tangram tables
#'
#' @name pipe.tangram
#' @param x left argument for rbind
#' @param y right argument for rbind
#' @rdname pipe.tangram
#' @return A column wise merged tangram object
#' @export
"|.tangram" <- function(x, y) cbind(x,y)


#' An rbind for generated tables tangram objects.
#'
#' Execute the equivalent of an rbind for generated tables
#'
#' @param ... tangram objects to rbind
#' @param deparse.level numeric; not used
#' @return A merged tangram object
#' @export
rbind.tangram <- function(..., deparse.level=1)
{
  elements <- list(...)
  x <- NULL
  for(i in 2:length(elements))
  {
    z <- elements[[i]]
    len <- length(z[[1]])
    while("cell_header" %in% class(z[[1]][[len]])) z <- del_row(z, 1)
    x <- c(elements[[i-1]], z)
  }

  class(x) <- c("tangram", "list")
  attr(x, "embedded") <- FALSE
  attr(x, "footnote") <- attr(elements[[1]], "footnote")
  attr(x, "id")       <- attr(elements[[1]], "id")
  attr(x, "caption")  <- attr(elements[[1]], "caption")
  attr(x, "style")    <- attr(elements[[1]], "style")
  attr(x, "args")     <- attr(elements[[1]], "args")
  x
}

#' Provide a "+" operator for rbind of tangram tables
#'
#' The plus operator provides an rbind for tangram tables
#'
#' @param x left argument for rbind
#' @param y right argument for rbind
#' @return A row wise merged tangram object
#' @export
"+.tangram" <- function(x, y) rbind(x,y)
