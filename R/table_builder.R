  #############################################################################
 ##
## Set of functions to use in building a table, cell by cell

#' @import Hmisc
#' @include S3-Cell.R
#'

#' Determine the label of a given AST node.
#' NOTE: Should have data attached via reduce before calling.
#'
#' @param node Abstract syntax tree node.
#'
#' @return A string with a label for the node
#' @export
#'
#' @examples
#'
#' hmisc_data_type(c(1,2,3))
#' hmisc_data_type(factor(c("A","B","C")))
#' hmisc_data_type(factor(c("A","B","B","A")))
#' hmisc_data_type(factor(c(TRUE, FALSE, TRUE, FALSE)))
#'
derive_label <- function(node)
{
  l <- node$string()
  try({
        l2 <- label(node$data, units=FALSE)
        if(nchar(l2)>0) {l<-l2}
  })

  # Find units if they exist
  x <- strsplit(l, "\\s+\\(")[[1]]
  l <- x[1]

  units <- NA
  if(length(x) > 1) units <- strsplit(x[2], "\\)")[1]
  try({
        u2 <- units(node$data)
        if(nchar(u2)>0) {units<-u2}
  })

  cell_label(l, units)
}

  #############################################################################
 ##
## Helper functions for adding headers

#' Special flatten list, that doesn't flatten cells.
#' Helper for dealing with ... arguments.
args_flatten <- function(ls)
{
  flat <- list()
  el   <- 1

  for(a in ls)
  {
    if(inherits(a, "cell") || inherits(a, "tg"))
    {
      flat[[el]] <- a
      el <- el + 1
    } else {
      for(b in a)
      {
        flat[[el]] <- b
        class(flat[[el]]) <- class(a)
        el <- el+1
      }
    }
  }
  flat
}

prepare_header_row <- function(row, col, additional_class, ...)
{
  # Get flattened args list
  flat <- args_flatten(list(...))

  # Convert every element to an appropriate cell
  lapply(flat, FUN=function(cell){
    value <- if(inherits(cell, "cell")) cell              else
             if(is.na(cell))            cell()            else
                                        tg(cell, row, col)
    attr(value, "class") <- c(additional_class, attr(value,"class"))
    value
  })
}

new_header <- function(table_builder, attribute, ...)
{
  old_hdr <- attr(table_builder$table, attribute)

  new_hdr <- prepare_header_row(table_builder$row,
                                table_builder$col,
                                ifelse(is.null(old_hdr),
                                       c("cell_header"),
                                       c("cell_subheader", "cell_header")),
                                ...)

  attr(table_builder$table, attribute) <- if(is.null(old_hdr))
  {
    header <- list(new_hdr)
    attr(header, "class")    <- c("cell_table", "cell")
    attr(header, "embedded") <- FALSE
    header
  } else {
    old_hdr[[length(old_hdr)+1]] <- new_hdr
    old_hdr
  }

  # Return table_builder for pipe operator
  table_builder
}

col_header <- function(table_builder, ...) new_header(table_builder, "col_header", ...)
row_header <- function(table_builder, ...) new_header(table_builder, "row_header", ...)

  #############################################################################
 ##
## Table cursor, movement and manipulation. Loosely based on VT100

new_table_builder <- function(row, column)
{
  list(nrow=1, ncol=1, table=cell_table(1,1), row=row, col=column)
}

write_cell <- function(table_builder, x, ...)
{
  table_builder$table[[table_builder$nrow]][[table_builder$ncol]] <- tg(x, table_builder$row, table_builder$col, ...)
  table_builder
}

home <- function(table_builder)
{
  table_builder$ncol <- 1
  table_builder$nrow <- 1
  table_builder
}

cursor_up <- function(table_builder, n=1)
{
  table_builder$nrow <- table_builder$nrow - n
  if(table_builder$nrow <= 0) stop("cursor_up beyond available cells")
  table_builder
}

cursor_down <- function(table_builder, n=1)
{
  table_builder$nrow <- table_builder$nrow + n
  if(table_builder$nrow <= 0) stop("cursor_down beyond available cells")
  table_builder
}

cursor_left <- function(table_builder, n=1)
{
  table_builder$ncol <- table_builder$ncol - n
  if(table_builder$ncol <= 0) stop("cursor_left beyond available cells")
  table_builder
}

cursor_right <- function(table_builder, n=1)
{
  table_builder$ncol <- table_builder$ncol + n
  if(table_builder$ncol <= 0) stop("cursor_right beyond available cells")
  table_builder
}

cursor_pos <- function(table_builder, nrow, ncol)
{
  if(nrow <= 0 || ncol <= 0) stop("cursor_pos does not allow negative values")
  table_builder$ncol <- ncol
  table_builder$nrow <- nrow
  table_builder
}

#' Goto first column, does not advance to next row
carriage_return <- function(table_builder)
{
  table_builder$ncol <- 1
  table_builder
}

#' Advance down to next line, does not goto first column
line_feed <- cursor_down

#' Return to 1st column, and advance to next line
new_line <- function(table_builder)
{
  table_builder     %>%
  carriage_return() %>%
  line_feed()
}

#' Advance to the bottom of all defined rows, and open a new one
new_row <- function(table_builder)
{
  table_builder %>%
  home()        %>%
  cursor_down(length(table_builder$table))
}

#' Advance to the furthest right column on the top row and open a new column
new_col <- function(table_builder)
{
  table_builder %>%
  home()        %>%
  cursor_right(length(table_builder$table[[1]]) )
}

#' Nest table continuation inside apply
table_builder_apply <- function(table_builder, X, FUN)
{
  sapply(X, FUN=function(x) {
    table_builder <<- FUN(table_builder, x)
  })
  table_builder
}

#' Add an element in the current cell, and advance to next column
add_col <- function(table_builder, label=NA, subrow=NA, subcol=NA, ...)
{
  # Get flattened args list
  flat <- args_flatten(list(...))

  table_builder %>%
  table_builder_apply(flat, FUN=function(tbl, object) {
    tbl %>%
    write_cell(object, label=label, subrow=subrow, subcol=subcol) %>%
    cursor_right()
  })
}

#' Add an element in the current cell, and advance down to the next row
add_row <- function(table_builder, label=NA, subrow=NA, subcol=NA, ...)
{
  # Get flattened args list
  flat <- args_flatten(list(...))

  table_builder %>%
  table_builder_apply(flat, FUN=function(tbl, object) {
    tbl %>%
    write_cell(object, label=label, subrow=subrow, subcol=subcol) %>%
    cursor_down()
  })
}

  #############################################################################
 ##
## Table Cell generation functions

#' @export
tg <- function(x, row, column, ...)
{
  UseMethod("tg", x)
}

tg.default <- function(x, row, column, ...)
{
  cell_label(as.character(x))
}

tg.cell <- function(x, row, column, ...)
{
  x
}

tg.N <- function(n, row, column, ...)
{
  cell_n(n, src=key(row, column, "N", ...))
}

tg.aov <- function(model, row, column, ...)
{
  test <- summary(model)[[1]]
  cell_fstat(f   = cell_format("%.2f", test$'F value'[1]),
           n1  = test$Df[1],
           n2  = test$Df[2],
           p   = cell_format("%1.3f", test$'Pr(>F)'[1]),
           src = key(row, column, "aov", ...))
}

tg.htest <- function(model, row, column, ...)
{
  cell_studentt(round(model$statistic,2), model$df, round(model$p.value, 3),
    src=key(row, column, "ttest", ...))
}

tg.quantile <- function(quantiles, row, column, ...)
{
  cell_quantile(quantiles,
                  src=key(row    = row,
                          col    = column,
                          label  = "quantile",
                          ...))
}

tg.fraction <- function(x, row, column, ...)
{
  cell_fraction(x[1], x[2],
                  src=key(row    = row,
                          col    = column,
                          label  = "quantile",
                          ...))
}

tg_N <- function(...)
{
  v <- c(...)
  class(v) <- c("N", "numeric")
  v
}

tg_fraction <- function(numerator, denominator)
{
  structure(c(numerator, denominator), class=c("fraction", "tg", "numeric"))
}

tg_quantile <- function(x, ...)
{
  result <- quantile(x, ...)
  class(result) <- c("quantile", "tg", "numeric")
  result
}
