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
#' @include S3-Cell.R
#' @export
derive_label <- function(node)
{
  l <- node$name()
  units <- NA
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
            l     <- u2[1,2]
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

#' Create a new header on a table
#'
#' Function to append a header object to a given attribute. Will create
#' a new header if one doesn't exit, or append to existing
#'
#' @param table_builder The table builder object to modify
#' @param attribute The header attribute name, i.e. row_header or col_header
#' @param sub boolean indicating if this is a subheader
#' @param ... All the header elements to add
#' @return the modified table_builder
#'
new_header <- function(table_builder, attribute, sub, ...)
{
  # Grab old header if it exists
  old_hdr   <- attr(table_builder$table, attribute)

  # Either a header or subheader
  hdr_class <- if (is.null(old_hdr) | !sub) "cell_header" else c("cell_subheader", "cell_header")

  # Convert every element to an appropriate cell from request
  new_hdr   <- lapply(args_flatten(...), FUN=function(cell) {
    value <- tg(cell, table_builder$row, table_builder$col)
    attr(value, "class") <- c(hdr_class, attr(value,"class"))
    value
  })

  # If the old header is null, then create one
  attr(table_builder$table, attribute) <- if(is.null(old_hdr))
  {
    header <- list(new_hdr)
    attr(header, "class")    <- c("cell_table", "cell")
    attr(header, "embedded") <- FALSE
    header
  } else { # extend existing
    old_hdr[[length(old_hdr)+1]] <- new_hdr
    old_hdr
  }

  # Return table_builder for pipe operator
  table_builder
}

#' Create a new column header in a table
#'
#' Function to append a column header to a table being built. The first call creates
#' a column header, subsequent calls add sub headers to existing column header
#'
#' @param table_builder The table builder object to modify
#' @param ... All the column header elements to add
#' @param sub treat as subheader if after first header, defaults to TRUE
#' @return the modified table_builder
#' @export
#'
col_header <- function(table_builder, ..., sub=TRUE) new_header(table_builder, "col_header", sub, ...)

#' Create a new row header in a table.
#'
#' Function to append a row header to a table being built. The first call creates
#' a row header, subsequent calls add sub headers to existing row header
#'
#' @param table_builder The table builder object to modify
#' @param ... All the row header elements to add
#' @param sub treat as subheader if after first, default to TRUE
#' @return the modified table_builder
#' @export
#'
row_header <- function(table_builder, ..., sub=TRUE) new_header(table_builder, "row_header", sub, ...)

  #############################################################################
 ##
## Table cursor, movement and manipulation. Loosely based on VT100


#' Create empty table builder.
#'
#' Function to create a new table builder to use in continuations.
#' This maintains a cursor state where values are being written to the
#' table under construction, as well as references to the row and column
#' for automated tracability when generating indexes.
#'
#' @param row The row node from the AST
#' @param column The col node from the AST
#' @return a table builder with 1 empty cell at position (1,1)
#' @export
#'
#' @examples
#' x <- Parser$new()$run(y ~ x)
#' new_table_builder(x$right, x$left)
#'
new_table_builder <- function(row, column)
{
  list(nrow=1, ncol=1, table=cell_table(1,1), row=row, col=column)
}

#' Write a single cell
#'
#' Function to write a value to the current position in the table builder
#'
#' @param table_builder The table builder to work on
#' @param x the cell to write
#' @param ... additional attributes to pass for traceback
#' @return a table builder with the given cell written in the current cursor position
#' @export
#'
#' @examples
#' library(magrittr)
#' x <- Parser$new()$run(y ~ x)
#' new_table_builder(x$right, x$left) %>% write_cell(tg_N(23))
#'
write_cell <- function(table_builder, x, ...)
{
  if(table_builder$nrow > length(table_builder$table))
  {
    table_builder$table[[table_builder$nrow]] <- list()
  }
  table_builder$table[[table_builder$nrow]][[table_builder$ncol]] <- tg(x, table_builder$row, table_builder$col, ...)
  table_builder
}

#' Home the cursor.
#'
#' Return table builder cursor position to (1,1)
#'
#' @param table_builder The table builder to work on
#' @return a table builder with the cursor at home
#' @export
#'
#' @examples
#' library(magrittr)
#' x <- Parser$new()$run(y ~ x)
#' new_table_builder(x$right, x$left) %>% home()
#'
home <- function(table_builder)
{
  table_builder$ncol <- 1
  table_builder$nrow <- 1
  table_builder
}

#' Move cursor up
#'
#' Move table builder cursor up specified value (default 1)
#'
#' @param table_builder The table builder to work on
#' @param n units to move cursor up
#' @return a table builder with the cursor up n positions
#' @export
#'
#' @examples
#' library(magrittr)
#' x <- Parser$new()$run(y ~ x)
#' new_table_builder(x$right, x$left) %>% cursor_pos(3,3) %>% cursor_up(2)
#'
cursor_up <- function(table_builder, n=1)
{
  table_builder$nrow <- table_builder$nrow - n
  if(table_builder$nrow <= 0) stop("cursor_up beyond available cells")
  table_builder
}

#' Move cursor down
#'
#' Move table builder cursor down specified value (default 1)
#'
#' @param table_builder The table builder to work on
#' @param n units to move cursor down
#' @return a table builder with the cursor down n positions
#' @export
#'
#' @examples
#' library(magrittr)
#' x <- Parser$new()$run(y ~ x)
#' new_table_builder(x$right, x$left) %>% cursor_pos(3,3) %>% cursor_down(2)
#'
cursor_down <- function(table_builder, n=1)
{
  table_builder$nrow <- table_builder$nrow + n
  if(table_builder$nrow <= 0) stop("cursor_down beyond available cells")
  table_builder
}

#' Move cursor left
#'
#' Move table builder cursor left the specified value (default 1)
#'
#' @param table_builder The table builder to work on
#' @param n units to move cursor left
#' @return a table builder with the cursor left n positions
#' @export
#'
#' @examples
#' library(magrittr)
#' x <- Parser$new()$run(y ~ x)
#' new_table_builder(x$right, x$left) %>% cursor_pos(3,3) %>% cursor_left(2)
#'
cursor_left <- function(table_builder, n=1)
{
  table_builder$ncol <- table_builder$ncol - n
  if(table_builder$ncol <= 0) stop("cursor_left beyond available cells")
  table_builder
}

#' Move cursor right
#'
#' Move table builder cursor right the specified value (default 1)
#'
#' @param table_builder The table builder to work on
#' @param n units to move cursor right
#' @return a table builder with the cursor right n positions
#' @export
#'
#' @examples
#' library(magrittr)
#' x <- Parser$new()$run(y ~ x)
#' new_table_builder(x$right, x$left) %>% cursor_pos(3,3) %>% cursor_right(2)
#'
cursor_right <- function(table_builder, n=1)
{
  table_builder$ncol <- table_builder$ncol + n
  if(table_builder$ncol <= 0) stop("cursor_right beyond available cells")
  table_builder
}

#' Move cursor to position
#'
#' Move table builder cursor to the specified position
#'
#' @param table_builder The table builder to work on
#' @param nrow The number of the row to move too
#' @param ncol The number of the col to move too
#' @return a table builder with the cursor at the specified position
#' @export
#'
#' @examples
#' library(magrittr)
#' x <- Parser$new()$run(y ~ x)
#' new_table_builder(x$right, x$left) %>% cursor_pos(3,3)
#'
cursor_pos <- function(table_builder, nrow, ncol)
{
  if(nrow <= 0 || ncol <= 0) stop("cursor_pos does not allow negative values")
  table_builder$ncol <- ncol
  table_builder$nrow <- nrow
  table_builder
}

#' Move cursor to first column
#'
#' Move table builder cursor to the first column, does not advance row
#'
#' @param table_builder The table builder to work on
#' @return a table builder with the cursor at the first column
#' @export
#'
#' @examples
#' library(magrittr)
#' x <- Parser$new()$run(y ~ x)
#' new_table_builder(x$right, x$left) %>% cursor_pos(3,3) %>% carriage_return()
#'
carriage_return <- function(table_builder)
{
  table_builder$ncol <- 1
  table_builder
}

#' Move cursor to next line
#' Move table builder cursor to the next line (does not alter column)
#'
#' @param table_builder The table builder to work on
#' @param n optional number of line_feeds to perform
#' @return a table builder with the cursor at the first column
#' @export
#'
#' @examples
#' library(magrittr)
#' x <- Parser$new()$run(y ~ x)
#' new_table_builder(x$right, x$left) %>% cursor_pos(3,3) %>% line_feed()
#'
line_feed <- cursor_down

#' Return to 1st column, next line
#'
#' Return table_builder to 1st column, and advance to next line
#'
#' @param table_builder The table builder to work on
#' @return a table builder with the cursor at the first column on a new line
#' @export
#'
#' @examples
#' library(magrittr)
#' x <- Parser$new()$run(y ~ x)
#' new_table_builder(x$right, x$left) %>% new_line()
#'
new_line <- function(table_builder)
{
  table_builder     %>%
  carriage_return() %>%
  line_feed()
}

#' Open a new row
#'
#' Move table builder cursor to the bottom of all defined rows opening a new one
#' in the first column
#'
#' @param table_builder The table builder to work on
#' @return a table builder with the cursor at the first column
#' @export
#'
#' @examples
#' library(magrittr)
#' x <- Parser$new()$run(y ~ x)
#' new_table_builder(x$right, x$left) %>% new_row()
#'
new_row <- function(table_builder)
{
  table_builder %>%
  home()        %>%
  cursor_down(length(table_builder$table))
}

#' Open a new column in 1st row
#'
#' Advance table builder cursor to the furthest right column on the top row and open a new column
#'
#' @param table_builder The table builder to work on
#' @return a table builder with the cursor at the first column
#' @export
#'
#' @examples
#' library(magrittr)
#' x <- Parser$new()$run(y ~ x)
#' new_table_builder(x$right, x$left) %>% new_col()
#'
new_col <- function(table_builder)
{
  table_builder %>%
  home()        %>%
  cursor_right(length(table_builder$table[[1]]) )
}

#' Apply table building over variable
#'
#' Run a continuation function over a list of items.
#' Similar to a foldl in ML
#'
#' @param table_builder The table builder to work on
#' @param X list or vector of items to iterate
#' @param FUN the function to iterate over
#' @param ... additional arguments to pass to FUN
#' @return a table builder with the cursor at the last position of the apply
#' @export
#'
#' @examples
#' library(magrittr)
#' x <- Parser$new()$run(y ~ x)
#' new_table_builder(x$right, x$left) %>%
#' table_builder_apply(1:3, FUN=function(tb, x) {
#'   tb %>% write_cell(tg_N(x)) %>% cursor_right()
#' })
#'
table_builder_apply <- function(table_builder, X, FUN, ...)
{
  sapply(X, FUN=function(x) {
    table_builder <<- FUN(table_builder, x, ...)
  })
  table_builder
}

#' Add columns
#'
#' Add all elements specified and advance to the next column after each addition
#'
#' @param table_builder The table builder to work on
#' @param subrow optional additional specifier for sub element of AST row for traceabililty
#' @param subcol optional additional specifier for sub element of AST col for traceabililty
#' @param ... elements to add columnwise
#' @return a table builder with the cursor at the column past the last addition
#' @export
#'
#' @examples
#' library(magrittr)
#' x <- Parser$new()$run(y ~ x)
#' new_table_builder(x$right, x$left) %>%
#' add_col(tg_N(1:3))
add_col <- function(table_builder, ..., subrow=NA, subcol=NA)
{
  table_builder %>%
  table_builder_apply(args_flatten(...), FUN=function(tbl, object) {
    tbl %>%
    write_cell(object, subrow=subrow, subcol=subcol) %>%
    cursor_right()
  })
}

#' Add rows
#'
#' Add all elements specified and advance to the next row after each addition
#'
#' @param table_builder The table builder to work on
#' @param subrow optional additional specifier for sub element of AST row for traceabililty
#' @param subcol optional additional specifier for sub element of AST col for traceabililty
#' @param ... elements to add rowwise
#' @return a table builder with the cursor at the row past the last addition
#' @export
#'
#' @examples
#' library(magrittr)
#' x <- Parser$new()$run(y ~ x)
#' new_table_builder(x$right, x$left) %>%
#' add_row(tg_N(1:3))
add_row <- function(table_builder, ..., subrow=NA, subcol=NA)
{
  # Get flattened args list
  table_builder %>%
  table_builder_apply(args_flatten(...), FUN=function(tbl, object) {
    tbl %>%
    write_cell(object, subrow=subrow, subcol=subcol) %>%
    cursor_down()
  })
}

  #############################################################################
 ##
## Table Cell generation functions

#' Convert to Cell
#'
#' Base S3 function to allow for constructing table cells. The main purpose
#' of this layer is to append additional traceability information, and convert
#' to internal cell type for later rendering decisions.
#'
#' @param x element to convert
#' @param row The AST row of that is generating this cell
#' @param column The AST column that is generating this cell
#' @param ... additional specifiers for identifying this cell (see key)
#' @return an S3 rendereable cell
#' @export
#'
#' @examples
#' tg(NA, list(value="A"), list(value="B"))
tg <- function(x, row, column, ...)
{
  UseMethod("tg", x)
}

#' Default cell is a label
#'
#' Construct a cell. This is the default fallback, it creates a label cell
#'
#' @param x Object to turn into a renderable label cell
#' @param row The AST row of that is generating this cell
#' @param column The AST column that is generating this cell
#' @param ... additional specifiers for identifying this cell (see key)
#' @return an S3 rendereable cell label
#' @export
#' @examples
#' tg("Joe", list(value="A"), list(value="B"))
tg.default <- function(x, row, column, ...)
{
  cell_label(as.character(x))
}

#' Numeric Cell passed in
#'
#' Construct a cell from a numeric. This is essentially an identity function.
#' I.e., if a user has constructed a cell and passed it in do nothing.
#'
#' @param x The numeric value to return as a renderable cell
#' @param row The AST row of that is generating this cell
#' @param column The AST column that is generating this cell
#' @param ... additional specifiers for identifying this cell (see key)
#' @return an S3 rendereable cell
#' @export
#' @examples
#' tg(23.0, list(value="A"), list(value="B"))
tg.numeric <- function(x, row, column, ...)
{
  if(is.null(names(x)))
    cell_label(as.character(x), src=key(row, column, ...))
  else
    cell_label(paste(names(x),"=",as.character(x),sep=''), src=key(row, column, ...))
}

#' Identity function on cell
#'
#' Construct a cell from a cell. This is essentially an identity function.
#' I.e., if a user has constructed a cell and passed it in do nothing.
#'
#' @param x the object to return
#' @param row The AST row of that is generating this cell
#' @param column The AST column that is generating this cell
#' @param ... additional specifiers for identifying this cell (see key)
#' @return an S3 rendereable cell label
#' @export
#' @examples
#' tg(tg("Joe"), list(value="A"), list(value="B"))
tg.cell <- function(x, row, column, ...)
{
  x
}

#' N value as cell
#'
#' Construct a cell from an N value.
#'
#' @param x The tg_N object to convert into a rendereable cell
#' @param row The AST row of that is generating this cell
#' @param column The AST column that is generating this cell
#' @param ... additional specifiers for identifying this cell (see key)
#' @return an S3 rendereable cell label
#' @export
#' @examples
#' tg(tg_N("Joe"), list(value="A"), list(value="B"))
tg.N <- function(x, row, column, ...)
{
  cell_n(x, src=key(row, column, "N", ...))
}

#' AOV model as cell
#'
#' Construct a cell from an analysis of variance model
#'
#' @param x The aov object to turn into a renderable cell
#' @param row The AST row of that is generating this cell
#' @param column The AST column that is generating this cell
#' @param ... additional specifiers for identifying this cell (see key)
#' @return an S3 rendereable cell that is an F-statistic
#' @export
#' @examples
#' tg(aov(rnorm(10) ~ rnorm(10)), list(value="A"), list(value="B"))
tg.aov <- function(x, row, column, ...)
{
  test <- summary(x)[[1]]
  cell_fstat(f   = form(test$'F value'[1], "%.2f"),
             n1  = test$Df[1],
             n2  = test$Df[2],
             p   = form(test$'Pr(>F)'[1], "%1.3f"),
             reference = "1",
             src = key(row, column, "aov", ...))
}

#' Construct hypothesis test form cell
#'
#' Construct a cell from a hypothesis test
#'
#' @param x The htest object to convert to a rendereable cell
#' @param row The AST row of that is generating this cell
#' @param column The AST column that is generating this cell
#' @param ... additional specifiers for identifying this cell (see key)
#' @return an S3 rendereable cell that is a hypothesis test
#' @export
#' @examples
#' tg(t.test(rnorm(10),rnorm(10)), list(value="A"), list(value="B"))
tg.htest <- function(x, row, column, ...)
{
  ss <- key(row, column, "htest", ...)
  if(names(x$statistic) == "X-squared")
    cell_chi2(form(x$statistic, 2), x$parameter[1], form(x$p.value, "%1.3f"), reference="2", src=ss)
  else if(x$method == "Spearman's rank correlation rho")
    cell_spearman(form(x$statistic, 0), x$parameter, form(x$p.value, "%1.3f"), reference="3", src=ss)
  else
    cell_studentt(form(x$statistic, 2), x$parameter[1], form(x$p.value, "%1.3f"), reference="4", src=ss)
}

#' Construct a cell from a tg_quantile
#'
#' @param x The quantile object to turn in to a rendereable cell
#' @param row The AST row of that is generating this cell
#' @param column The AST column that is generating this cell
#' @param ... additional specifiers for identifying this cell (see key)
#' @return an S3 rendereable cell that is a hypothesis test
#' @export
#' @examples
#' tg(tg_quantile(rnorm(10), "%.f"), list(value="A"), list(value="B"))
tg.quantile <- function(x, row, column, ...)
{
  cell_quantile(x,
                src    = key(row    = row,
                             col    = column,
                             label  = "quantile",
                             ...)
               )
}

#' Cell Fraction Conversion
#'
#' Construct a cell from a tg_fraction
#'
#' @param x The fraction object to convert to a tg cell
#' @param row The AST row of that is generating this cell
#' @param column The AST column that is generating this cell
#' @param ... additional specifiers for identifying this cell (see key)
#' @return an S3 rendereable cell that is a hypothesis test
#' @export
#' @examples
#' tg(tg_fraction(1, 2, 3), list(value="A"), list(value="B"))
tg.fraction <- function(x, row, column, ...)
{
  format <- attr(x, "format")
  # Make an intelligent default format based on the data
  if(is.na(format)) format <- 3
  pformat <- if(is.numeric(format)) max(format-2,0) else format

  list_cell("cell_fraction",
            numerator=x[1],
            denominator=x[2],
            ratio=form(x[3],format),
            percentage=form(x[4],pformat),
            src=key(row    = row,
                    col    = column,
                    label  = "fraction",
                    ...))
}

#' N values creation
#'
#' Create a vector of N values that are convertable to a cell
#'
#' @param ... the N values
#' @return an S3 rendereable cell that is a hypothesis test
#' @export
#' @examples
#' tg_N(1, 2, 3)
tg_N <- function(...)
{
  v <- c(...)
  class(v) <- c("N", "numeric")
  v
}

#' Fraction values creation
#'
#' Create a fraction that is convertible to a cell
#'
#' @param numerator The numerator of the fraction
#' @param denominator The denominator of the fraction
#' @param format Formating option for ratio / percentage
#' @return an S3 rendereable cell that is a fraction
#' @export
#' @examples
#' tg_fraction(1, 2, 3)
tg_fraction <- function(numerator, denominator, format=3)
{
  ratio <- numerator / denominator
  structure(c(numerator=numerator,
              denominator=denominator,
              ratio=ratio,
              percentage=100*ratio
            ),
            class=c("fraction", "numeric"),
            format=format
           )
}

#' Quantile creation
#'
#' Create a quantile that is convertible to a cell
#'
#' @param x the data passed to the quantile {stats} function
#' @param format the formatting to be applied (usually comes from AST node)
#' @param ... all arguments that are passed to the quantile {stats} function
#' @return an S3 rendereable cell that is a hypothesis test
#' @export
#' @importFrom stats quantile
#' @examples
#' tg_quantile(rnorm(100), "%.2f")
tg_quantile <- function(x, format=NA, ...)
{
  x <- quantile(x, ...)
  class(x) <- c("quantile", "numeric")

  # Make an intelligent default format based on the data
  if(is.na(format)) format <- format_guess(x)

  attr(x, "format") <- format

  x
}

  #############################################################################
 ##
## Set of misc helper functions for dealing with data / tables

#' Guess the best format for a set of data
#'
#' Given a vector of data, default to 3 significant digits or all if maximum is greater
#' than zero
#'
#' @param x the data passed to the quantile {stats} function
#' @return the significant digits to preserve
#' @export
#' @examples
#' format_guess(rnorm(100))
format_guess <- function(x)
{
  d <- x[!is.na(x)]
  if(length(d) == 0) return(0) # Nothing, then just return 0 for rounding
  if(all(d == floor(d)))       # Is it all whole numbers, then no decimals
    return(0)
  else
    # Otherwise use 3 significant digits of a representative smaller side quantile
    return(max(2-max(floor(log10(quantile(abs(d), c(0.05, 0.5))))), 0))
}

#' Attach format attribute to object
#'
#' Attach formatting information to an object in the attr "format"
#'
#' @param object the object to attach format information
#' @param value the formatting to be applied (usually comes from AST node)
#' @return an S3 object with format attribute set
#' @export
#' @examples
#' form(2, "%.2f")
form <- function(object, value)
{
  attr(object, "format") <- value
  object
}
