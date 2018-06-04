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



#' Given a tangram object with embedded tables, flattens to a single table.
#'
#' Flattening function to expanded embedded tables inside table cells.
#'
#' @param table the table object to flatten
#' @return the flattened table object
#' @export
#' @include compile-cell.R
#' @include parser.R
table_flatten <- function(table)
{
#  if((is.null(attr(table, "embedded"))    || !attr(table, "embedded"))            &&
#     (!is.null(attr(table, "row_header")) || !is.null(attr(table, "col_header")))
#    )
  if(!is.null(attr(table, "row_header")) ||
     !is.null(attr(table, "col_header")) ||
     !"tangram" %in% class(table[[1]][[1]])
  )
  {
    x <- tangram(1, 1, FALSE)
    attr(table, "embedded") <- TRUE
    x[[1]][[1]] <- table
    table <- x
  }

  # Compute final size of table
  final_rows    <- 0
  final_cols    <- 0
  sapply(1:rows(table), FUN=function(row) {
    element <- table[[row]][[1]]
    if(inherits(element, "tangram") && attr(element, "embedded"))
      final_rows <<- final_rows + length(element)
    else
      final_rows <<- final_rows + 1
  })
  sapply(1:cols(table), FUN=function(col){
    element <- table[[1]][[col]]
    if(inherits(element, "tangram") && attr(element, "embedded"))
      final_cols <<- final_cols + length(element[[1]])
    else
      final_cols <<- final_cols + 1
  })

  # Grab labels
  row_label <- attr(table[[1]][[1]], "row_header")
  col_label <- attr(table[[1]][[1]], "col_header")

  # Set aside additional for labeling
  label_rows <- if(is.null(col_label)) 0 else rows(col_label) # How many rows in the column header
  label_cols <- if(is.null(row_label)) 0 else cols(row_label) # How many cols in the row headers
  # Allocate final table
  new_tbl <- tangram(final_rows+label_rows, final_cols+label_cols)

  # Fill in row labels
  if(label_cols > 0)
  {
    output_row <- label_rows + 1
    sapply(1:rows(table), FUN=function(row){
      rlabel <- attr(table[[row]][[1]], "row_header") # Only take row labels from column 1

      if(inherits(rlabel, "tangram")) {
        sapply(1:rows(rlabel), FUN=function(inner_row) {
          sapply(1:cols(rlabel), FUN=function(inner_col) {
            rl <- rlabel[[inner_row]][[inner_col]]
            attr(rl, "parity") <- ifelse(row %% 2==0, "even", "odd")
            new_tbl[[output_row]][[inner_col]] <<- rl
          })
          output_row <<- output_row + 1
        })
      }
      else
      {
        attr(rlabel, "parity") <- ifelse(row %% 2==0,"even", "odd")
        new_tbl[[output_row]][[1]] <<- rlabel
        output_row <<- output_row + 1
      }
    })
  }

  # Fill in col labels
  if(label_rows > 0)
  {
    output_col <- label_cols + 1
    sapply(1:cols(table), FUN=function(col){
      rlabel <- attr(table[[1]][[col]], "col_header") # Only take col labels from row 1

      if(inherits(rlabel, "tangram")) {
        sapply(1:cols(rlabel), FUN=function(inner_col) {
          sapply(1:rows(rlabel), FUN=function(inner_row) {
            rl <- rlabel[[inner_row]][[inner_col]]
            attr(rl, "parity") <- "even"
            new_tbl[[inner_row]][[output_col]] <<- rl
          })
          output_col <<- output_col + 1
        })
      }
      else
      {
        attr(rlabel, "parity") <- "even"
        new_tbl[[1]][[output_col]] <<- rlabel
        output_col <<- output_col + 1
      }
    })
  }

  # Set label class in upper left corner, that represent headers
  if(label_cols > 0 && label_rows > 0)
  {
    sapply(1:label_rows, FUN=function(row){
      sapply(1:label_cols, FUN=function(col){

        new_tbl[[row]][[col]] <<- if(inherits(new_tbl[[row]][[label_cols+1]], "cell_subheader"))
          cell_subheader("", parity="even")
        else
          cell_header("", parity="even")
      })
    })
  }

  # Main loop to fill final from provided
  output_row <- label_rows + 1
  sapply(1:rows(table), FUN=function(row) {
    output_col <- label_cols + 1
    sapply(1:cols(table), FUN=function(col) {
      element <- table[[row]][[col]]

      if(inherits(element, "tangram") && attr(element, "embedded"))
      {
        n_cols <- length(element[[1]]) # Number of columns in first row
        ## Need another double sapply here.
        sapply(element, FUN=function(inner_row)
        {
          sapply(inner_row, FUN=function(inner_element)
          {
            if(is.null(inner_element)) inner_element <- cell_label("")

            attr(inner_element, "parity") <- ifelse(row %% 2==0,"even", "odd")

            new_tbl[[output_row]][[output_col]] <<- inner_element

            output_col <<- output_col + 1
          })
          if(length(inner_row) < n_cols)
          {
            filler = n_cols - length(inner_row)
            for(i in 1:filler)
            {
              i_ele <- cell_label("", parity=ifelse(row %% 2==0,"even", "odd"))
              new_tbl[[output_row]][[output_col]] <<- i_ele
              output_col <<- output_col + 1
            }
          }

          output_col <<- output_col - n_cols
          output_row <<- output_row + 1
        })
        output_row <<- output_row - length(element)
      }
      else
      {
        if(is.null(element)) element <- cell_label("")
        attr(element, "parity") <- ifelse(row %% 2==0,"even", "odd")
        new_tbl[[output_row]][[output_col]] <<- element
      }
      output_col <<- output_col + length(element[[1]])

    })
    output_row <<- output_row + length(table[[row]][[1]])
  })

  new_tbl
}

cell_create_table <- function(ast, transforms, digits, ...)
{
  elements <- ast$terms()

  width  <- length(elements[[1]])
  height <- length(elements[[2]])
  tbl    <- tangram(height, width, FALSE)

  sapply(1:width, FUN=function(col_idx) {
    column <- elements[[1]][[col_idx]]

    sapply(1:height, FUN=function(row_idx) {
      row <- elements[[2]][[row_idx]]

      rowtype <- if(is.na(row$type))
                   transforms[["Type"]](row$data) else row$type
      coltype <- if(is.na(column$type))
                   transforms[["Type"]](column$data) else column$type

      transform <- if("list" %in% class(transforms[[rowtype]]))
        transforms[[rowtype]][[coltype]] else
        transforms[[rowtype]]

      if(is.null(row$format) || is.na(row$format)) row$set_format(digits)

      tbl[[row_idx]][[col_idx]] <<- transform(table_builder(row$value, column$value, TRUE), row, column, ...)$table
    })
  })

  flat <- table_flatten(tbl)

  if(!is.null(transforms[["Footnote"]]))
  {
    attr(flat, "footnote") <- transforms[["Footnote"]]
  }

  flat
}

#' Table creation methods
#'
#' The tangram method is the principal method to create tables. It uses
#' R3 method dispatch. If one specifies rows and columns, one gets an empty
#' table of the given size. A formula or character will invoke the parser
#' and process the specified data into a table like \code{Hmisc::summaryM}.
#' Given an \code{rms} object it will summarize that model in a table. A
#' \code{data.frame} is converted directly into a table as well for later
#' rendering. Can create tables from summary.rms(), anova.rms(), and other rms object info to create a
#' single pretty table of model results. The rms and Hmisc packages are required.
#'
#' @param x object; depends on S3 type, could be rows, formula, string of a formula, data.frame or numerical rows, an rms.model
#' @param after function or list of functions; one or more functions to further process an abstract table
#' @param as.character logical; if true data.frames all variables are passed through as.character and no numerical summary is provided.
#' @param colheader character; Use as column headers in final table
#' @param cols numeric; An integer of the number of cols to create
#' @param data data.frame; data to use for rendering tangram object
#' @param digits numeric; default number of digits to use for display of numerics
#' @param embedded logical; Will this table be embedded inside another
#' @param footnote character; A string to add to the table as a footnote.
#' @param quant numeric; A vector of quantiles to use for summaries
#' @param msd logical; Include mean and standard deviation in numeric summary
#' @param transforms list of lists of functions; that contain the transformation to apply for summarization
#' @param rnd.digits numeric; Digits to round reference, comparison, result and CI values to. Defaults to 2.
#' @param rnd.stats numeric; Digits to round model LR, R2, etc to. Defaults to rnd.digits.
#' @param short.labels numeric; Named vector of variable labels to replace in interaction rows. Must be in format c("variable name" = "shortened label").
#' @param ... addition models or data supplied to table construction routines
#'
#' @return A tangram object (a table).
#'
#' @rdname tangram
#' @export
#'
#' @examples
#' tangram(1, 1)
#' tangram(data.frame(x=1:3, y=c('a','b','c')))
#' tangram(drug ~ bili + albumin + protime + sex + age + spiders, pbc)
#' tangram("drug ~ bili + albumin + stage::Categorical + protime + sex + age + spiders", pbc)
tangram <- function(x, ...)
{
  UseMethod("tangram", x)
}

#' @rdname tangram
#' @export
tangram.numeric <- function(x, cols, embedded=FALSE, ...)
{
  # A list of lists
  result <- lapply(1:x, function(x) {lapply(1:cols, function(y) cell("")) })
  class(result) <- c("tangram", "list")
  attr(result, "embedded") <- embedded

  result
}

#' @rdname tangram
#' @export
tangram.data.frame <- function(x, colheader=NA, ..., quant=seq(0,1,0.25), msd=TRUE, as.character=NULL)
{
  cls <- sapply(names(x), function(y) class(x[1,y]))

  if(is.null(as.character)) as.character <- !any(!cls %in% c("character", "NULL", "labelled"))

  # Check for non-character
  if(!as.character)
  {
    nms <- names(cls)[cls %in% c("integer", "factor", "numeric")]
    return(tangram(paste0("1~", paste0(nms, collapse='+')), x, quant=quant, msd=msd, ...))
  }

  width   <- length(colnames(x)) + 1
  height  <- length(rownames(x)) + 1
  tbl     <- tangram(height, width, FALSE)

  sapply(2:width, FUN=function(col_idx) {
    if(any(is.na(colheader)))
    {
      lbl <- attr(x[,col_idx-1], "label")
      if(is.null(lbl)) lbl <- colnames(x)[col_idx-1]
      tbl[[1]][[col_idx]] <<- cell_header(lbl)
    } else {
      tbl[[1]][[col_idx]] <<- cell_header(colheader[col_idx-1])
    }
    sapply(2:height, FUN=function(row_idx) {
      if(col_idx > 2)
      {
        tbl[[row_idx]][[col_idx]] <<- cell(
          trimws(x[row_idx-1,col_idx-1],which="right"),
          class="data",
          parity=ifelse(row_idx %% 2==0, "even", "odd"))
      } else
      {
        tbl[[row_idx]][[col_idx]] <<- cell_header(
          trimws(x[row_idx-1,col_idx-1],which="right"),
          parity=ifelse(row_idx %% 2==0, "even", "odd"))
      }
    })
  })

  if(any(rownames(x) != as.character(1:(height - 1))))
  {
    tbl[[1]][[1]] <- cell_header("")
    sapply(2:height, FUN=function(row_idx) {
      tbl[[row_idx]][[1]] <<- cell_header(rownames(x)[row_idx-1])
    })
  } else {
    tbl <- del_col(tbl, 1)
  }

  tbl
}

#' @rdname tangram
#' @export
tangram.formula <- function(x, data, transforms=hmisc_style, after=NA, digits=NA, ...)
{
  if(length(class(data)) > 1 || class(data) != "data.frame") data <- as.data.frame(data)
  if(length(class(data)) > 1 || class(data) != "data.frame") stop("data must be supplied as data frame")

  # Helper function for single transform function
  if(!inherits(transforms, "list"))
  {
    transforms <- list(
      Type = function(x) {"Data"}, # Short circuit data type determination
      Data = list(
        Data = transforms,
        ASTMultiply = transforms
      ),
      ASTMultiply = list(
        Data = transforms,
        ASTMultiply = transforms
      )
    )
  }

  tbl <- cell_create_table(Parser$new()$run(x)$reduce(data)$distribute(),
                           transforms,
                           digits,
                           ...)

  if(suppressWarnings(all(is.na(after)))) {return(tbl)}

  # Post function processing
  if(class(after) == "list") sapply(as.list(after), function(f) tbl <<- f(tbl)) else tbl <- after(tbl)

  tbl
}

#' @rdname tangram
#' @export
tangram.character <- function(x, data, transforms=hmisc_style, after=NA, digits=NA, ...)
{
  tangram.formula(x, data, transforms, after, digits, ...)
}

#' @rdname tangram
#' @export
tangram.table <- function(x, ...)
{
  tbl <- tangram(1,1)

  if(is.null(dim(x)) || length(dim(x)) == 1)
  {
    tbl[[1]][[1]] <- cell_header("")
    sapply(1:length(x), function(i) tbl[[1]][[i+1]] <<- cell_header(names(x)[i]))
    tbl[[2]] <- list(cell_header(if(is.null(attr(x, "label"))) "" else attr(x, "label")))
    sapply(1:length(x), function(i) tbl[[2]][[i+1]] <<- cell(as.numeric(x[i])))
  } else if(length(dim(x)) == 2)
  {
    tbl[[1]][[1]] <- cell_header("")
    sapply(1:(dim(x)[1]), function(i) tbl[[1]][[i+1]] <<- cell_header(colnames(x)[i]))

    sapply(1:(dim(x)[1]), function(i) {
      tbl[[i+1]] <<- list(cell_header(if(is.null(rownames(x))) "" else rownames(x)[i]))
      sapply(1:(dim(x)[2]), function(j) tbl[[i+1]][[j+1]] <<- cell(as.numeric(x[i,j])))
    })
  } else
  {
    stop("Tables above 2 dimensions are not supported.")
  }

  tbl
}

#' @rdname tangram
#' @export
tangram.tbl_df <- function(x, ...)
{
  tangram(as.data.frame(x), as.character=TRUE)
}
