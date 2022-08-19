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
  if(is.null(attr(table, "row_header")) &&
     is.null(attr(table, "col_header")) &&
     !any(sapply(table, function(y) any(sapply(y, function(z) inherits(z, "tangram")))))
    )
  {
    # Nothing to flatten
    return(table)
  }

  if(!is.null(attr(table, "row_header")) ||
     !is.null(attr(table, "col_header")) ||
     (inherits(table, "list") & inherits(table[[1]], "list") & !inherits(table[[1]][[1]], "tangram"))
  )
  {
    x <- tangram(1, 1)
    x[[1]][[1]] <- table
    for(i in c("id", "caption", "style", "footnote", "args", "fixed_thead"))
      attr(x, i) <- attr(table, i)

    table <- x
  }

  # Compute final size of table
  final_rows    <- 0
  final_cols    <- 0
  sapply(1:rows(table), FUN=function(row) {
    element <- table[[row]][[1]]
    if(inherits(element, "tangram"))
      final_rows <<- final_rows + length(element)
    else
      final_rows <<- final_rows + 1
  })
  sapply(1:cols(table), FUN=function(col){
    element <- table[[1]][[col]]
    if(inherits(element, "tangram"))
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

      if(inherits(element, "tangram"))
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

  for(i in c("id", "caption", "style", "footnote", "args","fixed_thead"))
    attr(new_tbl, i) <- attr(table, i)

  new_tbl %>% home()
}

cell_create_table <- function(ast, transforms, digits, style, ...)
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

      if(is.null(transforms[["Cell"]]))
      {
        tbl[[row_idx]][[col_idx]] <<- transform(tangram(1,1), row, column, style=style, ...)
      } else {
        tbl[[row_idx]][[col_idx]] <<- transform(tangram(1,1), row, column, cell_style=transforms[["Cell"]], style=style, ...)
      }
    })
  })

  flat <- table_flatten(tbl)

  if(!is.null(transforms[["Footnote"]]))
  {
    attr(flat, "footnote") <- transforms[["Footnote"]]
  }

  flat
}

# Internal for excluding data.
exclude_data <- function(df, criteria)
{
  thatsafactjack <- Filter(function(a) is.factor(df[,a]), colnames(df))

  sel            <- rep(TRUE, nrow(df))
  if(is.list(criteria))
  {
    for(a in names(criteria)) sel <- sel & (match(df[,a], criteria[[a]], nomatch=0L)==0)
  } else
  {
    for(a in colnames(df)) sel <- sel & (match(df[,a], criteria, nomatch=0L)==0)
  }

  df             <- df[sel,,drop=FALSE]

  browser()
  for(a in thatsafactjack)
  {
    if(is.list(criteria))
    {
      if(a %in% names(criteria) &&
         (length(criteria[[a]]) > 1 || !is.na(criteria[[a]]))
        )
        df[,a] <- factor(df[,a], exclude=criteria[[a]])
    } else if(!is.na(a))
    {
      df[,a] <- factor(df[,a], exclude=criteria)
    }
  }

  df
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
#' Note that additional arguments are passed to any subsequent transform. This means that a lot
#' of possible arguments are not documented here but in the transform applied.
#' Examine their documentations for additional possible arguments if needed.
#'
#' @seealso Possible transforms are (see \code{\link{hmisc}}) (*default*),
#' \code{\link{nejm}} and \code{\link{lancet}}.
#'
#' @param x object; depends on S3 type, could be rows, formula, string of a formula, data.frame or numerical rows, an rms.model
#' @param id character; A unique charcter id used to identify this table over multiple runs. No spaces.
#' @param caption character; A string with the desired caption
#' @param style character; Desired rendering style, currently supports "hmisc", "nejm", and "lancet". Defaults to "hmisc"
#' @param footnote character; A vector of character strings as footnotes
#' @param after function or list of functions; one or more functions to further process an abstract table
#' @param as.character logical; if true data.frames all variables are passed through as.character and no numerical summary is provided.
#' @param colheader character; Use as column headers in final table
#' @param cols numeric; An integer of the number of cols to create
#' @param data data.frame; data to use for rendering tangram object
#' @param digits numeric; default number of digits to use for display of numerics
#' @param exclude vector or list; When x is a data.frame this exclusion criteria is applied to the data. If this is a list then each list pair is the (column name, criteria). It is preferred to use a list to be specific.
#' @param fixed_thead logical; On conversion to HTML5 should headers be treated as fixed?
#' @param format numeric or character; Format to apply to statistic
#' @param include_p logical; Include p-value when printing statistic
#' @param quant numeric; A vector of quantiles to use for summaries
#' @param msd logical; Include mean and standard deviation in numeric summary
#' @param pformat function or character; A function to format p values
#' @param test logical or function; Perform default test or a statistical function that will return a test result when passed a row and column
#' @param transforms list of lists of functions; that contain the transformation to apply for summarization
#' @param tformat numeric or character; format to apply to t-value
#' @param rnd.digits numeric; Digits to round reference, comparison, result and CI values to. Defaults to 2.
#' @param rnd.stats numeric; Digits to round model LR, R2, etc to. Defaults to rnd.digits.
#' @param short.labels numeric; Named vector of variable labels to replace in interaction rows. Must be in format c("variable name" = "shortened label").
#' @param ... addition models or data supplied to table construction routines
#'
#' @return A tangram object (a table).
#'
#' @importFrom utils find
#' @importFrom utils getAnywhere
#'
#' @rdname tangram
#' @export
#'
#' @examples
#' tangram(1, 1)
#' tangram(data.frame(x=1:3, y=c('a','b','c')), id="mytbl1")
#' tangram(drug ~ bili + albumin + protime + sex + age + spiders, pbc, id="mytbl2")
#' tangram("drug~bili+albumin+stage::Categorical+protime+sex+age+spiders", pbc,"mytbl3")
tangram <- function(x, ...)
{
  UseMethod("tangram", x)
}

#' @rdname tangram
#' @export
tangram.numeric <- function(x, cols, id=NULL, caption=NULL, style="hmisc", footnote=NULL, fixed_thead=NULL, ...)
{
  # A list of lists
  result <- lapply(1:x, function(x) {lapply(1:cols, function(y) cell("", ...)) })
  class(result) <- c("tangram", "list")

  attr(result, "id")       <- id
  attr(result, "caption")  <- caption
  attr(result, "style")    <- style
  attr(result, "footnote") <- footnote
  attr(result, "args")     <- list(...)
  attr(result, "fixed_thead") <- fixed_thead
  attr(result, "row")      <- 1
  attr(result, "col")      <- 1

  result
}

#' @rdname tangram
#' @export
tangram.anova.lme <- function(x, id=NULL, style="hmisc", caption=NULL, footnote=NULL, digits=NULL, fixed_thead=NULL, ...)
{
  y <- x
  y$call <- NULL
  class(y) <- "data.frame"

  if(is.null(digits)) digits = 3
  y[] <- lapply(y, function(z) render_f(z,digits))

  y$Model <- x$Model

  tangram.data.frame(y, id, caption=caption, footnote=footnote, style=style, as.character=TRUE, fixed_thead=fixed_thead, ...)
}

#' @rdname tangram
#' @export
tangram.data.frame <- function(x,
  id=NULL,
  colheader=NA,
  caption=NULL,
  style="hmisc",
  footnote=NULL,
  after=NA,
  quant=seq(0,1,0.25),
  msd=TRUE,
  as.character=NULL,
  fixed_thead=NULL,
  exclude=NULL,
  ...)
{
  if(!is.null(exclude)) x <- exclude_data(x, exclude)
  cls <- sapply(names(x), function(y) class(x[1,y]))

  if(is.null(id) && "knitr" %in% .packages()) id <- knitr::opts_current$get("label")
  if(is.null(as.character)) as.character <- !any(!cls %in% c("character", "NULL", "labelled"))

  # Check for non-character
  if(!as.character)
  {
    nms <- names(cls)[cls %in% c("integer", "factor", "numeric")]
    return(tangram(paste0("1~", paste0(nms, collapse='+')), x, id=id, caption=caption, style=style, footnote=footnote, after=after, quant=quant, msd=msd, fixed_thead=fixed_thead, ...))
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

  attr(tbl, "id")       <- id
  attr(tbl, "caption")  <- caption
  attr(tbl, "style")    <- style
  attr(tbl, "footnote") <- footnote
  attr(tbl, "args")     <- list(...)
  attr(tbl, "fixed_thead") <- fixed_thead
  attr(tbl, "row")      <- 1
  attr(tbl, "col")      <- 1

  if(suppressWarnings(all(is.na(after)))) {return(tbl)}

  # Post function processing
  if(inherits(after,"list")) sapply(as.list(after), function(f) tbl <<- f(tbl)) else tbl <- after(tbl)

  tbl
}

#' @rdname tangram
#' @export
tangram.formula <- function(x,
  data=NULL,
  id=NULL,
  transforms=NULL,
  caption=NULL,
  style="hmisc",
  footnote=NULL,
  after=NA,
  digits=NA,
  fixed_thead=NULL,
  exclude=NULL,
  ...)
{
  if(!is.null(data) && (length(class(data)) > 1 || !inherits(data,"data.frame"))) data <- as.data.frame(data)
  if(!is.null(data) && (length(class(data)) > 1 || !inherits(data,"data.frame"))) stop("data must be supplied as data frame")
  if(is.null(id) && "knitr" %in% .packages()) id <- knitr::opts_current$get("label")
  if(is.null(transforms)) transforms <- tryCatch(get(style, envir=globalenv()), error = getAnywhere(style))

  if(!is.null(exclude)) data <- exclude_data(data, exclude)


  # Helper function for single transform function
  if(!inherits(transforms, "list"))
  {
    if(length(formals(transforms)) == 2)
    {
      f <- transforms
      transforms <- function(table, row, column, cell_style, ...) {
          table                            %>%
          col_header(derive_label(column)) %>%
          row_header(derive_label(row))    %>%
          add_row(f(row$data, column$data))
      }
    }

    transforms <- list(
      Type = function(x) {"Data"}, # Short circuit data type determination
      Data = list(
        Data = transforms,
        ASTMultiply = transforms
      ),
      ASTMultiply = list(
        Data = transforms,
        ASTMultiply = transforms
      ),
      Cell = NULL
    )
  }

  location <- suppressWarnings(find(deparse(substitute(transforms))))

  if(length(location) > 0 && location[1] != "package:tangram")
  {
    if(find("add_row")[1] != "package:tangram")
      warning(paste0("add_row is masked by ", find("add_row"), ". This can cause user provided transforms to fail.\nUse tangram::add_row if that is the intent or change package load order.\n"))

    if(find("add_col")[1] != "package:tangram")
      warning(paste0("add_col is masked by ", find("add_col"), ". This can cause user provided transforms to fail.\nUse tangram::add_col if that is the intent or change package load order.\n"))
  }

  tbl <- cell_create_table(Parser$new()$run(x)$reduce(data)$distribute(),
                           transforms,
                           digits,
                           style,
                           ...)

  attr(tbl, "id")       <- id
  attr(tbl, "caption")  <- caption
  attr(tbl, "style")    <- style
  attr(tbl, "footnote") <- if(is.null(footnote)) attr(tbl, "footnote") else footnote
  attr(tbl, "args")     <- list(...)
  attr(tbl, "fixed_thead") <- fixed_thead
  attr(tbl, "row")      <- 1
  attr(tbl, "col")      <- 1

  if(suppressWarnings(all(is.na(after)))) {return(tbl)}

  # Post function processing
  if(inherits(after,"list")) sapply(as.list(after), function(f) tbl <<- f(tbl)) else tbl <- after(tbl)

  tbl
}

#' @rdname tangram
#' @export
tangram.character <- function(x, ...)
{
  tangram.formula(trimws(x), ...)
}

#' @rdname tangram
#' @importFrom stats mantelhaen.test
#' @param percents logical; Display percents when rendering a table object. Defaults to FALSE
#' @export
tangram.table <- function(
  x,
  id       = NULL,
  percents = FALSE,
  digits   = 1,
  test     = FALSE,
  footnote = NULL,
  ...)
{
  dimension <- if(is.null(dim(x))) 1 else length(dim(x))

  if(is.function(test))
  {
    stat <- test(x)
    test <- TRUE
  } else if(test)
  {
    if(dimension <= 2)
    {
      result <- chisq.test(x)
      stat   <- hmisc_chi2(render_f(result$statistic, digits),
                           result$parameter,
                           hmisc_p(result$p.value))
      if(is.null(footnote)) footnote <- "^2^\u03a7^2^ Contingency table test"
    } else if (dimension <= 4)
    {
      result <- mantelhaen.test(x)
      stat  <- if(!is.na(result$statistic))
      {
         cell(paste0("M^2^=", render_f(result$statistic, digits),
                     ", df=", result$parameter,
                     ", ",    hmisc_p(result$p.value), "^1^"),
              class="statistics",
              ...)
      } else cell("\u2014", ...)

      if(is.null(footnote)) footnote <- "^1^Cochran-Mantel-Haenszel test"
    }
  }

  tbl <- tangram(1,1, id=id, footnote=footnote, ...)

  if(dimension == 1)
  {
    cols <- if(is.null(names(x))) paste("Level", 1:length(x)) else names(x)
    tbl[[1]] <- lapply(cols, cell_header, USE.NAMES=FALSE)
    tbl[[2]] <- lapply(x, cell)
  } else if(dimension == 2)
  {
    rows     <- if(is.null(rownames(x))) paste("Level", 1:dim(x)[1]) else dimnames(x)[[1]]
    rows     <- sapply(rows, function(y) cell_header(y), USE.NAMES=FALSE)
    cols     <- if(is.null(colnames(x))) paste("Level", 1:dim(x)[2]) else dimnames(x)[[2]]
    tbl[[1]] <- lapply(c("", cols), cell_header, USE.NAMES=FALSE)

    for(i in 1:(dim(x)[1]))
    {
      denom <- colSums(x)
      tbl[[i+1]] <- if(percents)
      {
        pcnt  <- paste0(" (", render_f(100*x[i,] / denom, digits), ")")
        lapply(c("", paste0(x[i,],pcnt)), cell, USE.NAMES=FALSE)
      } else
      {
        lapply(c("",x[i,]), cell, USE.NAMES=FALSE)
      }
      tbl[[i+1]][[1]] <- cell_header(rows[i])
    }
  } else if(dimension == 3)
  {
    rows <- if(is.null(rownames(x))) paste("Level", 1:dim(x)[1]) else dimnames(x)[[1]]
    rows <- sapply(rows, function(y) cell_header(y), USE.NAMES=FALSE)
    tbl  <- tangram(x[1,,], id=id, percents=percents, digits=digits, test=FALSE, ...) %>%
            insert_column(0, cell_header(""), rows[1])
    for(i in 2:(dim(x)[1]))
    {
      tmp <- tangram(x[i,,], id=id, percents=percents, digits=digits, test=FALSE, ...) %>%
             insert_column(0, cell_header(""), rows[i]) %>%
             del_row(1)
      tbl <- rbind(tbl, tmp)
    }
  } else if(dimension == 4)
  {
    rows <- if(is.null(rownames(x))) paste("Level", 1:dim(x)[1]) else paste0("**", dimnames(x)[[1]], "**")
    rows <- sapply(rows, function(y) cell_header(y), USE.NAMES=FALSE)
    tbl  <- tangram(x[1,,,], id=id, percents=percents, digits=digits, test=FALSE, ...) %>%
            insert_column(0, "", rows[1], class="cell_header")
    for(i in 2:(dim(x)[1]))
    {
      tmp <- tangram(x[i,,,], id=id, percents=percents, digits=digits, test=FALSE, ...) %>%
             insert_column(0, "", rows[i], class="cell_header") %>%
             del_row(1)
      tbl <- rbind(tbl, tmp)
    }
  } else
  {
    stop("tangram table conversion above 4 dimensions not supported")
  }

  if(test)
  {
    tbl <- new_col(tbl)
    for(i in 1:length(tbl)) tbl <- add_row(tbl, "")
    tbl[[1]][[length(tbl[[1]])]] <- cell_header("Test")
    tbl[[2]][[length(tbl[[1]])]] <- stat
  }

  attr(tbl, "footnote") <- footnote

  tbl
}

#' @rdname tangram
#' @export
tangram.ftable <- function(x, id=NULL, ...)
{
  tangram.table(as.table(x), id=id, ...)
}


#' @rdname tangram
#' @export
tangram.matrix <- function(x, digits=NULL, ...)
{
  if(!is.null(digits)) x <- round(x, digits)
  x <- tangram(as.data.frame(x), as.character=TRUE, ...)
  for(i in 1:length(x))
  {
    x[[i]][[1]] <- cell(as.character(x[[i]][[1]]))
  }
  x
}

#' @rdname tangram
#' @export
tangram.tbl_df <- function(x, ...)
{
  tangram(as.data.frame(x), as.character=TRUE, colheader=colnames(x), ...)
}

#' @rdname tangram
#' @export
tangram.lm <- function(x, ...) tangram(summary(x), ...)

#' @rdname tangram
#' @export
tangram.summary.lm <- function(x, id=NULL, format=NULL, pformat=NULL, tformat=NULL, ...)
{
  if(is.null(pformat)) pformat <- "%1.3f"
  if(is.null(tformat)) tformat <- 3
  y <- as.data.frame(x$coefficients)
  names(y) <- c("Estimate", "Std Error", "*t* statistic", "*p*-value")

  y[,4] <- hmisc_p(y[,4], pformat, include_p=FALSE)

  if(is.null(format))
  {
    format  <- if(rownames(y)[1] == "(Intercept)")
                  format_guess(y[2:length(y[,1]),1]) else
                  format_guess(y[,1])
  }

  y[,1] <- render_f(y[,1], format)
  y[,2] <- render_f(y[,2], format)
  y[,3] <- render_f(y[,3], tformat)

  m <- tangram(y, id=id, as.character=TRUE, ...)

  nr <- length(m) + 1

  m[[nr]] <- list(cell(""), cell(""), cell(""), cell(""), cell(""))

  m[[nr+1]] <- list(
    cell_header("Residual Std Err"),
    cell(paste0(render_f(x$sigma, 3), " on ", x$df[2], " dof")),
    cell(""), cell(""), cell(""))

  m[[nr+2]] <- list(
    cell_header("Multiple R^2^"),
    cell(paste0(render_f(x$r.squared, 3))),
    cell_header("Adj R^2^"),
    cell(paste0(render_f(x$adj.r.squared, 3))),
    cell(""))

  m[[nr+3]] <- list(
    cell_header(paste0("F~", x$fstatistic[2], "," , x$fstatistic[3], "~")),
    cell(render_f(unname(x$fstatistic[1]), 3)),
    cell(""), cell(""), cell(""))

  m[[nr+4]] <- list(
    cell_header("*p*-value"),
    cell(hmisc_p(pf(x$fstatistic[1], x$fstatistic[2], x$fstatistic[3], lower.tail=FALSE), pformat, include_p=FALSE)),
    cell(""), cell(""), cell(""))

  m
}
