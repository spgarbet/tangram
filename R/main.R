#' @include S3-Cell.R
#' @include parser.R

table_flatten <- function(table)
{
  # Compute final size of table
  final_rows    <- 0
  final_cols    <- 0
  sapply(1:rows(table), FUN=function(row) {
    element <- table[[row]][[1]]
    if(inherits(element, "cell_table") && attr(element, "embedded"))
      final_rows <<- final_rows + length(element)
    else
      final_rows <<- final_rows + 1
  })
  sapply(1:cols(table), FUN=function(col){
    element <- table[[1]][[col]]
    if(inherits(element, "cell_table") && attr(element, "embedded"))
      final_cols <<- final_cols + length(element[[1]])
    else
      final_cols <<- final_cols + 1
  })

  # Grab labels
  row_label <- attr(table[[1]][[1]], "row_header")
  col_label <- attr(table[[1]][[1]], "col_header")

  # Set aside additional for labeling
  label_rows <- rows(col_label) # How many rows in the column header
  label_cols <- cols(row_label) # How many cols in the row headers

  # Allocate final table
  new_tbl <- cell_table(final_rows+label_rows, final_cols+label_cols)

  # Fill in row labels
  output_row <- label_rows + 1
  sapply(1:rows(table), FUN=function(row){
    rlabel <- attr(table[[row]][[1]], "row_header") # Only take row labels from column 1

    if(inherits(rlabel, "cell_table")) {
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

  # Fill in col labels
  output_col <- label_cols + 1
  sapply(1:cols(table), FUN=function(col){
    rlabel <- attr(table[[1]][[col]], "col_header") # Only take col labels from row 1

    if(inherits(rlabel, "cell_table")) {
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

  # Set label class in upper left corner, that represent headers
  sapply(1:label_rows, FUN=function(row){
    sapply(1:label_cols, FUN=function(col){
      # Either a header or subheader
      hdr_class <- if (inherits(new_tbl[[row]][[label_cols+1]], "cell_subheader"))
                   {
                     c("cell_subheader", "cell_header")
                   } else {
                     "cell_header"
                   }

      class(new_tbl[[row]][[col]])  <<- c(hdr_class, class(new_tbl[[row]][[col]]))
      attr(new_tbl[[row]][[col]],"parity") <<- "even"
    })
  })


  # Main loop to fill final from provided
  output_row <- label_rows + 1
  sapply(1:rows(table), FUN=function(row) {
    output_col <- label_cols + 1
    sapply(1:cols(table), FUN=function(col) {
      element <- table[[row]][[col]]

      if(inherits(element, "cell_table") && attr(element, "embedded"))
      {
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
          output_col <<- output_col - length(inner_row)
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

cell_create_table <- function(ast, transforms)
{
  elements <- ast$terms()

  width  <- length(elements[[1]])
  height <- length(elements[[2]])
  tbl    <- cell_table(height, width, FALSE)

  sapply(1:width, FUN=function(col_idx) {
    column <- elements[[1]][[col_idx]]

    sapply(1:height, FUN=function(row_idx) {
      row <- elements[[2]][[row_idx]]

      rowtype <- if(is.na(row$type))    transforms[["Type"]](row$data)    else row$type
      coltype <- if(is.na(column$type)) transforms[["Type"]](column$data) else column$type

      transform <- transforms[[rowtype]][[coltype]]

      tbl[[row_idx]][[col_idx]] <<- transform(new_table_builder(row, column), row, column)$table
    })
  })

  flat <- table_flatten(tbl)

  if(!is.null(transforms[["Footnote"]]))
  {
    attr(flat, "footnote") <- transforms[["Footnote"]]
  }

  flat
}

#' Create a function to transform all cells of a table
#'
#' Given a function that operates on a table cell and returns
#' the modified cell, return a function that given a table
#' applies that function to all cells and returns the modified
#' table.
#'
#' @param FUN function to apply, must return the modified cell
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
    if(col < cols)
      for(i in (col+1):cols)
        sapply((col+1):cols, function(i) table[[row]][[i-1]] <<- table[[row]][[i]])
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
    columns <- sapply(1:length(table[[1]]), function(col) {
                 any(sapply(1:length(table), function(row) {
                   "statistics" %in% class(table[[row]][[col]])
                 }))
               })

    sapply((length(table[[1]]):1)[rev(columns)], function(col) {
      table <<- del_col(table, col)
    })

    table
}

#' Cleanup an intercept only model
#'
#' Cleanup an intercept only table that was generated from the hmisc default
#' transform. This drops the statistics column, and modifies the header
#' to eliminate blank space.
#'
#' @param table the table to modify
#' @param function to apply, must return the modified cell
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
       up$label == "")
    {
      class(below) <- tail(class(below), -1)
      table[[1]][[col]] <<- below
    }
  })

  del_row(table, 2)
}

#' Generate a table using a data frame directly
#'
#' Transform a data frame directly into a table
#' @param data the data frame to use
#' @return table
#' @export
summary_frame <- function(data, colheader=NA)
{
  roffset <- if(any(is.na(colheader))) 1 else 2
  width   <- length(colnames(data)) + 1
  height  <- length(rownames(data)) + roffset
  tbl     <- cell_table(height, width, FALSE)

  tbl[[1]][[1]] <- cell_header("")
  if(!any(is.na(colheader))) tbl[[2]][[1]] <- cell_subheader("")

  sapply(2:width, FUN=function(col_idx) {
    if(any(is.na(colheader)))
    {
      tbl[[1]][[col_idx]] <<- cell_header(colnames(data)[col_idx-1])
    } else {
      tbl[[1]][[col_idx]] <<- cell_header(colheader[col_idx-1])
      tbl[[2]][[col_idx]] <<- cell_subheader(colnames(data)[col_idx-1])
    }
    sapply((roffset+1):height, FUN=function(row_idx) {
       tbl[[row_idx]][[col_idx]] <<- cell_label(trimws(data[row_idx-roffset,col_idx-1]))
    })
  })

  sapply((roffset+1):height, FUN=function(row_idx) {
    tbl[[row_idx]][[1]] <<- cell_header(rownames(data)[row_idx-roffset])
  })

  tbl
}

#' Generate a summary table using a specified formula and data frame
#'
#' @param formula, the formula to apply for summarization
#' @param data the data frame to use
#' @param transforms a list of lists that contain the transformation to apply for summarization
#' @param after one or more functions to further process an abstract table
#' @return the table flattened
#' @export
#'
#' @examples
#'
#' summary_table("drug ~ bili + albumin + stage::Categorical + protime + sex + age + spiders", pbc)
#'
summary_table <- function(formula, data, transforms=hmisc_style, after=NA)
{
  # Helper function for single transform function
  if(!inherits(transforms, "list"))
  {
    transforms <- list(
      Type = function(x) {"Data"}, # Short circuit data type determination
      Data = list(
        Data = transforms
      )
    )
  }

  tbl <- cell_create_table(Parser$new()$run(formula)$reduce(data)$distribute(),
                           transforms)

  if(suppressWarnings(is.na(after))) {return(tbl)}

  # Post function processing
  if(class(after) == "list") sapply(as.list(after), function(f) tbl <<- f(tbl)) else tbl <- after(tbl)

  tbl
}

extract_data_set_labels <- function(data.set, short.labels)
{
  label.data <- NULL
  if(!is.null(data.set)){
    if('Labels' %in% names(contents(data.set)$contents)){
      label.data <- data.frame(variable = names(data.set),
                               varlabel = as.vector(contents(data.set)$contents$Labels),
                               stringsAsFactors = FALSE)

      ## Create indicator for whether shortened labels will be used; if so, create new column with
      ## original labels, replaced with short versions as indicated by names of short.labels
      use.short <- FALSE

      ## Remove and warn of any elements of short.labels that aren't in names(data.set)
      if(length(setdiff(names(short.labels), names(data.set))) > 0){
        message(paste("Note: these elements of short.labels do not appear in names(data.set):",
                      paste(setdiff(names(short.labels), names(data.set)), collapse = ', ')))
      }

      short.labels <- short.labels[names(short.labels) %in% names(data.set)]
      if(length(short.labels) > 0){
        use.short <- TRUE

        ## All short labels are the same as variable labels by default
        label.data$shortlabel <- label.data$varlabel

        ## Replace desired variable labels with short versions
        for(i in 1:length(short.labels)){
          varnum <- match(names(short.labels)[i], label.data$variable)
          if(!is.na(varnum)){
            label.data$shortlabel[varnum] <- short.labels[i]
          }
        }
      }
    }
  }
  # Create a third column for interaction variable use
  label.data$intlabel <- if(use.short) label.data$shortlabel else label.data$varlabel

  label.data
}

summary_rms <- function(object.model,
                        data.set = NULL,
                        short.labels = NULL,
                        rm.rows = c('nl.int', 'nl', 'int', 'none'))
{
  # Check Arguments
  if(!('rms' %in% class(model.obj))){
    stop('model.obj must be of class rms')
  } else if(!(is.null(data.set) | 'data.frame' %in% class(data.set))){
    stop('data.set must be of class data.frame')
  }

  ## -- General setup ----------------------------------------------------------------------------
  ## Which rows (if any) to remove from final?
  rm.rows <- match.arg(rm.rows)

  ## These model classes will have ratios presented, not beta coefficients
  use.ratios <- c('lrm', 'cph')

  ## -- Variable labels --------------------------------------------------------------------------
  ## If data.set is NULL or has no labels, use variable names
  label.data <- extract_data_set_labels(data.set, short.labels)

  ## -- summary.rms() results --------------------------------------------------------------------
  mod.sum <- as.data.frame(summary(model.obj)) ## Often won't print due to duplicate rownames

  ## Get variable name in every row
  mod.sum$quantity <- rownames(mod.sum)
  rownames(mod.sum) <- NULL

  ## Create variable column ##
  ## Models for which summary() produces both coefficients and ratios: Take only ratios, variable
  ## column = row above ratio row
  if(sum(!is.na(match(use.ratios, class(model.obj)))) > 0){
    mod.sum$var <- c(NA, mod.sum$quantity[1:(nrow(mod.sum) - 1)])
    mod.sum <- subset(mod.sum, Type == 2)
  } else{
    mod.sum$var <- mod.sum$quantity

    ## If model is a Poisson model, exponentiate all point estimates, CLs to get IRRs
    if('Glm' %in% class(model.obj)){
      if(model.obj$family$family == 'poisson'){
        mod.sum[,c('Effect', 'Lower 0.95', 'Upper 0.95')] <-
          exp(mod.sum[,c('Effect', 'Lower 0.95', 'Upper 0.95')])
      }
    }
  }

  ## For categorical covariates, var column currently is of format
  ##  "variable name - comparison category:reference category"; split out components
  var.split <- lapply(mod.sum$var, FUN = function(x){ strsplit(x, split = ' - |:')[[1]] })
  mod.sum$var <- unlist(lapply(var.split, FUN = function(x){ x[1] }))

  ## Create new low/high variables with categories (not factor numbers) or formatted numbers
  mod.sum$low.char <- unlist(lapply(var.split, FUN = function(x){ x[3] }))
  mod.sum$low.char <- with(mod.sum, ifelse(is.na(low.char),
    format(round(Low, rnd.digits), nsmall = rnd.digits),
    as.character(low.char)))
  mod.sum$high.char <- unlist(lapply(var.split, FUN = function(x){ x[2] }))
  mod.sum$high.char <- with(mod.sum, ifelse(is.na(high.char),
    format(round(High, rnd.digits), nsmall = rnd.digits),
    as.character(high.char)))

  ## -- anova.rms() results ----------------------------------------------------------------------
  ## Create data frame of anova() results, add column to indicate variable/quantity
  mod.anova <- as.data.frame(anova(model.obj))
  mod.anova$line <- rownames(mod.anova); rownames(mod.anova) <- NULL
  mod.anova$var <- gsub('^ ', '', gsub(' +\\(.*\\)$| : .*$', '', mod.anova$line))

  ## Remove rows requested in rm.rows
  if(rm.rows != 'none'){
    remove.these <- NULL
    if(rm.rows %in% c('nl', 'nl.int')){
      remove.these <- c(remove.these, '^ *Nonlinear', '^ *f\\(A,B\\)')
    }
    if(rm.rows %in% c('int', 'nl.int')){
      remove.these <- c(remove.these, '^ *All Interactions', '^ *Nonlinear Interaction')
    }

    mod.anova <- mod.anova[-unique(unlist(sapply(remove.these, grep, mod.anova[,'var']))),]
  }

  ## For each row, create unique var value that is as descriptive as possible
  cur.var <- NULL
  for(i in 1:nrow(mod.anova)){
    if((!is.null(data.set) & mod.anova$var[i] %in% names(data.set)) |
        length(grep('*', mod.anova$var[i], fixed = TRUE)) > 0){
      cur.var <- mod.anova$var[i]
    } else{
      if(length(grep('^TOTAL|ERROR', mod.anova$var[i])) == 0){
        mod.anova$var[i] <- ifelse(is.null(cur.var),
          gsub('^All ', '', mod.anova$var[i]),
          paste(cur.var, gsub('^All ', '', mod.anova$var[i])))
      }
    }
  }
}
