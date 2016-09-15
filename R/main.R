#' @include S3-Cell.R
#' @include parser.R

cell_flatten <- function(table)
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
  row_label <- row_header(table[[1]][[1]])
  col_label <- col_header(table[[1]][[1]])

  # Set aside additional for labeling
  label_rows <- rows(col_label)
  label_cols <- cols(row_label)

  # Allocate final table
  new_tbl <- cell_table(final_rows+label_rows, final_cols+label_cols)

  # Fill in row labels
  output_row <- label_rows + 1
  sapply(1:rows(table), FUN=function(row){
    rlabel <- attr(table[[row]][[1]], "row_header") # Only take row labels from column 1

    if(inherits(rlabel, "cell_table")) {
      sapply(1:rows(rlabel), FUN=function(inner_row) {
        sapply(1:cols(rlabel), FUN=function(inner_col) {
          new_tbl[[output_row]][[inner_col]] <<- rlabel[[inner_row]][[inner_col]]
        })
        output_row <<- output_row + 1
      })
    }
    else
    {
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
          new_tbl[[inner_row]][[output_col]] <<- rlabel[[inner_row]][[inner_col]]
        })
        output_col <<- output_col + 1
      })
    }
    else
    {
      new_tbl[[1]][[output_col]] <<- rlabel
      output_col <<- output_col + 1
    }
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

      tbl[[row_idx]][[col_idx]] <<- transform(row, column)
    })
  })

  cell_flatten(tbl)
}

#' @export
summary_table <- function(formula, data, transforms=hmisc_style)
{
  # Helper function for single conversion function
  if(!inherits(transforms, "list"))
  {
    transforms <- list(
      Type = function(x) {"Data"},
      Data = list(
        Data = transforms
      )
    )
  }

  cell_create_table(Parser$new()$run(formula)$reduce(data)$distribute(),
                  transforms)
}
