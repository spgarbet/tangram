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


# Find all factors and their selectors via breadth first construction
# If they exist
#' @include transform-hmisc.R
node_2_factors <- function(node)
{
  if("ASTMultiply" %in% class(node))
  {
    l <- node_2_factors(node$left)
    if(is.null(l)) return(NULL)
    r <- node_2_factors(node$right)
    if(is.null(r)) return(l)

    if("factor" %in% names(r)) list(l,r) else c(l, r)
  } else {
    if(hmisc_data_type(node$data) == "Numerical") return(NULL)

    list(list(
      factor=derive_label(node),
      levels=lapply(levels(factor(node$data)), function(x)
      {
        if(is.na(x)) list() else
          list(
            name     = as.character(x),
            selector = node$data == x
          )
      }))
    )
  }
}

# Find the terminal data node if it exists
data_node <- function(node)
{
  if("ASTMultiply" %in% class(node))
  {
    if(hmisc_data_type(node$left$data) == "Numerical") return(node)

    return(data_node(node$right))
  } else {
    if(hmisc_data_type(node$data) == "Numerical") return(node)
    NULL
  }
}

construct_headers <- function(factors)
{
  # Figure out length of each factor
  len    <- sapply(factors, function(n) length(n$levels))
  # Determine size of breaks for each factor
  breaks <- cumprod(rev(len))
  breaks <- if(length(breaks) > 1) c(rev(breaks[1:(length(breaks)-1)]), 1) else 1
  for(i in 1:length(breaks)) factors[[i]]$gap <- breaks[i] -1
  # How many times each factor repeats based on length and breaks
  reps <- len*breaks
  reps <- reps[1]/reps
  for(i in 1:length(breaks)) factors[[i]]$rep <- reps[i]

  # Now construct the lists of headers
  hdrs <- lapply(factors, function(i) {
    rep(as.vector(sapply(i$levels, function(j) {
      c(j$name, rep("", i$gap))
    })), i$rep)
  })

  #for(i in 1:length(hdrs)) {
  #  hdrs[[i]][1] <- paste0(factors[[i]]$factor, ": ", hdrs[[i]][1])
  #}

  hdrs
}

construct_selectors <- function(factors)
{
  n      <- length(factors[[1]]$levels[[1]]$selector)
  levels <- lapply(factors, function(i) sapply(i$levels, function(j) j$selector))
  result <- Reduce(x=levels,
         init=matrix(rep(TRUE, n)),
         f=function(i,j) {
     di <- dim(i)[2]
     dj <- dim(j)[2]
     #matrix(rep(i, dim(j)[2]), nrow=dim(i)[1]) & matrix(rep(j, dim(i)[2]), nrow=dim(i)[1])
     i[,rep(1:di, each=dj)] & j[,rep(1:dj, times=di)]
  })
  as.matrix(result)
}

#' Tangram transform for proc_tab style summaries via a function
#'
#' Given a function that produces a vector of tangram cells, will generate a table
#'
#' @param table The table builder object
#' @param row The row from the abstract syntax tree that parsed the formula
#' @param column The column from the abstract syntax tree that parsed the formula
#' @param fun The function to apply to the broken out categories
#' @param overall Provide a summary of categorical breakdowns
#' @param ... additional arguments to pass to fun
#' @export
proc_tab <- function(table, row, column, fun=NULL, overall=FALSE, ...)
{
  row_f <- node_2_factors(row)
  col_f <- node_2_factors(column)

  row_d <- data_node(row)
  col_d <- data_node(column)

  if(is.null(row_d) && is.null(col_d)) stop("No numerical term specified in formula")

  col_hdrs <- construct_headers(col_f)
  for(i in 1:length(col_hdrs))
  {
    table <- if(i == 1) {
      if(overall) col_header(table, "N", col_hdrs[[i]], "Overall")  else col_header(table, "N", col_hdrs[[i]])
    } else {
      if(overall) col_header(table, "", col_hdrs[[i]], "") else col_header(table, "", col_hdrs[[i]])
    }
  }

  row_hdrs <- construct_headers(row_f)
  row_selc <- construct_selectors(row_f)
  col_selc <- construct_selectors(col_f)

  table <- row_header(table, cell_header(derive_label(row_d)), rep("", length(row_hdrs)))
  table <- add_col(table, rep("", (dim(col_selc)[2])+(if(overall) 2 else 1)))
  table <- new_row(table)

  # Define the function of the selector (Tricky due to multiple cases of col/row)
  g <- function(selc)
  {
    elm <- if(!is.null(row_d) && !is.null(col_d))
    {
      fun(row_d, col_d, selc, ...)
    } else {
      if(is.null(row_d)) fun(col_d, selc) else fun(row_d, selc)
    }
    if("cell" %in% class(elm)) elm else cell(elm)
  }

  # Now load the cells with the proc func execution
  for(i in 1:length(row_hdrs[[1]]))
  {
    row <- row_selc[,i]

    table <- row_header(table, "", sapply(row_hdrs, function(j) cell_subheader(j[i])))

    n <- sum(apply(col_selc, 1, any) & row, na.rm=TRUE)

    table <- add_col(table, cell_n(n))

    for(j in 1:(dim(col_selc)[2])) table <- add_col(table, g(row & col_selc[,j]))

    if(overall) table <- add_col(table, g(row))

    table <- new_line(table)
  }

  table
}
