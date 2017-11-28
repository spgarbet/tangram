# Find all factors and their selectors via breadth first construction
# If they exist
#' @include transform-hmisc.R
factor_selectors <- function(node)
{
  if("ASTMultiply" %in% class(node))
  {
    l <- factor_selectors(node$left)
    if(is.null(l)) return(NULL)
    r <- factor_selectors(node$right)
    if(is.null(r)) return(list(l))
    
    if("factor" %in% names(r)) list(l,r) else c(list(l), r)
  } else {
    if(hmisc_data_type(node$data) == "Numerical") return(NULL)
    
    list(
      factor=derive_label(node),
      levels=lapply(levels(factor(node$data)), function(x)
      {
        if(is.na(x)) list() else
          list(
            name     = as.character(x),
            selector = node$data == x
          )
      }
      )
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


proc_tab <- function(table, row, column, fun=NULL, ...)
{
  row_f <- factor_selectors(row)
  col_f <- factor_selectors(column)
  
  row_d <- data_node(row)
  col_d <- data_node(column)
  
  if(is.null(row_d) && is.null(col_d)) stop("No numerical term specified in formula")
  
  # Construct col header here
  len    <- sapply(col_f, function(n) length(n$levels))
  breaks <- cumprod(rev(len))
  breaks <- c(rev(breaks[1:(length(breaks)-1)]), 1) 
  for(i in 1:length(breaks)) col_f[[i]]$gap <- breaks[i] -1
  reps <- len*breaks
  reps <- reps[1]/reps
  for(i in 1:length(breaks)) col_f[[i]]$rep <- reps[i]
  hdrs <- lapply(col_f, function(i) {
    rep(as.vector(sapply(i$levels, function(j) {
      c(paste0(i$factor,":",j$name), rep("", i$gap))
    })), i$rep)
  })
  
  sapply(hdrs, function(i){
    table <<- col_header(table, i)
  })
  
  # Recursive application here?

  # Function application depends on formula
  ele <- if(!is.null(row_d) && !is.null(col_d))
  {
    fun(row_d, col_d, selector)
  } else {
    if(is.null(row_d)) fun(col_d, selector) else fun(row_d, selector)
  }
  
  ele <- if("cell" %in% class(ele)) ele else cell(ele)
  
  table
}