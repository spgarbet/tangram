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

construct_headers <- function(factors)
{
  # Figure out length of each factor
  len    <- sapply(factors, function(n) length(n$levels))
  # Determine size of breaks for each factor
  breaks <- cumprod(rev(len))
  breaks <- c(rev(breaks[1:(length(breaks)-1)]), 1) 
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
  
  for(i in 1:length(hdrs)) {
    hdrs[[i]][1] <- paste0(factors[[i]]$factor, ": ", hdrs[[i]][1])
  }
  
  hdrs
}

construct_selectors <- function(factors)
{
  n      <- length(factors[[1]]$levels[[1]]$selector)
  levels <- lapply(factors, function(i) sapply(i$levels, function(j) j$selector))
  Reduce(x=levels,
         init=list(rep(TRUE, n)),
         f=function(i,j) {
    
  })
}

proc_tab <- function(table, row, column, fun=NULL, ...)
{
  row_f <- factor_selectors(row)
  col_f <- factor_selectors(column)
  
  row_d <- data_node(row)
  col_d <- data_node(column)
  
  if(is.null(row_d) && is.null(col_d)) stop("No numerical term specified in formula")
  
  sapply(construct_headers(column), function(i) table <<- col_header(table, i))
  
  sapply(construct_selectors(row_f), function(row){
    sapply(construct_selectors(col_f), function(col) {
      
    })
  })
  
  # Recursive application here?

  # Function application depends on formula
  elm <- if(!is.null(row_d) && !is.null(col_d))
  {
    fun(row_d, col_d, selector)
  } else {
    if(is.null(row_d)) fun(col_d, selector) else fun(row_d, selector)
  }
  elm <- if("cell" %in% class(elm)) elm else cell(elm)
  
  table
}