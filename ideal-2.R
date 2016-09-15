# Flip list of lists

# Special flatten list, that doesn't flatten cells
cell_flatten <- function(ls)
{
  flat <- list()
  el   <- 1

  for(a in ls)
  {
    if(inherits(a, "cell"))
    {
      flat[[el]] <- a
      el <- el + 1
    }
    else
    {
      for(b in a)
      {
        flat[[el]] <- b
        el <- el+1
      }
    }
  }
  flat
}

empty_table <- function(row, column)
{
  list(nrow=1, ncol=0, table=cell_table(1,1), row=row, col=column)
}

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

tg.aov <- function(model, row, column, ...)
{
  test <- summary(model)[[1]]
  cell_fstat(f   = cell_format("%.2f", test$'F value'[1]),
           n1  = test$Df[1],
           n2  = test$Df[2],
           p   = cell_format("%1.3f", test$'Pr(>F)'[1]),
           src = key(row, column, "aov"))
}

prepare_header_row <- function(row, col, additional_class, ...)
{
  # Get flattened args list
  flat <- cell_flatten(list(...))

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

add_N <- function(table_builder, n)
{
  table_builder$ncol <- table_builder$ncol+1
  table_builder$table[[table_builder$nrow]][[table_builder$ncol]] <- cell_n(n, src=key(table_builder$row, table_builder$col, "N"))
  table_builder
}

add_each <- function(table_builder, values, func)
{
  sapply(values, FUN=function(x) {
    table_builder <<- add_col(table_builder, func(table_builder, x))
  })
  table_builder
}

add_col <- function(table_builder, object)
{
  table_builder$ncol <- table_builder$ncol+1
  table_builder$table[[table_builder$nrow]][[table_builder$ncol]] <- tg(object, table_builder$row, table_builder$col)
  table_builder
}

new_row <- function(table_builder)
{
  table_builder$nrow <- table_builder$nrow + 1
  table_builder
}

N <- function(...)
{
  v <- c(...)
  attr(v, "class") <- c("N", "numeric")
  v
}

tg_quantile <- function(table_builder, data, subcol=NA, subrow=NA, ...)
{
  cell_quantile(quantile(data, ...),
                src=key(row    = table_builder$row,
                        col    = table_builder$col,
                        label  = "quantile",
                        subcol = subcol,
                        subrow = subrow))
}


### Now create custom function for counting events with a category
summarize_count <- function(table, row, column)
{
  # Assume no factors
  dr <- row$data[,1]
  dc <- column$data[,1]

  # Test a poisson model
  test <- aov(glm(x ~ treatment,
                  aggregate(dr, by=list(id=dr, treatment=dc), FUN=length),
                  family=poisson))

  # Compute N values for each category
  subN <- lapply(levels(dc), FUN=function(cat){
    length(unique(dr[dc==cat]))
  })

  # Begin table construction, ala Wickham Style (monadic continuation)
  table                                         %>%
  row_header(derive_label(row))                 %>%  # Name of the row
  col_header("N", levels(dc), "Test Statistic") %>%  # Main header
  col_header(NA,  N(subN),    NA)               %>%  # Subheader is N values  ## FIXME WHAT ABOUT SUBCOL in key
  add_N(length(unique(dr)))                     %>%  # First column is N value
  add_each(levels(dc), function(table_builder, cat)  # Quantile every category
  {
    x  <- dr[dc == cat]
    xx <- aggregate(x, by=list(x), FUN=length)$x
    tg_quantile(table_builder, xx, na.rm=TRUE, subcol=cat)
  })                                            %>%
  add_col(test)                                      # AOV results
}

n  <- 1000
df <- data.frame(id = sample(1:250, n*3, replace=TRUE), event = as.factor(rep(c("A", "B","C"), n)))

ast    <- Parser$new()$run(event ~ id)$reduce(df)
row    <- ast$right
column <- ast$left
table  <- summarize_count(empty_table(row, column), row, column)
