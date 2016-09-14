# Flip list of lists

# Special flatten list, that doesn't flatten tg_cells
tg_flatten <- function(ls)
{
  flat <- list()
  el   <- 1

  for(a in ls)
  {
    if(inherits(a, "tg_cell"))
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
  list(nrow=1, ncol=0, table=tg_table(1,1), row=row, col=column)
}

#' @export
tg <- function(x, row, column, ...)
{
  UseMethod("tg", x)
}

tg.default <- function(x, row, column, ...)
{
  tg_label(as.character(x))
}

tg.aov <- function(model, row, column, ...)
{
  test <- summary(model)[[1]]
  tg_fstat(f   = tg_format("%.2f", test$'F value'[1]),
           n1  = test$Df[1],
           n2  = test$Df[2],
           p   = tg_format("%1.3f", test$'Pr(>F)'[1]),
           src = key(row, column, "aov"))
}


#' Internal help to prepare header
# prepare_header <- function(value, additional_class = "tg_header")
# {
#   # Convert to list of lists if not already
#   # if(!inherits(value, "list"))
#   # {
#   #   value <- if(inherits(value, "tg_cell"))
#   #   {
#   #     list(list(value))
#   #   } else
#   #   {
#   #     lapply(as.list(value), FUN=as.list)
#   #   }
#   # }
#   # if(!inherits(value[[1]], "list"))
#   # {
#   #   value <- lapply(value, FUN=as.list)
#   # }
#
#   value <- lapply(value, FUN=function(row){
#     lapply(row, FUN=function(col){
#       col <- if(inherits(col, "tg_cell"))
#       {
#         col
#       } else if(is.na(col))
#       {
#         tg_cell()
#       } else
#       {
#         tg_label(col)
#       }
#       attr(col,"class") <- c(additional_class, attr(col,"class"))
#       col
#     })
#   })
#
#   attr(value, "class")    <- c("tg_table", "tg_cell")
#   attr(value, "embedded") <- FALSE
#
#   value
# }
#
# build_header <- function(table_builder, attribute, ...)
# {
#   label <- if(length(list(...)) < 2) {list(...)[[1]]} else {c(...)}
#   old_header <- attr(table_builder$table, attribute)
#
#   attr(table_builder$table, attribute) <- if(is.null(old_header))
#   {
#     prepare_header(label)
#   } else {
#     old_header[[length(old_header)+1]] <- prepare_header(label, c("tg_subheader", "tg_header"))
#   }
#   table_builder
# }
#row_header <- function(table_builder, ...) build_header(table_builder, "row_header", ...)
#col_header <- function(table_builder, ...) build_header(table_builder, "col_header", ...)

prepare_header_row <- function(row, col, additional_class, ...)
{
  # Get flattened args list
  flat <- tg_flatten(list(...))

  # Convert every element to an appropriate tg_cell
  lapply(flat, FUN=function(cell){
    value <- if(inherits(cell, "tg_cell")) cell               else
             if(is.na(cell))               tg_cell()          else
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
                                       c("tg_header"),
                                       c("tg_subheader", "tg_header")),
                                ...)

  attr(table_builder$table, attribute) <- if(is.null(old_hdr))
  {
    header <- list(new_hdr)
    attr(header, "class")    <- c("tg_table", "tg_cell")
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
  table_builder$table[[table_builder$nrow]][[table_builder$ncol]] <- tg_n(n, src=key(row, col, "N"))
  table_builder
}

add_each <- function(table_builder, values, func)
{
  sapply(values, FUN=function(x) {
    table_builder <<- add_col(func(x))
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

row <- ast$right
column <- ast$left
table <- empty_table(row,column)


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
  table <- empty_table(ast$right, ast$left)   # DELETE ME
  table <-                                    # DELETE ME

  table                                         %>%
  row_header(derive_label(row))                 %>%  # Name of the row
  col_header("N", levels(dc), "Test Statistic") %>%  # Main header
  col_header(NA,  N(subN),    NA)               %>%  # Subheader is N values  ## FIXME WHAT ABOUT SUBCOL in key
  add_N(length(unique(dr)))                     %>%  # First column is N value
  add_each(levels(dc), function(cat)                 # Quantile every category
  {
    x  <- dr[dc == cat]
    xx <- aggregate(x, by=list(x), FUN=length)$x
    quantile(xx, na.rm=TRUE)
  })                                            %>%
  add_col(test)                                      # AOV results
}
