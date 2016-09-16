# Flip list of lists


### Now create custom function for counting events with a category
n  <- 1000
df <- data.frame(id = sample(1:250, n*3, replace=TRUE), event = as.factor(rep(c("A", "B","C"), n)))
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



ast    <- Parser$new()$run(event ~ id)$reduce(df)
row    <- ast$right
column <- ast$left
table  <- summarize_count(empty_table(row, column), row, column)
