### Now create custom function for counting events with a category
summarize_count <- function(table, datar, datac)
{
  # Assume no factors
  dr <- datar[1] 
  dc <- datac[1]

  # Test a poisson model
  test <- aov(glm(x ~ treatment,
                  aggregate(dr, by=list(id=dr, treatment=dc), FUN=length),
                  family=poisson)))

  # Compute N values for each category
  subN <- lapply(levels(dc), FUN=function(cat){
    length(unique(dr[dc==cat]))
  })

  # Begin table construction, ala Wickham Style (monadic continuation)
  table                                         %>%
  row_header(derive_label(row))                 %>%  # Name of the row
  col_header("N", levels(dc), "Test Statistic") %>%  # Main header
  col_header(NA,  N(subN),    NA)               %>%  # Subheader is N values
  add_N(length(unique(dr))                      %>%  # First column is N value
  add_each(levels(dc), function(cat)                 # Quantile every category
  {
    x  <- dr[dc == cat]
    xx <- aggregate(x, by=list(x), FUN=length)$x
    quantile(xx, na.rm=TRUE)
  })                                            %>%
  add_col(test)                                      # AOV results
}
