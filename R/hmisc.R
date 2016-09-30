library(stringr)
library(Hmisc)

#' @import stringr
#' @import Hmisc
#' @include S3-Cell.R
#' @include typing.R

# 1 X (n + no. categories + test statistic)
#' @export
summarize_kruskal_horz <- function(table, row, column)
{
  datar      <- row$data[,1]
  datac      <- as.categorical(column$data[,1])
  categories <- levels(datac)

  # Compute N values for each category
  subN <- lapply(levels(datac), FUN=function(cat){
    length(datac[datac == cat & !is.na(datac)])
  })

  # Kruskal-Wallis via F-distribution
  test <- spearman2(datac, datar, na.action=na.retain)
  fstat <- cell_fstat(f   = cell_format("%.2f", test['F']),
                      n1  = test['df1'],
                      n2  = test['df2'],
                      p   = cell_format("%1.3f", test['P']),
                      src = key(row, column, "F"))

  table                                          %>%
  row_header(derive_label(row))                  %>%
  col_header("N", categories, "Test Statistic")  %>%
  col_header("",  tg_N(subN), ""               ) %>%
  add_col(tg_N(sum(!is.na(datar))))              %>%
  table_builder_apply(categories, function(tbl, category) {
     x <- datar[datac == category]

     tbl %>% add_col(tg_quantile(x, row$format, na.rm=TRUE), subcol=category)
  })                                             %>%
  add_col(fstat)
}

# no. categories X 3
#' @export
summarize_kruskal_vert <- function(table, row, column)
{
  datar      <- as.categorical(row$data[,1])
  datac      <- column$data[,1]
  categories <- levels(datar)

  # Kruskal-Wallis via F-distribution
  test <- spearman2(datar, datac, na.action=na.retain)
  fstat <- cell_fstat(f   = cell_format("%.2f", test['F']),
                      n1  = test['df1'],
                      n2  = test['df2'],
                      p   = cell_format("%1.3f", test['P']),
                      src = key(row, column, "F"))

  table                                                             %>%
  col_header("N", derive_label(column), "Test Statistic")           %>%
  table_builder_apply(categories, FUN=function(tbl, category) {
    x <- datac[datar == categories[category]]
    tbl                                                  %>%
    row_header(category)                                 %>%
    add_col(tg_N(length(x)))                             %>%
    add_col(tg_quantile(x, column$format, na.rm=TRUE), subrow=category) %>%
    new_line()
  })                                                                %>%
  cursor_pos(1, 3)                                                  %>%
  add_col(fstat)
}

# N X (M+2)
#' @export
summarize_chisq <- function(table, row, column)
{
  datar          <- as.categorical(row$data[,1])
  datac          <- as.categorical(column$data[,1])

  row_categories <- levels(datar)
  col_categories <- levels(datac)

  # If it's binomial, then don't display the first value
  if(length(row_categories) == 2) {row_categories <- last(row_categories)}

  # Compute N values for each category
  subN <- lapply(levels(datac), FUN=function(cat){
    length(datac[datac == cat & !is.na(datac)])
  })

  # Chi^2 test
  y    <- table(datar,datac, useNA="no")
  y    <- y[,which(!apply(y,2,FUN = function(x){all(x == 0)}))]
  y    <- y[which(!apply(y,1,FUN = function(x){all(x == 0)})),]
  test <- chisq.test(y, correct=FALSE)

  # First row label is different
  first_row_lbl <- derive_label(row)
  first_row_lbl$label <- paste(first_row_lbl$label,":", row_categories[1])

  labels      <- lapply(row_categories, FUN=function(x) paste("  ", x))
  labels[[1]] <- first_row_lbl

  # Now construct the table by add rows to each column
  table                                                      %>%
  col_header("N", col_categories, "Test Statistic")          %>%
  col_header("", tg_N(subN), "")                             %>%
  table_builder_apply(labels, FUN=
    function(tbl, row_name) {tbl %>% row_header(row_name)})  %>%
  add_col(tg_N(sum(!is.na(datar) & !is.na(datac))))          %>%
  table_builder_apply(col_categories, FUN=function(table, col_category) {
    denominator <- length(datac[datac == col_category & !is.na(datac)])

    table_builder_apply(table, row_categories, FUN=
      function(table, row_category) {
          numerator <- length(datac[datac == col_category &
                                    datar == row_category &
                                    !is.na(datac)])
          add_row(table,
                  tg_fraction(numerator, denominator),
                  subcol=col_category, subrow=row_category)
      }) %>%
    new_col()
  })                                                         %>%
  add_row(test,rep("", length(row_categories)-1))
}

# 1 X 3
#' @export
summarize_spearman <- function(table, row, column)
{
  datar <- row$data[,1]
  datac <- column$data[,1]

  test  <- cor.test(datar, datac, alternate="two.sided", method="spearman", na.action=na.omit, exact=FALSE)

  table %>%
  row_header(derive_label(row)) %>%
  col_header("N", derive_label(column), "Test Statistic") %>%
  col_header("", "", "") %>%
  add_col(tg_N(sum(!is.na(datar) & !is.na(datac)))) %>%
  add_col(round(test$estimate,2)) %>%
  add_col(test)
}

apply_factors <- function(row, column)
{
  stop("Not Implemented")
}

#' Determine data type of a vector loosely consistent with Hmisc.
#'
#' @param x Vector to determine type of
#' @param category_threshold The upper threshold of unique values for which a vector is considered categorical.
#'
#' @return One of the following strings: Binomial, Categorical, or Numerical.
#' @export
#'
#' @examples
#'
#' hmisc_data_type(c(1,2,3))
#' hmisc_data_type(factor(c("A","B","C")))
#' hmisc_data_type(factor(c("A","B","B","A")))
#' hmisc_data_type(factor(c(TRUE, FALSE, TRUE, FALSE)))
#'
hmisc_data_type <- function(x, category_threshold=NA)
{
  x <- x[,1] # FIXME: Need to deal with factors
  if(is.categorical(x,category_threshold))  "Categorical" else
  if(is.numeric(x))                         "Numerical"   else
  stop(paste("Unsupported class/type - ",class(x), typeof(x)))
}

#'
#' Style Bundle for Hmisc defaults.
#'
#' List of lists, should contain a "Type" entry with a function to determine type of vector passed in.
#' Next entries are keyed off returned types from function, and represent the type of a row.
#' The returned list should contain the same list of types, and represents the type of a column. Thus it now returns
#' a function to process the intersection of those two types.
#'
#' @keywords data
#' @export
#'
hmisc_style <- list(
  Type        = hmisc_data_type,
  Numerical   = list(
                  Numerical   = summarize_spearman,
                  Categorical = summarize_kruskal_horz,
                  Factors     = apply_factors
            ),
  Categorical = list(
                  Numerical   = summarize_kruskal_vert,
                  Categorical = summarize_chisq,
                  Factors     = apply_factors
            ),
  Factors     = list(
                  Numerical   = apply_factors,
                  Categorical = apply_factors,
                  Factors     = apply_factors
            )
)
