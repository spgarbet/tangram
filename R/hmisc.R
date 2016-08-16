#' @import stringr
#' @include S3-Cell.R

summarize_ordinal_lr <- function(data, row, column)
{
  tg_table(1, 1, TRUE)
}

is.categorical <- function(x, threshold=NA)
{
  is.factor(x) ||
  (!is.na(threshold) && length(unique(x[! is.na(x)])) < threshold)
}

is.binomial <- function(x, threshold=NA)
{
  (is.factor(x) && length(levels(x)) == 2) ||
  (!is.na(threshold) && length(unique(x[! is.na(x)])) == 2)
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
  if(is.binomial(x,category_threshold))          "Binomial"
  else if(is.categorical(x,category_threshold))  "Categorical"
  else if(is.numeric(x))                         "Numerical"
  else                   stop(paste("Unsupported class/type - ",class(x), typeof(x)))
}

derive_label <- function(data, column)
{
  l <- column
  try({
        l2 <- label(data[column])
        if(nchar(l2)>0) {l<-l2}
  })

  # Find units if they exist
  x <- strsplit(l, "\\s*\\(")

  if(length(x[[1]]) <= 1)
  {
    tg_label(l)
  }
  else
  {
    tg_label(x[[1]][1], strsplit(x[[1]][2], "\\)")[[1]][1])
  }
}

summarize_kruskal_horz <- function(data, row, column)
{
  categories <- levels(data[,column])
  if (is.null(categories)) {unique(data[,row])}

  # 1 X (n + no. categories + test statistic)
  tbl <- tg_table(1, length(categories) + 2, TRUE)

  # Label for the table cell
  row_lbl <- derive_label(data, row)
  col_lbl <- tg_table(2, 2+length(categories))
  col_lbl[[1]][[1]] <- tg_header("N")
  col_lbl[[1]][[length(categories)+2]] <- tg_header("Test Statistic")

  # N value
  N <- sum(!is.na(data[,row]))
  tbl[[1]][[1]] <- tg_label(as.character(N))

  # The quantiles by category
  sapply(1:length(categories), FUN=function(category) {
    x <- data[data[,column] == categories[category], row]
    tbl[[1]][[category+1]] <<- tg_quantile(quantile(x, na.rm=TRUE))
    col_lbl[[1]][[category+1]] <<- tg_header(categories[category])
    col_lbl[[2]][[category+1]] <<- tg_subheader(paste("N=",sum(!is.na(x)),sep=''))
  })

  # Kruskal-Wallis via F-distribution
  test <- spearman2(data[,column], data[,row], na.action=na.retain)

  tbl[[1]][[length(categories)+2]] <- tg_fstat(test['F'], test['df1'], test['df2'], test['P'])

  attr(tbl, "row_label") <- row_lbl
  attr(tbl, "col_label") <- col_lbl

  tbl
}

summarize_kruskal_vert <- function(data, row, column)
{
  categories <- levels(data[,row])
  if (is.null(categories)) {unique(data[,row])}

  # Label for the table cell
  col_lbl <- tg_table(1, 3)
  row_lbl <- tg_table(length(categories), 1)

  col_lbl[[1]][[1]] <- tg_header("N")
  col_lbl[[1]][[2]] <- derive_label(data, column)
  col_lbl[[1]][[3]] <- tg_header("Test Statistic")

  tbl <- tg_table(length(categories), 3, TRUE) # no. categories X 3

  # The quantiles by category
  sapply(1:length(categories), FUN=function(category) {
    x <- data[data[,row] == categories[category], column]
    tbl[[category]][[1]] <<- tg_label(as.character(length(x)))
    tbl[[category]][[2]] <<- tg_quantile(quantile(x, na.rm=TRUE))
    row_lbl[[category]][[1]] <<- tg_label(category)
  })

  # Kruskal-Wallis via F-distribution
  test <- spearman2(data[,row], data[,column], na.action=na.retain)

  tbl[[1]][[3]] <- tg_fstat(test['F'], test['df1'], test['df2'], test['P'])

  attr(tbl, "row_label") <- row_lbl
  attr(tbl, "col_label") <- col_lbl

  tbl
}

summarize_chisq <- function(data, row, column)
{
  row_categories <- levels(data[,row])
  if (is.null(row_categories)) {unique(data[,row])}

  col_categories <- levels(data[,column])
  if (is.null(col_categories)) {unique(data[,column])}

  n <- length(row_categories)
  m <- length(col_categories)

  # Label for the table cell
  row_lbl <- tg_table(length(row_categories), 1)
  row_lbl[[1]][[1]] <- derive_label(data, row)
  row_lbl[[1]][[1]]$label <- paste(row_lbl[[1]][[1]]$label,":", row_categories[1])
  sapply(2:length(row_categories), FUN=function(level){
    row_lbl[[level]][[1]] <<- tg_label(paste("  ", row_categories[level]))
  })
  col_lbl <- tg_table(2, 2+length(col_categories))
  col_lbl[[1]][[1]] <- tg_header("N")
  col_lbl[[1]][[length(col_categories)+2]] <- tg_header("Test Statistic")

  # N X (M+2)
  tbl <- tg_table(n, m+2, TRUE)

  N <- length(data[!is.na(data[,row]) & !is.na(data[,column]),row])
  tbl[[1]][[1]] <- tg_label(as.character(N))

  # The fractions by category intersection
  sapply(1:length(col_categories), FUN=function(col_category) {
    c_x <- data[data[,column] == col_categories[col_category], column]
    c_x <- c_x[!is.na(c_x)]
    denominator <- length(c_x)
    sapply(1:length(row_categories), FUN=function(row_category) {
      c_xy <- data[data[,column] == col_categories[col_category] &
                   data[,row]    == row_categories[row_category], column]
      c_xy <- c_xy[!is.na(c_xy)]
      numerator <- length(c_xy)
      if(numerator > 0)
      {
        tbl[[row_category]][[col_category+1]] <<- tg_fraction(numerator, denominator)
      }
    })
    col_lbl[[1]][[col_category+1]] <<- tg_header(col_categories[col_category])
    col_lbl[[2]][[col_category+1]] <<- tg_subheader(paste("N=",sum(!is.na(c_x)),sep=''))
  })

  y <- table(data[,row],data[,column], useNA="no")
  y <- y[,which(!apply(y,2,FUN = function(x){all(x == 0)}))]
  y <- y[which(!apply(y,1,FUN = function(x){all(x == 0)})),]

  test <- chisq.test(y, correct=FALSE)

  tbl[[1]][[m+2]] <- tg_chi2(test$statistic, test$parameter, test$p.value)

  # Throw out first if length is 2
  if(length(tbl) == 2)
  {
    tbl[[2]][[m+2]] <- tbl[[1]][[m+2]]
    tbl[[2]][[1]]   <- tbl[[1]][[1]]
    tbl[[1]]        <- tbl[[2]]
    tbl[[2]]        <- NULL

    # Redo labeling as well
    row_lbl[[2]]      <- NULL
    row_lbl[[1]][[1]] <- derive_label(data, row)
    row_lbl[[1]][[1]] <- tg_label(paste(row_lbl[[1]][[1]]$label,":", row_categories[2]))
  }

  attr(tbl, "row_label") <- row_lbl
  attr(tbl, "col_label") <- col_lbl

  tbl
}

summarize_spearman <- function(data, row, column)
{
  tbl <- tg_table(1, 3, TRUE)

  # Label for the table cell
  col_lbl <- tg_table(1, 3)
  col_lbl[[1]][[1]] <- tg_header("N")
  col_lbl[[1]][[2]] <- derive_label(data, column)
  col_lbl[[1]][[3]] <- tg_header("Test Statistic")

  row_lbl <- derive_label(data, row)

  # FIXME? MAYBE, this should use pvrank if it can
  test <- cor.test(data[,row], data[,column], alternate="two.sided", method="spearman", na.action=na.omit, exact=FALSE)

  n <- length(data[!is.na(data[,row]) & !is.na(data[,column])  ,row])

  tbl[[1]][[1]] <- tg_label(as.character(n))

  tbl[[1]][[2]] <- tg_estimate(test$estimate)

  # Reversed engineered from cor.test for spearman
  r <- test$estimate
  statistic <- r/sqrt((1 - r^2)/(n - 2))

  tbl[[1]][[3]] <- tg_studentt(statistic, n-2, test$p.value)

  attr(tbl, "row_label") <- row_lbl
  attr(tbl, "col_label") <- col_lbl
  tbl
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
                  Binomial    = summarize_kruskal_horz
            ),
  Categorical = list(
                  Numerical   = summarize_kruskal_vert,
                  Categorical = summarize_chisq,
                  Binomial    = summarize_chisq
            ),
  Binomial    = list(
                  Numerical   = summarize_kruskal_vert,
                  Categorical = summarize_chisq,
                  Binomial    = summarize_chisq
            )
)
