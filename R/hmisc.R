#' @import stringr
#' @include S3-Cell.R


#' Determine if a vector is categorical or not
#'
#' @param x Vector to determine type of
#' @param category_threshold The upper threshold of unique values for which a vector is considered categorical.
#'
#' @return A Boolean: TRUE / FALSE
#' @export
#'
#' @examples
#'
#' is.categorical(c(1,2,3))
#' is.categorical(factor(c("A","B","C")))
#' is.categorical(factor(c("A","B","B","A")))
#' is.categorical(factor(c(TRUE, FALSE, TRUE, FALSE)))
#'
is.categorical <- function(x, threshold=NA)
{
  is.factor(x) ||
  (!is.na(threshold) && length(unique(x[! is.na(x)])) < threshold)
}

#' Determine if a vector is binomial or not
#'
#' @param x Vector to determine type of
#' @param category_threshold The upper threshold of unique values for which a vector is considered categorical.
#'
#' @return a Boolean: TRUE / FALSE
#' @export
#'
#' @examples
#'
#' is.binomial(c(1,2,3))
#' is.binomial(factor(c("A","B","C")))
#' is.binomial(factor(c("A","B","B","A")))
#' is.binomial(factor(c(TRUE, FALSE, TRUE, FALSE)))
#' is.binomial(c('M', 'F', 'M', 'F'), 10)
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
  x <- x[,1]
  if(is.binomial(x,category_threshold))          "Binomial"
  else if(is.categorical(x,category_threshold))  "Categorical"
  else if(is.numeric(x))                         "Numerical"
  else                   stop(paste("Unsupported class/type - ",class(x), typeof(x)))
}

#' Determine the label of a given AST node. Should have data attached via reduce before calling.
#'
#' @param node Abstract syntax tree node.
#'
#' @return A string with a label for the node
#' @export
#'
#' @examples
#'
#' hmisc_data_type(c(1,2,3))
#' hmisc_data_type(factor(c("A","B","C")))
#' hmisc_data_type(factor(c("A","B","B","A")))
#' hmisc_data_type(factor(c(TRUE, FALSE, TRUE, FALSE)))
#'
derive_label <- function(node)
{
  l <- node$string()
  try({
        l2 <- label(node$data, units=FALSE)
        if(nchar(l2)>0) {l<-l2}
  })

  # Find units if they exist
  x <- strsplit(l, "\\s+\\(")[[1]]
  l <- x[1]

  units <- NA
  if(length(x) > 1) units <- strsplit(x[2], "\\)")[1]
  try({
        u2 <- units(node$data)
        if(nchar(u2)>0) {units<-u2}
  })

  tg_label(l, units)
}

summarize_kruskal_horz <- function(row, column)
{
  datar <- row$data[,1]
  datac <- column$data[,1]

  if(!inherits(datac, "factor"))
  {
    lbl_c <- label(datac)
    datac <- factor(datac, levels=unique(datac[!is.na(datac)]))
    label(datac) <- lbl_c
  }

  categories <- levels(datac)
  if (is.null(categories)) {categories <- unique(datar)}

  # 1 X (n + no. categories + test statistic)
  tbl <- tg_table(1, length(categories) + 2, TRUE)

  # Label for the table cell
  row_lbl <- derive_label(row)
  col_lbl <- tg_table(2, 2+length(categories))
  col_lbl[[1]][[1]] <- tg_header("N")
  col_lbl[[1]][[length(categories)+2]] <- tg_header("Test Statistic")

  # N value
  N <- sum(!is.na(datar))
  tbl[[1]][[1]] <- tg_label(as.character(N),
    src=paste(row$value, ":", column$value,":N",sep=''))

  # The quantiles by category
  sapply(1:length(categories), FUN=function(category) {
    x <- datar[datac == categories[category]]
    tbl[[1]][[category+1]] <<- tg_quantile(quantile(x, na.rm=TRUE),
        src=paste(row$value, ":", column$value,"[",categories[category],"]",sep=''))
    col_lbl[[1]][[category+1]] <<- tg_header(categories[category])
    col_lbl[[2]][[category+1]] <<- tg_subheader(paste("N=",sum(!is.na(x)),sep=''),
        src=paste(row$value, ":", column$value,"[",categories[category],"]",":N",sep=''))
  })

  # Kruskal-Wallis via F-distribution
  test <- spearman2(datac, datar, na.action=na.retain)

  tbl[[1]][[length(categories)+2]] <-
    tg_fstat(test['F'], test['df1'], test['df2'], test['P'],
      src=paste(row$value, ":", column$value,":KruskalWallis",sep=''))

  attr(tbl, "row_label") <- row_lbl
  attr(tbl, "col_label") <- col_lbl

  tbl
}

summarize_kruskal_vert <- function(row, column)
{
  if(!inherits(datar, "factor"))
  {
    lbl_r <- label(datar)
    datar <- factor(datar, levels=unique(datar[!is.na(datar)]))
    label(datar) <- lbl_r
  }

  categories <- levels(datar)
  if (is.null(categories)) {unique(datar)}

  # Label for the table cell
  col_lbl <- tg_table(1, 3)
  row_lbl <- tg_table(length(categories), 1)

  col_lbl[[1]][[1]] <- tg_header("N")
  col_lbl[[1]][[2]] <- derive_label(column)
  col_lbl[[1]][[3]] <- tg_header("Test Statistic")

  tbl <- tg_table(length(categories), 3, TRUE) # no. categories X 3

  # The quantiles by category
  sapply(1:length(categories), FUN=function(category) {
    x <- datac[datar == categories[category]]
    tbl[[category]][[1]] <<- tg_label(as.character(length(x)),
      src=paste(row$value, ":", column$value,"[",categories[category],"]",":N",sep=''))
    tbl[[category]][[2]] <<- tg_quantile(quantile(x, na.rm=TRUE),
      src=paste(row$value, ":", column$value,"[",categories[category],"]",sep=''))
    row_lbl[[category]][[1]] <<- tg_label(category)
  })

  # Kruskal-Wallis via F-distribution
  test <- spearman2(datar, datac, na.action=na.retain)

  tbl[[1]][[3]] <- tg_fstat(round(test['F'],2), test['df1'], test['df2'], round(test['P'],3),
      src=paste(row$value, ":", column$value,":KruskalWallis",sep=''))

  attr(tbl, "row_label") <- row_lbl
  attr(tbl, "col_label") <- col_lbl

  tbl
}

summarize_chisq <- function(row, column)
{
  datar <- row$data[,1]
  datac <- column$data[,1]

  if(!inherits(datar, "factor"))
  {
    lbl_r <- label(datar)
    datar <- factor(datar, levels=unique(datar[!is.na(datar)]))
    label(datar) <- lbl_r
  }

  if(!inherits(datac, "factor"))
  {
    lbl_c <- label(datac)
    datac <- factor(datac, levels=unique(datac[!is.na(datac)]))
    label(datac) <- lbl_c
  }

  row_categories <- levels(datar)
  if (is.null(row_categories)) {unique(datar)}

  col_categories <- levels(datac)
  if (is.null(col_categories)) {unique(datac)}

  n <- length(row_categories)
  m <- length(col_categories)

  # Label for the table cell
  row_lbl <- tg_table(length(row_categories), 1)
  row_lbl[[1]][[1]] <- derive_label(row)
  row_lbl[[1]][[1]]$label <- paste(row_lbl[[1]][[1]]$label,":", row_categories[1])
  sapply(2:length(row_categories), FUN=function(level){
    row_lbl[[level]][[1]] <<- tg_label(paste("  ", row_categories[level]))
  })
  col_lbl <- tg_table(2, 2+length(col_categories))
  col_lbl[[1]][[1]] <- tg_header("N")
  col_lbl[[1]][[length(col_categories)+2]] <- tg_header("Test Statistic")

  # N X (M+2)
  tbl <- tg_table(n, m+2, TRUE)

  N <- sum(!is.na(datar) & !is.na(datac))

  tbl[[1]][[1]] <- tg_label(as.character(N),
    src=paste(row$value, ":", column$value,":N",sep=''))

  # The fractions by category intersection
  sapply(1:length(col_categories), FUN=function(col_category) {
    c_x <- datac[datac == col_categories[col_category]]
    c_x <- c_x[!is.na(c_x)]
    denominator <- length(c_x)
    sapply(1:length(row_categories), FUN=function(row_category) {
      c_xy <- datac[datac == col_categories[col_category] &
                    datar == row_categories[row_category]]
      c_xy <- c_xy[!is.na(c_xy)]
      numerator <- length(c_xy)
      if(numerator > 0)
      {
        tbl[[row_category]][[col_category+1]] <<- tg_fraction(numerator, denominator,
          src=paste(row$value,"[",row_categories[row_category],"]:",column$value,"[",col_categories[col_category],"]", sep=''))
      }
    })
    col_lbl[[1]][[col_category+1]] <<- tg_header(col_categories[col_category])
    col_lbl[[2]][[col_category+1]] <<- tg_subheader(paste("N=",sum(!is.na(c_x)),sep=''),
      src=paste(row$value, ":", column$value,"[",col_categories[col_category],"]",":N",sep=''))
  })

  y <- table(datar,datac, useNA="no")
  y <- y[,which(!apply(y,2,FUN = function(x){all(x == 0)}))]
  y <- y[which(!apply(y,1,FUN = function(x){all(x == 0)})),]

  test <- chisq.test(y, correct=FALSE)

  tbl[[1]][[m+2]] <- tg_chi2(test$statistic, test$parameter, test$p.value,
    src=paste(row$value, ":", column$value,":Chi^2",sep=''))

  # Throw out first if length is 2
  if(length(tbl) == 2)
  {
    tbl[[2]][[m+2]] <- tbl[[1]][[m+2]]
    tbl[[2]][[1]]   <- tbl[[1]][[1]]
    tbl[[1]]        <- tbl[[2]]
    tbl[[2]]        <- NULL

    # Redo labeling as well
    row_lbl[[2]]      <- NULL
    row_lbl[[1]][[1]] <- derive_label(row)
    row_lbl[[1]][[1]] <- tg_label(paste(row_lbl[[1]][[1]]$label,":", row_categories[2]))
  }

  attr(tbl, "row_label") <- row_lbl
  attr(tbl, "col_label") <- col_lbl

  tbl
}

summarize_spearman <- function(row, column)
{
  tbl <- tg_table(1, 3, TRUE)

  datar <- row$data[,1]
  datac <- column$data[,1]

  # Label for the table cell
  col_lbl <- tg_table(2, 3)
  col_lbl[[1]][[1]] <- tg_header("N")
  col_lbl[[1]][[2]] <- derive_label(column)
  col_lbl[[1]][[3]] <- tg_header("Test Statistic")
  col_lbl[[2]][[1]] <- tg_subheader("")
  col_lbl[[2]][[2]] <- tg_subheader("")
  col_lbl[[2]][[3]] <- tg_subheader("")

  row_lbl <- derive_label(row)

  test <- cor.test(datar, datac, alternate="two.sided", method="spearman", na.action=na.omit, exact=FALSE)

  n <- sum(!is.na(datar) & !is.na(datac))

  tbl[[1]][[1]] <- tg_label(as.character(n),
    src=paste(row$value, ":", column$value,":N",sep=''))

  tbl[[1]][[2]] <- tg_estimate(test$estimate,
    src=paste(row$value, ":", column$value,sep=''))

  # Reversed engineered from cor.test for spearman
  r <- test$estimate
  statistic <- r/sqrt((1 - r^2)/(n - 2))

  tbl[[1]][[3]] <- tg_studentt(statistic, n-2, test$p.value,
    src=paste(row$value, ":", column$value,":ttest",sep=''))

  attr(tbl, "row_label") <- row_lbl
  attr(tbl, "col_label") <- col_lbl
  tbl
}

apply_factors <- function(row, column)
{
  stop("Not Implemented")
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
                  Binomial    = summarize_kruskal_horz,
                  Factors     = apply_factors
            ),
  Categorical = list(
                  Numerical   = summarize_kruskal_vert,
                  Categorical = summarize_chisq,
                  Binomial    = summarize_chisq,
                  Factors     = apply_factors
            ),
  Binomial    = list(
                  Numerical   = summarize_kruskal_vert,
                  Categorical = summarize_chisq,
                  Binomial    = summarize_chisq,
                  Factors     = apply_factors
            ),
  Factors     = list(
                  Numerical   = apply_factors,
                  Categorical = apply_factors,
                  Binomial    = apply_factors,
                  Factors     = apply_factors
            )
)
