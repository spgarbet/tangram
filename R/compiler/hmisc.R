library(stringr)

source("tableGrammar/parser.R")
source("tableGrammar/S3-Cell.R")
source("tableGrammar/summary.R")

## Default Summary Functions

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

summarize_ordinal_lr <- function(data, row, column)
{
  tg_table(1, 1, TRUE)
}


# Data type tests
is.wholenumber <- function(x, tol=.Machine$double.eps^0.5)
{
  abs(x - round(x)) < tol
}

is.count <- function(x, tol=.Machine$double.eps^0.5)
{
  all(is.wholenumber(x, tol) & x >=0, na.rm=TRUE)
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


# Used to determine "type" of data for transform
data_type <- function(x, category_threshold=NA)
{
  if     (is.ordered(x))                         "ordinal"
  else if(is.binomial(x,category_threshold))     "binomial"
  else if(is.categorical(x,category_threshold))  "categorical"
  else if(is.count(x))                           "count"
  else if(is.numeric(x))                         "continuous"
  else                   stop(paste("Unsupported class/type - ",class(x), typeof(x)))
}

# Top  level list is row "class"
# Next level list is column "class"
# TODO: ordered needs to be made sensible


transformDefaults = list(
  continuous  = list(
                  continuous  = summarize_spearman,
                  categorical = summarize_kruskal_horz,
                  binomial    = summarize_kruskal_horz,
                  ordinal     = summarize_ordinal_lr
            ),
  categorical = list(
                  continuous  = summarize_kruskal_vert,
                  categorical = summarize_chisq,
                  binomial    = summarize_chisq,
                  ordinal     = summarize_ordinal_lr
            ),
  binomial    = list(
                  continuous  = summarize_kruskal_vert,
                  categorical = summarize_chisq,
                  binomial    = summarize_chisq,
                  ordinal     = summarize_ordinal_lr
            ),
  ordinal     = list(
                  continuous  = summarize_kruskal_vert,
                  categorical = summarize_chisq,
                  binomial    = summarize_chisq,
                  ordinal     = summarize_ordinal_lr
            )
)

tg_flatten <- function(table)
{
  # Compute final size of table
  final_rows    <- 0
  final_cols    <- 0
  sapply(1:rows(table), FUN=function(row) {
    element <- table[[row]][[1]]
    if(inherits(element, "tg_table") && attr(element, "embedded"))
      final_rows <<- final_rows + length(element)
    else
      final_rows <<- final_rows + 1
  })
  sapply(1:cols(table), FUN=function(col){
    element <- table[[1]][[col]]
    if(inherits(element, "tg_table") && attr(element, "embedded"))
      final_cols <<- final_cols + length(element[[1]])
    else
      final_cols <<- final_cols + 1
  })

  
  # Grab labels
  row_label <- attr(table[[1]][[1]], "row_label")
  col_label <- attr(table[[1]][[1]], "col_label") 
  
  # Set aside additional for labeling
  label_rows <- rows(col_label)
  label_cols <- cols(row_label)
  
  # Allocate final table
  new_tbl <- tg_table(final_rows+label_rows, final_cols+label_cols) 
  
  # Fill in row labels
  output_row <- label_rows + 1
  sapply(1:rows(table), FUN=function(row){
    rlabel <- attr(table[[row]][[1]], "row_label") # Only take row labels from column 1
    
    if(inherits(rlabel, "tg_table")) {
      sapply(1:rows(rlabel), FUN=function(inner_row) {
        sapply(1:cols(rlabel), FUN=function(inner_col) {
          new_tbl[[output_row]][[inner_col]] <<- rlabel[[inner_row]][[inner_col]]
        })
        output_row <<- output_row + 1
      })
    }
    else
    {
      new_tbl[[output_row]][[1]] <<- rlabel
      output_row <<- output_row + 1
    }
  })
  
  # Fill in col labels
  output_col <- label_cols + 1
  sapply(1:cols(table), FUN=function(col){
    rlabel <- attr(table[[1]][[col]], "col_label") # Only take col labels from row 1
    
    if(inherits(rlabel, "tg_table")) {
      sapply(1:cols(rlabel), FUN=function(inner_col) {
        sapply(1:rows(rlabel), FUN=function(inner_row) {
          new_tbl[[inner_row]][[output_col]] <<- rlabel[[inner_row]][[inner_col]]
        })
        output_col <<- output_col + 1
      })
    }
    else
    {
      new_tbl[[1]][[output_col]] <<- rlabel   
      output_col <<- output_col + 1
    }
  })
  
  # Main loop to fill final from provided
  output_row <- label_rows + 1
  sapply(1:rows(table), FUN=function(row) {
    output_col <- label_cols + 1
    sapply(1:cols(table), FUN=function(col) {
      element <- table[[row]][[col]]
      
      if(inherits(element, "tg_table") && attr(element, "embedded"))
      {
        ## Need another double sapply here.
        sapply(element, FUN=function(inner_row)
        {
          sapply(inner_row, FUN=function(inner_element)
          {
            new_tbl[[output_row]][[output_col]] <<- inner_element
            output_col <<- output_col + 1
          })
          output_col <<- output_col - length(inner_row)
          output_row <<- output_row + 1
        })
        output_row <<- output_row - length(element)
      }
      else
      {
        new_tbl[[output_row]][[output_col]] <<- element
      }
      output_col <<- output_col + length(element[[1]])
      
    })
    output_row <<- output_row + length(table[[row]][[1]])
  })
  
  new_tbl
}

tg_create_table <- function(ast, data, transforms, data_type_fun)
{
  elements <- ast$elements()

  width  <- length(elements[[1]])
  height <- length(elements[[2]])
  tbl    <- tg_table(height, width)

  sapply(1:width, FUN=function(col_idx) {
    column <- elements[[1]][col_idx]
    
    sapply(1:height, FUN=function(row_idx) {
      row <- elements[[2]][row_idx]
      
      rowtype <- data_type_fun(data[,row])
      coltype <- data_type_fun(data[,column])

      transform <- transforms[[rowtype]][[coltype]]

      tbl[[row_idx]][[col_idx]] <<- transform(data, row, column)
    })
  })
  
  tg_flatten(tbl)
  #tbl
}

tg_summary <- function(formula, data, transforms=transformDefaults, data_type_fun=data_type)
{
  ast <- Parser$new()$run(formula)
  
  tg_create_table(ast, data, transforms, data_type_fun)
}

### function to round values to N significant digits
# input:   vec       vector of numeric
#          n         integer is the required sigfig  
# output:  outvec    vector of numeric rounded to N sigfig
sigfig <- function(vec, n=3)
{ 
  formatC(signif(vec,digits=n), digits=n, format="g", flag="#") 
}   

roundfig <- function(vec, n=3)
{ 
  formatC(round(vec,digits=n), digits=n, format="f", flag="#") 
}  



