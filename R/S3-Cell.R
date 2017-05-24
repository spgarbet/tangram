# Plan of attack
# 1) rows/cols ? why is this needed. Can it be chucked overboard?
#    Used only on cell_table right now. Should only be used for that.
# 2) render_f / formatting moved to construction. Stop duplicating in render code.
# 3) cell_table object is still fine
# 4) 

rows <- function(x)
{
  UseMethod("rows", x)
}

cols <- function(x)
{
  UseMethod("cols", x)
}

render_f <- function(x, format=NA)
{
  if(is.null(format) || is.na(format)) format <- attr(x, "format")
  if(is.null(format) || is.na(format)) format <- 3
  if(is.character(format) && substr(format, 1, 1) != "%") format <- as.numeric(format)

  result <- if(is.numeric(format))
  {
    sprintf(paste("%1.", format, "f", sep=""), x)
  }
  else
  {
    sprintf(format, x)
  }
  names(result) <- names(x)
  result
}

cell <- function(contents=NA)
{
  if(inherits(contents,"list"))
  {
    structure(contents, class="cell")
  }
  else if(is.na(contents))
  {
    structure(list(), class="cell")
  }
  else
  {
    print(traceback())
    stop("Improper table cell construction")
  }
}


rows.cell <- function(object)
{
  length(object)
}

cols.cell <- function(object)
{
  if(length(object) >= 1)
  {
    length(object[[1]])
  }
  else
  {
    0
  }
}

#' Create an empty cell_table
#'
#' Create an empty cell_table (S3) object to fill with desired table elements. Note that
#' The initial size is not a limiting factor, the table can grow as needed later.
#'
#' @param rows An integer of the number of rows to create
#' @param cols An integer of the number of cols to create
#' @param embedded A boolean representing whether this table will is to be marked as embedded in another
#'
#' @return An empty cell_table object.
#' @export
cell_table <- function(rows, cols, embedded=TRUE)
{
  nt <- sapply(1:rows, FUN=function(x) list(sapply(1:cols, FUN=function(x) cell())))

  tbl <- structure(cell(nt), class=c("cell_table", "cell") )

  attr(tbl, "embedded") <- embedded

  tbl
}

#' Construct a table cell from an object
#' 
#' Any R object can be used as a cell value. Attributes
#' are used to store additional aspects of that cell
#' attached to the object. This is a helper function
#' to attach all the additional attributes to the 
#' provided object
#' 
#' @param x R object to attach attributes top
#' @param ... Each additional argument becomes an attribute for the object
#' @return The modified R object
cell <- function(x, ...)
{
  aspects <- list(...)
  for(i in names(aspects))
    attr(x, i) <- aspects[[i]]
  x
}

#' Create an cell_label (S3) object of the given text.
#'
#' A cell_label object represents a label cell inside a table. It can
#' also contain units.
#'
#' @param text The text of the label. May include a subset of LaTeX greek or math.
#' @param units An optional field that contains units
#' @param ... optional extra information to attach
#'
#' @return A cell_table object
#' @export
#'
#' @examples
#' cell_label("Compaction Method")
#' cell_label("Concentration", "mg/dl")
#' cell_label("Concentration", "mg/dl", subcol="A")
cell_label <- function(text, units=NA, ...)
{
  cell(as.character(text),
       aspect="cell_label",
       units=as.character(units), ...)
}

#' Create a cell_header object of the given text.
#'
#' A cell_header object represents a label cell inside a table. It can
#' also contain units. 
#'
#' @param text The text of the label. May include a subset of LaTeX greek or math.
#' @param units An optional field that contains units
#' @param ... optional extra information to attach
#'
#' @return A cell_header object
#' @export
#' @examples
#' cell_header("Yahoo")
#' cell_header("Concentration", "mg/dl")
#' cell_header("Concentration", "mg/dl", src="A")
cell_header <- function(text, units=NA, ...)
{
  cell(as.character(text),        aspect=c("cell_header", "cell_label"),
       units=as.character(units), ...)
}

#' Create a cell_subheader object of the given text.
#'
#' A cell_subheader object represents a label cell inside a table. It can
#' also contain units.
#'
#' @param text The text of the label. May include a subset of LaTeX greek or math.
#' @param units An optional field that contains units
#' @param ... optional extra information to attach
#'
#' @return A cell_subheader object.
#' @export
#' @examples
#' cell_subheader("Concentration")
#' cell_subheader("Concentration", "mg/dl")
#' cell_subheader("Concentration", "mg/dl", src="A")
cell_subheader <- function(text, units=NA, ...)
{
  cell(as.character(text),      
       aspect=c("cell_subheader", "cell_header", "cell_label"),
       units=as.character(units), ...)
}

#' Create a interquartile range cell object of the given data
#'
#' Construct a cell which has the 3 interquartile ranges specified. 
#'
#' @param x numeric vector whose sample quantiles are wanted. NA and NaN values are not allowed in numeric vectors unless na.rm is TRUE.
#' @param format numeric or character; Significant digits or fmt to pass to sprintf
#' @param na.rm logical; if true, any NA and NaN's are removed from x before the quantiles are computed.
#' @param names logical; if true, the result has a names attribute. Set to FALSE for speedup with many probs.
#' @param type integer; specify algorithm to use in constructing quantile. See quantile for more information.
#' @param ... additional arguments to constructing cell
#'
#' @return A cell_quantile object.
#' @export
#' @examples
#' require(stats)
#' cell_iqr(rnorm(100), '3')
cell_iqr <- function(x,
                     format = NA,
                     na.rm  = TRUE,
                     names  = FALSE,
                     type   = 8,
                     ...
                          )
{
  x <- quantile(x,  c(0.25, 0.50, 0.75), na.rm, names, type)
  
  if(is.na(format)) format <- format_guess(x)
  ql <- sapply(x, function(x) render_f(x, format))
  cell(ql, aspect="cell_iqr", ...)
}


#' Create an cell_estimate (S3) object of the given estimate
#'
#' A cell_estimate object contains a statistical estimate. It may
#' additionally contain an interval with a low and high of a
#' specified width.
#'
#' @param value The value of the estimate
#' @param low An optional field that specifies a lower interval for the estimate.
#' @param high An optional field that specifies an upper interval for the estimate.
#' @param conf.level An optional field for storing the width of the interval.
#' @param src An optional field for traceability of the source of the field
#'
#' @return A cell_estimate object.
#' @export
#' @examples
#' cell_estimate(1.0)
#' cell_estimate(c(1.0, 0.5, 1.5))
cell_estimate <- function(x, ...)
{
  cell(x, aspects="cell_estimate", ...)
}

#' Create an cell_fstat (S3) object of the given statistic
#'
#' A cell_fstat object contains a statistical result of an F-test.
#'
#' @param f The value of the f-statistic
#' @param n1 1st dimension degrees of freedom
#' @param n2 2nd dimension degrees of freedom
#' @param p The p-value of the resulting test
#' @param reference A possible reference number for use in a table key
#' @param src An optional field for traceability of the source of the field
#'
#' @return A cell_fstat object.
#' @export
#' @examples
#' cell_fstat(4.0, 10, 20, 0.004039541, 1, "example")
cell_fstat <- function(f, n1, n2, p, reference=NA, src=NA)
{
  list_cell(c("cell_fstat","statistics"), f=f, n1=n1, n2=n2, p=p, reference=reference, src=src)
}

#' Create an cell_fraction (S3) object of the given statistic
#'
#' A cell_fraction object contains a statistical result of a fraction/percentage.
#'
#' @param numerator The value of the numerator
#' @param denominator The value of the denominator
#' @param ratio The ratio of the two
#' @param percentage The percentage this represents
#' @param src An optional field for traceability of the source of the field
#'
#' @return A cell_fraction object.
#' @export
#' @examples
#' cell_fraction(1, 4, 0.25, 25)
cell_fraction <- function(numerator, denominator, ratio, percentage, src=NA)
{
  list_cell("cell_fraction",
            numerator=numerator, denominator=denominator,
            ratio=ratio,         percentage=percentage,
            src=src)
}

#' Create an cell_chi2 (S3) object of the given statistic
#'
#' A cell_chi2 object contains a statistical result of an X^2-test.
#'
#' @param chi2 The value of the X^2 statistic
#' @param df degrees of freedom
#' @param p p-value of resulting test
#' @param reference A possible reference number for use in a table key
#' @param src An optional field for traceability of the source of the field
#'
#' @return A cell_chi2 object.
#' @export
#' @examples
#' cell_chi2(5.6, 2, 0.06081)
cell_chi2 <- function(chi2, df, p, reference=NA, src=NA)
{
  list_cell(c("cell_chi2", "statistics"), chi2=chi2, df=df, p=p, reference=reference, src=src)
}

#' Create an cell_studentt (S3) object of the given statistic
#'
#' A cell_studentt object contains a statistical result of an t-test.
#'
#' @param t The value of the X^2 statistic
#' @param df degrees of freedom
#' @param p p-value of resulting test
#' @param reference A possible reference number for use in a table key
#' @param src An optional field for traceability of the source of the field
#'
#' @return A cell_studentt object.
#' @export
#' @examples
#' cell_studentt(2.0, 20, 0.02963277)
cell_studentt <- function(t, df, p, reference=NA, src=NA)
{
  list_cell(c("cell_studentt", "statistics"), t=t, df=df, p=p, reference=reference, src=src)
}

#' Create an cell_spearman (S3) object of the given statistic
#'
#' A cell_spearman object contains a statistical result of an spearman-test.
#'
#' @param S The value of the spearman statistic
#' @param rho The rho value of the test
#' @param p p-value of resulting test
#' @param reference A possible reference number for use in a table key
#' @param src An optional field for traceability of the source of the field
#'
#' @return A cell_spearman object.
#' @export
#' @examples
#' cell_spearman(20, 0.2, 0.05)
cell_spearman <- function(S, rho, p, reference=NA, src=NA)
{
  list_cell(c("cell_spearman", "statistics"), S=S, rho=rho, p=p, reference=reference, src=src)
}

#' Create an cell_n (S3) object of the given statistic
#'
#' A cell_n object contains a statistical result of an X^2-test.
#'
#' @param n The value of the X^2 statistic
#' @param src An optional field for traceability of the source of the field
#'
#' @return A cell_n object.
#' @export
#' @examples
#' cell_n(20)
cell_n <- function(n, src=NA)
{
  list_cell("cell_n", n=n, src=src)
}


#' Key derivation helper function
#'
#' @param row The AST row node to use in key generation
#' @param col The AST col node to use in key generation
#' @param label Additional label about source of data
#' @param subrow Additional specifier for row
#' @param subcol Additional specifier for column
#' @export
key <- function(row, col, label=NA, subrow=NA, subcol=NA)
{
  rv <- if(is.na(subrow)) row$value else paste(row$value, '[',subrow,']',sep='')
  cv <- if(is.na(subcol)) col$value else paste(col$value, '[',subcol,']',sep='')
  if(is.na(label)) paste(rv,":",cv,sep='') else paste(rv,":",cv,":",label,sep='')
}


