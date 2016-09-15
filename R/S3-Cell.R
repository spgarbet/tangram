rows <- function(x)
{
  UseMethod("rows", x)
}

cols <- function(x)
{
  UseMethod("cols", x)
}

cell_format <- function(format, x)
{
  if(is.na(format)) format <- 3
  if(is.character(format) && substr(format, 1, 1) != "%") format <- as.numeric(format)

  result <- if(is.numeric(format))
  {
    withCallingHandlers(formatC(signif(as.numeric(x), digits=format), digits=format,format="fg", flag="#"),
      warning = function(w) {}
    )
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

#' @export
cell_table <- function(rows, cols, embedded=TRUE)
{
  nt <- sapply(1:rows, FUN=function(x) list(sapply(1:cols, FUN=function(x) cell())))

  tbl <- structure(cell(nt), class=c("cell_table", "cell") )

  attr(tbl, "embedded") <- embedded

  tbl
}

cell_label <- function(text, units=NA, src=NA)
{
  if(!inherits(text, "character")) text <- as.character(text)
  structure(cell(list(label=as.character(text), units=as.character(units), src=src)), class = c("cell_label", "cell"))
}

cell_header <- function(text, units=NA, src=NA)
{
  structure(cell(list(label=as.character(text), units=as.character(units), src=src)), class = c("cell_header", "cell_label", "cell"))
}

cell_subheader <- function(text, units=NA, src=NA)
{
  structure(cell(list(label=as.character(text), units=as.character(units), src=src)), class = c("cell_subheader", "cell_header", "cell_label", "cell"))
}

cell_quantile <- function(quantiles, src=NA)
{
  ql <- as.list(quantiles)
  if(!is.na(src)) ql[['src']] <- src
  structure(cell(ql), class=c("cell_quantile", "cell"))
}

cell_estimate <- function(value, low=NA, high=NA, conf.level=0.95, src=NA)
{
  structure(cell(list(value=value, low=low, high=high, src=src)), class=c("cell_estimate", "cell"))
}

cell_fstat <- function(f, n1, n2, p, src=NA)
{
  structure(cell(list(f=f, n1=n1, n2=n2, p=p, src=src)), class=c("cell_fstat","cell"))
}

cell_fraction <- function(numerator, denominator, src=NA)
{
  structure(cell(list(numerator=numerator, denominator=denominator, src=src)), class=c("cell_fraction","cell"))
}

cell_chi2 <- function(chi2, df, p, src=NA)
{
  structure(cell(list(chi2=chi2, df=df, p=p, src=src)), class=c("cell_chi2", "cell"))
}

cell_studentt <- function(t, df, p, src=NA)
{
  structure(cell(list(t=t, df=df, p=p, src=src)), class=c("cell_studentt", "cell"))
}

cell_n <- function(n, src=NA)
{
  structure(cell(list(n=n, src=src)), class=c("cell_n", "cell"))
}


#' Key derivation helper function
#' @export
key <- function(row, col, label=NA, subrow=NA, subcol=NA)
{
  rv <- if(is.na(subrow)) row$value else paste(row$value, '[',subrow,']',sep='')
  cv <- if(is.na(subcol)) col$value else paste(col$value, '[',subcol,']',sep='')
  if(is.na(label)) paste(rv,":",cv,sep='') else paste(rv,":",cv,":",label,sep='')
}


