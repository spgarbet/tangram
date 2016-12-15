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
    #withCallingHandlers(formatC(round(as.numeric(x), digits=format), digits=format,format="fg", flag="#"),
    #  warning = function(w) {}
    #)
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

list_cell <- function(classes, ...)
{
  structure(list(...), class=c(classes, "cell"))
}

cell_label <- function(text, units=NA, src=NA)
{
  if(!inherits(text, "character")) text <- as.character(text)
  structure(cell(list(label=as.character(text), units=as.character(units), src=src)), class = c("cell_label", "cell"))
}

cell_header <- function(text, units=NA, src=NA)
{
  list_cell(c("cell_header", "cell_label"),
            label=as.character(text),
            units=as.character(units),
            src=src)
}

cell_subheader <- function(text, units=NA, src=NA)
{
  list_cell(c("cell_subheader", "cell_header", "cell_label"),
            label=as.character(text),
            units=as.character(units),
            src=src)
}

cell_quantile <- function(quantiles, src=NA)
{
  format <- attr(quantiles, "format")
  ql <- lapply(as.list(quantiles), function(x) form(x, format))
  if(!is.na(src)) ql[['src']] <- src
  structure(cell(ql), class=c("cell_quantile", "cell"))
}

cell_estimate <- function(value, low=NA, high=NA, conf.level=0.95, src=NA)
{
  list_cell("cell_estimate", value=value, low=low, high=high, src=src)
}

cell_fstat <- function(f, n1, n2, p, src=NA)
{
  list_cell(c("cell_fstat","statistics"), f=f, n1=n1, n2=n2, p=p, src=src)
}

cell_fraction <- function(numerator, denominator, ratio, percentage, src=NA)
{
  list_cell("cell_fraction",
            numerator=numerator, denominator=denominator,
            ratio=ratio,         percentage=percentage,
            src=src)
}

cell_chi2 <- function(chi2, df, p, src=NA)
{
  list_cell(c("cell_chi2", "statistics"), chi2=chi2, df=df, p=p, src=src)
}

cell_studentt <- function(t, df, p, src=NA)
{
  list_cell(c("cell_studentt", "statistics"), t=t, df=df, p=p, src=src)
}

cell_spearman <- function(S, rho, p, src=NA)
{
  list_cell(c("cell_spearman", "statistics"), S=S, rho=rho, p=p, src=src)
}

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


