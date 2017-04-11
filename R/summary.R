#######
# Given the compiled tree of data, render as a text summary
#' @include S3-Cell.R

summary.default <- function(object,...) ""

summary.cell_label <- function(object,...)
{
  if(is.na(object$units))
  {
    if(length(object$label) == 0) return("") else return(object$label)
  } else {
    return(paste(object$label, " (", object$units, ")", sep=""))
  }
}

summary.cell_quantile <- function(object,...)
{
  paste(render_f(object$'25%',object$format),
        " *", render_f(object$'50%', object$format), "* ",
        render_f(object$'75%', object$format),
        sep="")
}

#' Create a text summary of a given table
#'
#' @param object The cell table to render to text
#' @param ... additional arguments to renderer. Unused at present.
#' @return A text string rendering of the given table
#' @export
#' @importFrom stringr str_pad
#'
summary.cell_table <- function(object,...)
{
  nrows <- rows(object)
  ncols <- cols(object)

  text <- matrix(data=rep("", nrows*ncols), nrow=nrows, ncol=ncols)

  last_header_row <- 0 # Current Header Row
  sapply(1:nrows, FUN=function(row) {
    sapply(1:ncols, FUN=function(col) {
      if(last_header_row == 0 && !inherits(object[[row]][[col]], "cell_header")) last_header_row <<- row - 1
      text[row,col] <<- summary(object[[row]][[col]])
    })
  })

  maxwidths <- apply(text, 2, FUN=function(x) max(nchar(x), na.rm=TRUE))

  sapply(1:nrows, FUN=function(row) {
    sapply(1:ncols, FUN=function(col) {
      if(col == 1)
      {
        text[row,col] <<- str_pad(text[row,col], maxwidths[col], "right")
      }
      else
      {
        text[row,col] <<- str_pad(text[row,col], maxwidths[col], "both")
      }
    })
  })

  pasty <- apply(text, 1, function(x) paste(x, collapse="  "))

  cat(paste(rep("=",nchar(pasty[1])),collapse=''),'\n')
  for(row in pasty)
  {
    cat(row,'\n')
    if(last_header_row > 0              &&
       length(pasty) >= last_header_row &&
       row == pasty[last_header_row])
    {
      cat(paste(rep("-",nchar(pasty[1])),collapse=''),'\n')
    }
  }
  cat(paste(rep("=",nchar(pasty[1])),collapse=''),'\n')

}

#' Print a text summary of a given table
#'
#' @param x The cell table to render to text
#' @param ... additional arguments, unused at present
#' @return A text string rendering of the given table
#' @export
#' @importFrom stringr str_pad
#'
print.cell_table <- function(x,...) {summary(x,...)}

summary.cell_estimate <- function(object,...)
{
  if(is.na(object$low))
    render_f(object$value)
  else
    paste(render_f(object$value)," (",render_f(object$low),", ",render_f(object$high),")", sep='')
}

summary.cell_fstat <- function(object,...)
{
  paste("F_{",object$n1,",",object$n2,"}=",render_f(object$f),", P=",render_f(object$p),sep="")
}

summary.cell_fraction <- function(object,...)
{
  x <- render_f(object$ratio)
  den <- as.character(object$denominator)
  num <- sprintf(paste("%",nchar(den),"s",sep=''), object$numerator)
  paste(x, "  ",
        num,"/",den,
        sep="")
}

summary.cell_chi2 <- function(object,...)
{
  paste("    X^2_",object$df,"=",render_f(object$chi2),", P=",render_f(object$p),sep="")
}

summary.cell_studentt <- function(object,...)
{
  paste("T_",object$df,"=",render_f(object$t), ", P=",render_f(object$p), sep="")
}

summary.cell_spearman <- function(object,...)
{
  paste("S=",render_f(object$S),", P=",render_f(object$p), sep="")
}

summary.cell_n <- function(object,...)
{
  if (inherits(object, "cell_header"))
    paste("(N=",as.character(object$n),")",sep='')
  else
    as.character(object$n)
}
