#######
# Given the compiled tree of data, render as a text summary
#' @include S3-Cell.R

summary.default <- function(object) ""

summary.cell_label <- function(object)
{
  if(is.na(object$units))
    object$label
  else
    paste(object$label, " (", object$units, ")", sep="")
}

summary.cell_quantile <- function(object)
{
  paste(object$'25%', " *", object$'50%', "* ", object$'75%', sep="")
}

#' @export
summary.cell_table <- function(object)
{
  nrows <- rows(object)
  ncols <- cols(object)

  text <- matrix(data=rep("", nrows*ncols), nrow=nrows, ncol=ncols)

  last_header_row <- 0 # Current Header Row
  sapply(1:nrows, FUN=function(row) {
    sapply(1:ncols, FUN=function(col) {
      if(last_header_row == 0 && !inherits(object[[row]][[col]], "cell_header")) last_header_row <<- row
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

#' @export
print.cell_table <- function(object) {summary(object)}

summary.cell_estimate <- function(object)
{
  if(is.na(object$low))
    as.character(object$value)
  else
    paste(object$value," (",object$low,",",object$high,")")
}

summary.cell_fstat <- function(object)
{
  paste("F_{",object$n1,",",object$n2,"}=",object$f,", P=",object$p,sep="")
}

summary.cell_fraction <- function(object)
{
  x <- sprintf("%3s",round(100*object$numerator/object$denominator,0))
  den <- as.character(object$denominator)
  num <- sprintf(paste("%",nchar(den),"s",sep=''), object$numerator)
  paste(x, "%  ",
        num,"/",den,
        sep="")
}

summary.cell_chi2 <- function(object)
{
  paste("    X^2_",object$df,"=",object$chi2,", P=",object$p,sep="")
}

summary.cell_studentt <- function(object)
{
  paste("T_",object$df,"=",object$t, ", P=",object$p, sep="")
}

summary.cell_n <- function(object)
{
  if (inherits(object, "cell_header"))
    paste("(N=",as.character(object$n),")",sep='')
  else
    as.character(object$n)
}
