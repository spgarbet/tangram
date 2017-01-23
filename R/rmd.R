#######
# Given the compiled tree of data, render as a text rmd
#' @include S3-Cell.R

rmd.default <- function(object,...) ""

rmd.cell_label <- function(object,...)
{
  if(is.na(object$units))
  {
    if(length(object$label) == 0) return("") else return(object$label)
  } else {
    return(paste(object$label, " (", object$units, ")", sep=""))
  }
}

rmd.cell_quantile <- function(object,...)
{
  paste(render_f(object$'25%',object$format),
        " **", render_f(object$'50%', object$format), "** ",
        render_f(object$'75%', object$format),
        sep="")
}


rmd.cell_estimate <- function(object,...)
{
  if(is.na(object$low))
    render_f(object$value)
  else
    paste(render_f(object$value)," (",render_f(object$low),", ",render_f(object$high),")", sep='')
}

rmd.cell_fstat <- function(object,...)
{
  paste("F<sub>",object$n1,",",object$n2,"</sub>=",render_f(object$f),", P=",render_f(object$p),sep="")
}

rmd.cell_fraction <- function(object,...)
{
  x <- render_f(object$ratio)
  den <- as.character(object$denominator)
  num <- sprintf(paste("%",nchar(den),"s",sep=''), object$numerator)
  paste(x, "  ",
        num,"/",den,
        sep="")
}

rmd.cell_chi2 <- function(object,...)
{
  paste("&chi;<sup>2</sup><sub>",object$df,"</sub>=",render_f(object$chi2),", P=",render_f(object$p),sep="")
}

rmd.cell_studentt <- function(object,...)
{
  paste("T<sub>",object$df,"</sub>=",render_f(object$t), ", P=",render_f(object$p), sep="")
}

rmd.cell_spearman <- function(object,...)
{
  paste("S=",render_f(object$S),", P=",render_f(object$p), sep="")
}

rmd.cell_n <- function(object,...)
{
  if (inherits(object, "cell_header"))
    paste("(N=",as.character(object$n),")",sep='')
  else
    as.character(object$n)
}

#' @export
#' @importFrom stringr str_pad
rmd.cell_table <- function(object,...)
{
  nrows <- rows(object)
  ncols <- cols(object)

  text <- matrix(data=rep("", nrows*ncols), nrow=nrows, ncol=ncols)

  last_header_row <- 0 # Current Header Row
  sapply(1:nrows, FUN=function(row) {
    sapply(1:ncols, FUN=function(col) {
      if(last_header_row == 0 && !inherits(object[[row]][[col]], "cell_header")) last_header_row <<- row - 1
      text[row,col] <<- rmd(object[[row]][[col]])
    })
  })

  # Pad strings
  sapply(1:ncols, FUN=function(col) {
    if(is.na(text[1,col]))
    {
      text[1,col] <<- "     "
    } else if(nchar(text[1,col]) < 5)
    {
      text[1,col] <<- str_pad(text[1,col], width=5, side="right");
    }
  })

  pasty <- apply(text, 1, function(x) paste(c("|", paste(x, collapse="|"), "|"), collapse=""))

  cat(pasty[1], '\n')
  cat(gsub("-\\|", ":|", gsub("[^\\|]", "-", pasty[1])), '\n')

  for(row in pasty[2:nrows]) cat(row, '\n')

}

#' @export
rmd <- function(object, ...)
{
  UseMethod("rmd", object)
}

