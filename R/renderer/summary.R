#######
# Given the compiled tree of data, render as a text summary

summary.tg_cell <- function(object) ""

summary.tg_label <- function(object) 
{
  if(is.na(object$units))
    object$label
  else
    paste(object$label, " (", object$units, ")", sep="")
}

summary.tg_quantile <- function(object)
{
  paste(sigfig(object$q25), " *", sigfig(object$q50), "* ", sigfig(object$q75), sep="")
}

summary.tg_table <- function(object)
{
  nrows <- rows(object)
  ncols <- cols(object)
  
  text <- matrix(data=rep("", nrows*ncols), nrow=nrows, ncol=ncols)
  
  sapply(1:nrows, FUN=function(row) {
    sapply(1:ncols, FUN=function(col) {
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
    if(row == pasty[2]) cat(paste(rep("-",nchar(pasty[1])),collapse=''),'\n') # FIXME: This is hardcoded at 2!!!!
  }
  cat(paste(rep("=",nchar(pasty[1])),collapse=''),'\n')
  
}

summary.tg_estimate <- function(object)
{
  if(is.na(object$low))
    as.character(object$value)
  else
    paste(object$value, " (",object$low,",",object$high,")")
}

summary.tg_fstat <- function(object)
{
  paste("F_{",object$n1,",",object$n2,"}=",roundfig(object$f,2),", P=",roundfig(object$p,3),sep="")
}

summary.tg_fraction <- function(object)
{
  x <- sprintf("%3s",round(100*object$numerator/object$denominator,0))
  den <- as.character(object$denominator)
  num <- sprintf(paste("%",nchar(den),"s",sep=''), object$numerator)
  paste(x, "%  ",
        num,"/",den,
        sep="")
}

summary.tg_chi2 <- function(object)
{
  paste("    X^2_",object$df,"=",roundfig(object$chi2,2),", P=",roundfig(object$p,3),sep="")
}