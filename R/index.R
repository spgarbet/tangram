#######
# Given the compiled tree of data, render as a text index

#' @export
index <- function(x, ...)
{
  UseMethod("index", x)
}

index.tg_cell <- function(object,caption) NULL

index.tg_label <- function(object, caption)
{
  if(is.na(object$src)) return(NULL)

  paste(paste(caption, object$src, sep=":"), object$label, sep=",")
}

index.tg_fstat <- function(object, caption)
{
  src <- paste(caption, object$src, sep=":")
  c(
    paste(paste(src, "F",  sep=':'), object$f,  sep=","),
    paste(paste(src, "n1", sep=':'), object$n1, sep=","),
    paste(paste(src, "n2", sep=':'), object$n2, sep=","),
    paste(paste(src, "p",  sep=':'), object$p,  sep=",")
  )
}

#' @export
index.tg_table <- function(object, caption="Figure")
{
  nrows <- rows(object)
  ncols <- cols(object)

  # Render it all
  unlist(sapply(1:nrows, simplify=FALSE, FUN=function(row) {
    unlist(sapply(1:ncols, simplify=FALSE, FUN=function(col) {
      c(index(object[[row]][[col]], caption))
    }))
  }))
}

