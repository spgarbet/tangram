#######
# Given the compiled tree of data, render as a text index

#' @export
index <- function(object, ...)
{
  UseMethod("index", object)
}

index_content <- function(object,caption,value)
{
  if(!("src" %in% names(object))) return(NULL)
  if(is.na(object$src)) return(NULL)
  src <- paste(caption, object$src, sep=":")
  idx <- substr(base64encode(charToRaw(digest(src))), 1, 4)

  result <- c(idx, src, value)
  names(result) <- c("key", "src", "value")
  result
}

#' @importFrom base64enc base64encode
#' @importFrom digest digest
#' @export
index.default <- function(object,caption, ...)
{
  if(!("src" %in% names(object))) return(NULL)
  if(is.na(object$src)) return(NULL)
  src <- paste(caption, object$src, sep=":")
  nms <- names(object)
  lapply(nms[!nms %in% c('label','src','units')],
         function(y)
         {
           idx <- substr(base64encode(charToRaw(digest(c(src,y)))), 1, 4)

           c(idx, paste(src, y, sep=':'), as.character(object[[y]]))
           #paste(idx, paste(src, y, sep=':'), object[[y]], sep=",")
         })
}

#' @export
index.cell_n <- function(object, caption, ...)
{
  index_content(object, caption, object$n)
}

#' @export
index.cell_estimate <- function(object, caption, ...)
{
  content <- if(is.na(object$low))
    as.character(object$value)
  else
    paste(object$value,
          " (",object$low,", ",object$high,")",
          sep="")

  index_content(object, caption, content)
}

#' @export
index.cell_quantile <- function(object, caption, ...)
{
  content <-
    paste(object$'50%',
          " [",object$'25%',", ",object$'75%',"]",
          sep="")

  index_content(object, caption, content)
}

#' @export
index.cell_fstat <- function(object, caption, ...)
{
  content <-
    paste("F=", object$f,", p = ", object$p, sep='')

  index_content(object, caption, content)
}

#' @export
index.cell_fraction <- function(object, caption, ...)
{
  content <-
    paste(round(100*object$numerator/object$denominator, 2),
          "% ",
          object$numerator,
          "/",
          object$denominator,
          sep='')

  index_content(object, caption, content)
}

#' @export
index.cell_chi2 <- function(object, caption, ...)
{
  content <-
    paste("chisq=",
          object$chi2,
          ", p=",
          object$p,
          sep='')

  index_content(object, caption, content)
}

# FIXME: OPEN QUESTION: should caption be part of the "key"?

#' @export
index.cell_table <- function(object, caption="Table",...)
{
  nrows <- rows(object)
  ncols <- cols(object)

  # Render it all
  result<-
  unlist(sapply(1:nrows, simplify=FALSE, FUN=function(row) {
    unlist(sapply(1:ncols, simplify=FALSE, FUN=function(col) {
      c(index(object[[row]][[col]], caption))
    }))
  }))

  names(result) <- NULL
  result <- matrix(result, ncol=3, byrow=TRUE)
  colnames(result) <- c("key", "src", "value")
  result
}

