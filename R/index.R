#######
# Given the compiled tree of data, render as a text index

#' @export
index <- function(object, ...)
{
  UseMethod("index", object)
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

