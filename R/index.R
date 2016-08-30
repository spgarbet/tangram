#######
# Given the compiled tree of data, render as a text index

#' @export
index <- function(x, ...)
{
  UseMethod("index", x)
}

index.tg_cell <- function(object) NULL

index.tg_subheader <- function(object)
{
  paste("<td class=\"subheader\">",
        gsub("^N=","<em>N</em>=", object$label),
        "</td>",
        sep="")
}


index.tg_estimate <- function(object)
{
  paste(object$value)

  if(is.na(object$low))
    paste("<td class=\"estimate\"><b>",
          object$low,
          "</b></td>",
          sep="")
  else
    paste("<td class=\"estimate\"><b>",
          object$low,
          "</b>",
          " (",object$low,",",object$high,")",
          "</td>",
          sep="")
}

#' @export
index.tg_table <- function(object, css="Hmisc.css", caption="Figure")
{
  nrows <- rows(object)
  ncols <- cols(object)

  # Render it all
  sapply(1:nrows, FUN=function(row) {
    sapply(1:ncols, FUN=function(col) {
      index(object[[row]][[col]])
    })
  })
}
