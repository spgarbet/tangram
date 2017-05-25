  #############################################################################
 ##
## Set of formatting helper functions

#' Format a vector of provided numeric values
#'
#' Given a vector of data return as strings formatted as requested
#'
#' @param x numeric; the data to format. Must work with quantile function.
#' @param format numeric or character; If numeric preserve that many position past the decimal, if character pass directly into sprintf as format string
#' @return character; formatted values as character strings
#' @export
#' @examples
#' render_f(rnorm(5), 3)
#' render_f(round(rnorm(5), 2), "%010.03f")
render_f <- function(x, format)
{
  if(is.null(format) || is.na(format)) format <- attr(x, "format")
  if(is.null(format) || is.na(format)) format <- 3
  if(is.character(format) && substr(format, 1, 1) != "%") format <- as.numeric(format)

  result <- if(is.numeric(format))
  {
    sprintf(paste("%1.", format, "f", sep=""), x)
  }
  else
  {
    sprintf(format, x)
  }
  names(result) <- names(x)
  result
}


#' Guess the best format for a given set of numerical data
#'
#' Given a vector of data, default to 3 significant digits or all if maximum is greater
#' than zero
#'
#' @param x numeric; basic math and quantile function must work on data passed in
#' @return numeric; the digits past the decimal recommended for display
#' @export
#' @examples
#' format_guess(rnorm(100))
#' format_guess(rnorm(100, sd=1e-6))
format_guess <- function(x)
{
  d <- x[!is.na(x)]
  if(length(d) == 0) return(0) # Nothing, then just return 0 for rounding
  if(all(d == floor(d)))       # Is it all whole numbers, then no decimals
    return(0)
  else
    # Otherwise use 3 significant digits of a representative smaller side quantile
    return(max(2-max(floor(log10(quantile(abs(d), c(0.05, 0.5))))), 0))
}
