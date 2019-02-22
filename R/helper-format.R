# tangram a general purpose table toolkit for R
# Copyright (C) 2017 Shawn Garbett
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


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
  if(is.null(format) || is.na(format)) format <- 2
  if(!is.character(format) && format < 0) format <- 0
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
  result[is.na(x)] <- NA
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
    0
  else
  {
    consider <- quantile(abs(d), c(0.05, 0.5))
    if(sum(consider) == 0.0) 3 else
      # Otherwise use 3 significant digits of a representative smaller side quantile
      max(2-max(floor(log10(consider))), 0)
  }
}

