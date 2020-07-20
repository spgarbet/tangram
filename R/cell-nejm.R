# tangram a general purpose table toolkit for R
# Copyright (C) 2017-2018 Shawn Garbett
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


#' Create a NEJM style range
#'
#' Construct a cell which has the range of the given data in NEJM style
#'
#' @param x numeric vector whose range is desired
#' @param format numeric or character; an argument to pass to the formatting function
#' @param ... additional arguments to passed to cell()
nejm_range <- function(x, format, ...)
{
  if(is.na(format) || is.null(format)) format <- format_guess(x)

  if(sum(!is.na(x)) == 0)             return(cell("\u2014", ...))
  if(is.infinite(min(x, na.rm=TRUE))) return(cell("\u2014", ...))

  cell(paste0(render_f(min(x,na.rm=TRUE),format), "\u2014", render_f(max(x, na.rm=TRUE), format)), ...)
}


#' Create an cell_fraction (S3) in NEJM style of the given data
#'
#' A cell object contains a statistical result of a fraction/percentage in nejm style
#'
#' @param numerator numeric; The value of the numerator
#' @param denominator numeric; The value of the denominator
#' @param format numeric or character; a string formatting directive
#' @param ... optional extra information to attach
#' @return A cell_fraction object.
#' @export
#' @examples
#' nejm_fraction(1, 4, 3)
nejm_fraction <- function(numerator, denominator, format=NULL, ...)
{
  #format <- if(inherits(format,"numeric")) format - 2 else format

  percent <- 100*numerator/denominator

  if(is.na(format) || is.null(format)) format <- format_guess(percent)

  percent      <- render_f(percent, format)

  cell(paste0(str_pad(numerator, nchar(as.character(denominator))),
              "/",
              denominator,
              " (",
              percent,
              ")"), ...)

}

#' Create a interquartile range cell object of the given data NEJM style
#'
#' Construct a cell which has the 3 interquartile ranges specified.
#'
#' @param x numeric vector whose sample quantiles are wanted. NA and NaN values are not allowed in numeric vectors unless na.rm is TRUE.
#' @param format numeric or character; Significant digits or fmt to pass to sprintf
#' @param na.rm logical; if true, any NA and NaN's are removed from x before the quantiles are computed.
#' @param names logical; if true, the result has a names attribute. Set to FALSE for speedup with many probs.
#' @param type integer; specify algorithm to use in constructing quantile. See quantile for more information.
#' @param msd logical; compute an msd attribute containing mean and standard deviation
#' @param quant numeric; The quantiles to display. Should be an odd length vector, since the center value is highlighted.
#' @param ... additional arguments to constructing cell
#'
#' @return A cell_quantile object.
#' @export
#' @importFrom stats quantile
#' @importFrom stats sd
#' @examples
#' require(stats)
#' nejm_iqr(rnorm(100), '3')
nejm_iqr <- function(x,
                    format = NA,
                    na.rm  = TRUE,
                    names  = FALSE,
                    type   = 8,
                    msd    = FALSE,
                    quant  = c(0.25, 0.50, 0.75),
                    ...
                    )
{
  if(!is.null(quant) && length(quant) %% 2 == 0)
    stop("hmisc_iqr quant argument must be an odd length")

  if(is.null(quant)) ql <- "" else
  {
    m <- median(1:length(quant))
    y <- quantile(x, quant, na.rm, names, type)

    if(is.na(format)) format <- format_guess(y)
    ql <- "\u2014"
    if(sum(!is.na(y)) > 0)
    {
      ql <- sapply(y, function(x) render_f(x, format))
      ql <- paste0(ql[m], " (",
                          paste0(ql[1:(m-1)], collapse="\u2014"),
                          "\u2014",
                          paste0(ql[(m+1):length(quant)], collapse="\u2014"),
                          ")")
    }
  }

  if(msd) ql <- paste0(ql, "\u00A0\u00A0",
      render_f(mean(x, na.rm=TRUE), format),
      "\u00b1",
      render_f(sd(x, na.rm=TRUE), format)
      )

  cell(ql, ...)
}

#' Cell Generation functions for nejm default
#'
#' Each function here is called when a cell is generated. Overriding these in a formula call will allows
#' one to customize exactly how each cell's contents are generated.
#'
#' While this serves as the base template for transforms, it is by no means required if one develops their
#' own bundle of data transforms. One can create ay number of cell level styling choices.
#'
#' @include cell-hmisc.R
#' @keywords data
#' @export
nejm_cell <- list(
  n        = cell_n,
  range    = nejm_range,
  iqr      = nejm_iqr,
  fraction = nejm_fraction,
  fstat    = hmisc_fstat,
  chi2     = hmisc_chi2,
  spearman = hmisc_spearman,
  wilcox   = hmisc_wilcox,
  p        = hmisc_p
)
